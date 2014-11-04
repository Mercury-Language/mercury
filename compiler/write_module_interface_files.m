%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: write_module_interface_files.m.
% Main author: fjh (when this code was in modules.m).
%
% This module writes the automatically generated .int0, .int3, .int2 and .int
% files for each Mercury source module.
%
% The interface file system works as follows:
%
% 1. a .int3 file is written, which contains all the types, typeclasses, insts
% and modes defined in the interface. Equivalence types, solver types, insts
% and modes are written in full, others are written in abstract form. These
% are module qualified as far as possible given the information present in the
% current module. The datestamp on the .date3 file gives the last time the
% .int3 file was checked for consistency.
%
% 2. The .int and .int2 files are created, using the .int3 files
% of imported modules to fully module qualify all items.
% The .int2 file is mostly just a fully qualified version of the .int3 file,
% however it also includes some extra information, such as functors for
% discriminated union types, which may be needed for mode analysis.
% The .int3 file must be kept for datestamping purposes. The datestamp
% on the .date file gives the last time the .int and .int2 files
% were checked.
%
% 3. The .int0 file is similar to the .int file except that it also
% includes declarations (but not clauses) from the implementation section.
% It is used when compiling sub-modules. The datestamp on the .date0
% file gives the last time the .int0 file was checked.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.write_module_interface_files.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % write_private_interface_file(Globals, SourceFileName,
    %   SourceFileModuleName, ModuleName, MaybeTimestamp, Items, !IO):
    %
    % Given a source file name and module name, the timestamp of the source
    % file, and the list of items in that module, output the private (`.int0')
    % interface file for the module. (The private interface contains all the
    % declarations in the module, including those in the `implementation'
    % section; it is used when compiling sub-modules.)
    %
    % XXX The comment on the predicate definition used to read:
    % Read in the .int3 files that the current module depends on, and use
    % these to qualify all the declarations as much as possible. Then write
    % out the .int0 file.
    %
:- pred write_private_interface_file(globals::in, file_name::in,
    module_name::in, module_name::in, maybe(timestamp)::in, list(item)::in,
    io::di, io::uo) is det.

    % write_interface_file(Globals, SourceFileName,
    %   SourceFileModuleName, ModuleName, MaybeTimestamp, Items, !IO):
    %
    % Given a source file name and module name, the timestamp of the source
    % file, and the list of items in that module, output the long (`.int')
    % and short (`.int2') interface files for the module.
    %
    % XXX The comment on the predicate definition used to read:
    % Read in the .int3 files that the current module depends on, and use these
    % to qualify all items in the interface as much as possible. Then write out
    % the .int and .int2 files.
    %
:- pred write_interface_file(globals::in, file_name::in,
    module_name::in, module_name::in, maybe(timestamp)::in, list(item)::in,
    io::di, io::uo) is det.

    % Output the unqualified short interface file to <module>.int3.
    %
:- pred write_short_interface_file(globals::in, file_name::in,
    module_name::in, list(item)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Remove all the imported items the list.
    % This internal utility predicate is exported for make.module_dep_file.m.
    %
:- pred strip_imported_items(list(item)::in, list(item)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.item_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.modules.            % undesirable dependency
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_error.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.read_modules.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% Write out .int0 files.
%

write_private_interface_file(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, MaybeTimestamp, Items0, !IO) :-
    grab_unqual_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, Items0, Module, !IO),

    % Check whether we succeeded.
    module_and_imports_get_results(Module, Items1, Specs0, Errors),
    ( if set.is_non_empty(Errors) then
        module_name_to_file_name(Globals, ModuleName, ".int0",
            do_not_create_dirs, FileName, !IO),
        % XXX _NumErrors
        write_error_specs(Specs0, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        io.write_strings(["Error reading interface files.\n",
            "`", FileName, "' not written.\n"], !IO)
    else
        % Module-qualify all items.
        module_name_to_file_name(Globals, ModuleName, ".m",
            do_not_create_dirs, FileName, !IO),
        module_qualify_items(Items1, Items2, map.init, _, Globals, ModuleName,
            yes(FileName), "", _, _, _, Specs0, Specs),
        (
            Specs = [_ | _],
            % XXX _NumErrors
            write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            io.write_strings(["`", FileName, "' not written.\n"], !IO)
        ;
            Specs = [],

            % Write out the `.int0' file.

            strip_imported_items(Items2, Items3),
            process_items_for_private_interface(ModuleName, Items3,
                section_interface, [], RevIntItems0, [], RevImplItems0),
            list.reverse(RevIntItems0, IntItems0),
            list.reverse(RevImplItems0, ImplItems0),
            order_items(IntItems0, IntItems),
            order_items(ImplItems0, ImplItems),
            Items4 = [make_pseudo_decl(md_interface) | IntItems],
            (
                ImplItems = [],
                Items = Items4
            ;
                ImplItems = [_ | _],
                Items = Items4 ++
                    [make_pseudo_decl(md_implementation) | ImplItems]
            ),
            actually_write_interface_file(Globals, SourceFileName, ModuleName,
                ".int0", MaybeTimestamp, Items, !IO),
            touch_interface_datestamp(Globals, ModuleName, ".date0", !IO)
        )
    ).

    % process_items_for_private_interface processes each item in the item
    % list of a module, as part of the process of creating .int0 files.
    %
    % The `.int0' file contains items which are available to any module in the
    % interface section, and items which are only available to sub-modules in
    % the implementation section. The term "private interface" is ambiguous:
    % sometimes it refers to the `.int0' file which, as just explained,
    % contains the public interface as well. The term "private interface
    % proper" may be used to refer to the information in the implementation
    % section of the `.int0' file.
    %
    % (Historically, the `.int0' file did not distinguish between the public
    % and private interfaces.)
    %
    % This predicate has several jobs.
    %
    % - It removes items that do not belong in the private interface,
    %   in either sense. This includes clauses, pragmas that function as
    %   clauses, and initialise and finalise declarations, since they are
    %   effectively also represent code
    %
    % - It expands any mutable declarations into the pred and mode declarations
    %   for their access predicates, since only these components of a
    %   mutable declaration should be written to a private interface file.
    %
    % - It makes any instance declarations abstract.
    %
    % - It removes the items that divide sections from each other, and then
    %   collects the remaining items in two lists, containing the items
    %   that appear in the interface section and in the implementation section
    %   respectively.
    %
    % The original input list must be in the original order, since this
    % predicate assumes that order in its tracking of the current section.
    % However, the two lists it returns are in the reverse of the original
    % order, and the naming scheme of the variables here reflects that fact.
    %
:- pred process_items_for_private_interface(module_name::in, list(item)::in,
    section::in,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out) is det.

process_items_for_private_interface(_ModuleName, [], _Section,
        !RevIntItems, !RevImplItems).
process_items_for_private_interface(ModuleName, [Item | Items], !.Section,
        !RevIntItems, !RevImplItems) :-
    process_item_for_private_interface(ModuleName, Item, !Section,
        !RevIntItems, !RevImplItems),
    process_items_for_private_interface(ModuleName, Items, !.Section,
        !RevIntItems, !RevImplItems).

:- pred process_item_for_private_interface(module_name::in, item::in,
    section::in, section::out,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out) is det.

process_item_for_private_interface(ModuleName, Item, !Section,
        !RevIntItems, !RevImplItems) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ModuleDefn = md_interface,
            !:Section = section_interface
        ;
            ModuleDefn = md_implementation,
            !:Section = section_implementation
        ;
            ModuleDefn = md_import(_),
            % Only imports listed in the implementation section will be
            % directly imported by sub-modules. Import declarations in the
            % interface section must be duplicated into the implementation
            % section of the `.int0' file.
            (
                !.Section = section_interface,
                !:RevIntItems = [Item | !.RevIntItems],
                !:RevImplItems = [Item | !.RevImplItems]
            ;
                !.Section = section_implementation,
                !:RevImplItems = [Item | !.RevImplItems]
            )
        )
    ->
        true
    ;
        (
            Item = item_clause(_)
        ;
            Item = item_pragma(ItemPragma),
            ItemPragma = item_pragma_info(_, Pragma, _, _),
            pragma_allowed_in_interface(Pragma) = no
        ;
            Item = item_initialise(_)
        ;
            Item = item_finalise(_)
        )
     ->
        true
    ;
        Item = item_mutable(ItemMutable)
    ->
        ItemMutable = item_mutable_info(MutableName, Type, _Value, Inst, Attrs,
            _Varset, Context, _SeqNum),
        ConstantInterface = mutable_var_constant(Attrs),
        (
            ConstantInterface = yes,
            ConstantGetPredDeclItem = constant_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ConstantSetPredDeclItem = constant_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ReplacementItems =
                [ConstantGetPredDeclItem, ConstantSetPredDeclItem]
        ;
            ConstantInterface = no,
            StdGetPredDeclItem = std_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdSetPredDeclItem = std_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            IOStateInterface = mutable_var_attach_to_io_state(Attrs),
            (
                IOStateInterface = yes,
                PureGetPredDeclItem = io_get_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                PureSetPredDeclItem = io_set_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                ReplacementItems =
                    [StdGetPredDeclItem, StdSetPredDeclItem,
                    PureGetPredDeclItem, PureSetPredDeclItem]
            ;
                IOStateInterface = no,
                ReplacementItems =
                    [StdGetPredDeclItem, StdSetPredDeclItem]
            )
        ),
        (
            !.Section = section_interface,
            !:RevIntItems = ReplacementItems ++ !.RevIntItems
        ;
            !.Section = section_implementation,
            !:RevImplItems = ReplacementItems ++ !.RevImplItems
        )
    ;
        ( Item = item_instance(InstanceInfo0) ->
            InstanceInfo = make_instance_abstract(InstanceInfo0),
            InsertedItem = item_instance(InstanceInfo)
        ;
            InsertedItem = Item
        ),
        (
            !.Section = section_interface,
            !:RevIntItems = [InsertedItem | !.RevIntItems]
        ;
            !.Section = section_implementation,
            !:RevImplItems = [InsertedItem | !.RevImplItems]
        )
    ).

%-----------------------------------------------------------------------------%
%
% Write out .int and .int2 files.
%

write_interface_file(Globals, SourceFileName, SourceFileModuleName, ModuleName,
        MaybeTimestamp, Items0, !IO) :-
    some [!InterfaceItems] (
        get_interface(ModuleName, yes, Items0, !:InterfaceItems),

        % Get the .int3 files for imported modules.
        grab_unqual_imported_modules(Globals, SourceFileName,
            SourceFileModuleName, ModuleName, !.InterfaceItems, Module0, !IO),

        % Check whether we succeeded.
        module_and_imports_get_results(Module0, !:InterfaceItems,
            Specs0, Errors),
        ( if set.is_non_empty(Errors) then
            % XXX _NumErrors
            write_error_specs(Specs0, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            module_name_to_file_name(Globals, ModuleName, ".int",
                do_not_create_dirs, IntFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".int2",
                do_not_create_dirs, Int2FileName, !IO),
            io.write_strings(["Error reading short interface files.\n",
                "`", IntFileName, "' and ",
                "`", Int2FileName, "' not written.\n"], !IO)
        else
            % Module-qualify all items.
            module_name_to_file_name(Globals, ModuleName, ".m",
                do_not_create_dirs, FileName, !IO),
            module_qualify_items(!InterfaceItems, map.init, _, Globals,
                ModuleName, yes(FileName), "", _, _, _, Specs0, Specs),

            % We want to finish writing the interface file (and keep
            % the exit status at zero) if we found some warnings.
            globals.set_option(halt_at_warn, bool(no),
                Globals, NoHaltAtWarnGlobals),
            write_error_specs(Specs, NoHaltAtWarnGlobals,
                0, _NumWarnings, 0, NumErrors, !IO),
            ( if NumErrors > 0 then
                module_name_to_file_name(Globals, ModuleName, ".int",
                    do_not_create_dirs, IntFileName, !IO),
                io.write_strings(["`", IntFileName, "' ", "not written.\n"],
                    !IO)
            else
                % Strip out the imported interfaces, assertions are also
                % stripped since they should only be written to .opt files,
                % check for some warnings, and then write out the `.int'
                % and `int2' files and touch the `.date' file.

                strip_imported_items_and_assertions(!InterfaceItems),
                strip_unnecessary_impl_defns(!InterfaceItems),
                report_and_strip_clauses_in_interface(!InterfaceItems, [],
                    InterfaceSpecs0),
                check_int_for_no_exports(Globals, !.InterfaceItems, ModuleName,
                    InterfaceSpecs0, InterfaceSpecs, !IO),
                write_error_specs(InterfaceSpecs, Globals,
                    0, _NumWarnings2, 0, _NumErrors2, !IO),
                % XXX _NumErrors
                order_items(!InterfaceItems),
                actually_write_interface_file(Globals, SourceFileName,
                    ModuleName, ".int", MaybeTimestamp, !.InterfaceItems, !IO),
                get_short_interface(!.InterfaceItems, int2,
                    ShortInterfaceItems),
                actually_write_interface_file(Globals, SourceFileName,
                    ModuleName, ".int2", MaybeTimestamp, ShortInterfaceItems,
                    !IO),
                touch_interface_datestamp(Globals, ModuleName, ".date", !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Write out .int3 files.
%

write_short_interface_file(Globals, SourceFileName, ModuleName, Items0, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.

    some [!Specs] (
        !:Specs = [],
        get_interface(ModuleName, no, Items0, InterfaceItems0),
        % Assertions are also stripped since they should only be written
        % to .opt files.
        strip_assertions(InterfaceItems0, InterfaceItems1),
        report_and_strip_clauses_in_interface(InterfaceItems1, InterfaceItems,
            !Specs),
        get_short_interface(InterfaceItems, int3, ShortInterfaceItems0),
        module_qualify_items(ShortInterfaceItems0, ShortInterfaceItems,
            map.init, _, Globals, ModuleName, no, "", _, _, _, !Specs),
        % XXX _NumErrors
        write_error_specs(!.Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        % XXX why do we do this even if there are some errors?
        actually_write_interface_file(Globals, SourceFileName,
            ModuleName, ".int3", no, ShortInterfaceItems, !IO),
        touch_interface_datestamp(Globals, ModuleName, ".date3", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred strip_imported_items_and_assertions(list(item)::in,
    list(item)::out) is det.

strip_imported_items_and_assertions(Items0, Items) :-
    % The imported items loop has to process the items in their original order,
    % but the loop that removes assertions does not care about the order.
    strip_imported_items_loop(Items0, [], RevItems),
    strip_assertions_loop(RevItems, [], Items).

strip_imported_items(Items0, Items) :-
    strip_imported_items_loop(Items0, [], RevItems),
    list.reverse(RevItems, Items).

:- pred strip_imported_items_loop(list(item)::in,
    list(item)::in, list(item)::out) is det.

strip_imported_items_loop([], !RevItems).
strip_imported_items_loop([Item | Items], !RevItems) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            )
            % The lack of a recursive call here effectively deletes both
            % Item and everything in Items from the list.
        ;
            % XXX Some of these should probably cause an error message.
            ( ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ; ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_include_module(_)
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            !:RevItems = [Item | !.RevItems],
            strip_imported_items_loop(Items, !RevItems)
        )
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        ),
        !:RevItems = [Item | !.RevItems],
        strip_imported_items_loop(Items, !RevItems)
    ).

:- pred strip_assertions(list(item)::in, list(item)::out) is det.

strip_assertions(Items0, Items) :-
    strip_assertions_loop(Items0, [], RevItems),
    list.reverse(RevItems, Items).

:- pred strip_assertions_loop(list(item)::in,
    list(item)::in, list(item)::out) is det.

strip_assertions_loop([], !RevItems).
strip_assertions_loop([Item | Items], !RevItems) :-
    % If this code ever changes to care about the order of the items,
    % you will need to modify strip_imported_items_and_assertions.
    (
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(promise_type_true, _, _, _, _, _)
    ->
        true
    ;
        !:RevItems = [Item | !.RevItems]
    ),
    strip_assertions_loop(Items, !RevItems).

%-----------------------------------------------------------------------------%

:- pred strip_unnecessary_impl_defns(list(item)::in, list(item)::out) is det.

strip_unnecessary_impl_defns(Items0, Items) :-
    some [!IntTypesMap, !ImplTypesMap, !ImplItems] (
        gather_type_defns(Items0, IntItems0, !:ImplItems,
            !:IntTypesMap, !:ImplTypesMap),
        BothTypesMap = multi_map.merge(!.IntTypesMap, !.ImplTypesMap),

        % Work out which module imports in the implementation section of
        % the interface are required by the definitions of equivalence
        % types and dummy types in the implementation.
        get_requirements_of_impl_exported_types(!.IntTypesMap, !.ImplTypesMap,
            BothTypesMap, NecessaryDummyTypeCtors,
            NecessaryAbsImplExpTypeCtors, NecessaryTypeImplImports),

        % Work out which module imports in the implementation section of
        % the interface are required by the definitions of typeclasses
        % in the implementation. Specifically, we require that ones
        % that are needed by any constraints on the typeclasses.
        get_requirements_of_impl_typeclasses(!.ImplItems,
            NecessaryTypeclassImplImports),

        NecessaryImplImports = NecessaryTypeImplImports `set.union`
            NecessaryTypeclassImplImports,

        % If a type in the implementation section isn't dummy and doesn't have
        % foreign type alternatives, make it abstract.
        map.map_values_only(make_impl_type_abstract(BothTypesMap),
            !ImplTypesMap),

        % If there is an exported type declaration for a type with an abstract
        % declaration in the implementation (usually it will originally
        % have been a d.u. type), remove the declaration in the implementation.
        % Don't remove `type_is_abstract_enum' declarations, though.
        FindRemovableAbsExpTypes =
            (pred(TypeCtor::out) is nondet :-
                map.member(!.ImplTypesMap, TypeCtor, Defns),
                all [Defn] (
                    list.member(Defn - _, Defns)
                => (
                    Defn = parse_tree_abstract_type(Details),
                    Details \= abstract_enum_type(_)
                )),
                multi_map.contains(!.IntTypesMap, TypeCtor)
            ),
        solutions(FindRemovableAbsExpTypes, RemovableAbstractExportedTypes),
        RemoveFromImplTypesMap =
            (pred(TypeCtor::in, !.ImplTypesMap::in, !:ImplTypesMap::out)
                    is det :-
                multi_map.delete(TypeCtor, !ImplTypesMap)
            ),
        list.foldl(RemoveFromImplTypesMap, RemovableAbstractExportedTypes,
            !ImplTypesMap),

        AddProjectedItem =
            (pred((_ - ItemTypeDefn)::in, !.ImplItems::in, !:ImplItems::out)
                    is det :-
                Item = item_type_defn(ItemTypeDefn),
                !:ImplItems = [Item | !.ImplItems]
            ),
        AddProjectedItems =
            (pred(_::in, Defns::in, !.ImplItems::in, !:ImplItems::out)
                    is det :-
                list.foldl(AddProjectedItem, Defns, !ImplItems)
            ),
        map.foldl(AddProjectedItems, !.ImplTypesMap, !ImplItems),

        IntItems = [make_pseudo_decl(md_interface) | IntItems0],

        maybe_strip_import_decls(!ImplItems),

        strip_unnecessary_impl_imports(NecessaryImplImports, !ImplItems),
        set.union(NecessaryDummyTypeCtors, NecessaryAbsImplExpTypeCtors,
            AllNecessaryTypeCtors),
        strip_unnecessary_impl_types(AllNecessaryTypeCtors, !ImplItems),
        strip_local_foreign_enum_pragmas(!.IntTypesMap, !ImplItems),
        (
            !.ImplItems = [],
            Items = IntItems
        ;
            !.ImplItems = [_ | _],
            standardize_impl_items(!.ImplItems, StdImplItems),
            ImplSectionItem = make_pseudo_decl(md_implementation),
            list.condense([IntItems, [ImplSectionItem], StdImplItems], Items)
        )
    ).

:- type module_specifier_in_defn
    --->    module_specifier_in_defn(
                prog_context,
                module_specifier
            ).

:- pred standardize_impl_items(list(item)::in, list(item)::out) is det.

standardize_impl_items(Items0, Items) :-
    do_standardize_impl_items(Items0, no, Unexpected, [], RevRemainderItems,
        [], ImportModuleSpecs, [], UseModuleSpecs, [], TypeDefnInfos),
    (
        Unexpected = yes,
        unexpected($module, $pred, "unexpected items")
        % XXX If the above exception is thrown and you need a workaround,
        % you can replace the call to unexpected with this code:
        % Items = Items0
    ;
        Unexpected = no,
        list.reverse(RevRemainderItems, RemainderItems),
        ImportItems = list.map(wrap_import_module_spec, ImportModuleSpecs),
        UseItems = list.map(wrap_use_module_spec, UseModuleSpecs),
        TypeDefnItems = list.map(wrap_type_defn_item, TypeDefnInfos),
        list.condense([ImportItems, UseItems, TypeDefnItems, RemainderItems],
            Items)
    ).

:- func wrap_type_defn_item(item_type_defn_info) = item.

wrap_type_defn_item(ItemTypeDefn) = item_type_defn(ItemTypeDefn).

:- func wrap_import_module_spec(module_specifier_in_defn) = item.

wrap_import_module_spec(ModuleSpecInDefn) = Item :-
    ModuleSpecInDefn = module_specifier_in_defn(Context, ModuleSpec),
    ModuleDefn = md_import([ModuleSpec]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, -1),
    Item = item_module_defn(ItemModuleDefn).

:- func wrap_use_module_spec(module_specifier_in_defn) = item.

wrap_use_module_spec(ModuleSpecInDefn) = Item :-
    ModuleSpecInDefn = module_specifier_in_defn(Context, ModuleSpec),
    ModuleDefn = md_use([ModuleSpec]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, -1),
    Item = item_module_defn(ItemModuleDefn).

:- pred do_standardize_impl_items(list(item)::in, bool::in, bool::out,
    list(item)::in, list(item)::out,
    list(module_specifier_in_defn)::in, list(module_specifier_in_defn)::out,
    list(module_specifier_in_defn)::in, list(module_specifier_in_defn)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

do_standardize_impl_items([], !Unexpected, !RevRemainderItems,
        !ImportSpecs, !UseSpecs, !TypeDefns).
do_standardize_impl_items([Item | Items], !Unexpected,
        !RevRemainderItems, !ImportSpecs, !UseSpecs, !TypeDefns) :-
    ( Item = item_module_defn(ItemModuleDefn) ->
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _),
        (
            ModuleDefn = md_import(ImportModules),
            ( ImportModules = [ModuleSpec] ->
                insert_module_spec(Context, ModuleSpec, !ImportSpecs)
            ;
                unexpected($module, $pred, "non-singleton-module import")
            )
        ;
            ModuleDefn = md_use(UseModules),
            ( UseModules = [ModuleSpec] ->
                insert_module_spec(Context, ModuleSpec, !UseSpecs)
            ;
                unexpected($module, $pred, "non-singleton-module use")
            )
        ;
            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ; ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            !:Unexpected = yes
        ;
            ModuleDefn = md_include_module(_),
            !:RevRemainderItems = [Item | !.RevRemainderItems]
        )
    ; Item = item_type_defn(ItemTypeDefn) ->
        insert_type_defn(ItemTypeDefn, !TypeDefns)
    ;
        !:RevRemainderItems = [Item | !.RevRemainderItems]
    ),
    do_standardize_impl_items(Items, !Unexpected,
        !RevRemainderItems, !ImportSpecs, !UseSpecs, !TypeDefns).

:- pred insert_module_spec(prog_context::in, module_specifier::in,
    list(module_specifier_in_defn)::in, list(module_specifier_in_defn)::out)
    is det.

insert_module_spec(Context, NewModuleSpec, [], [New]) :-
    New = module_specifier_in_defn(Context, NewModuleSpec).
insert_module_spec(Context, NewModuleSpec, [Head | Tail], Result) :-
    Head = module_specifier_in_defn(_, HeadModuleSpec),
    compare(CompareSymName, NewModuleSpec, HeadModuleSpec),
    ( CompareSymName = (<) ->
        New = module_specifier_in_defn(Context, NewModuleSpec),
        Result = [New, Head | Tail]
    ;
        insert_module_spec(Context, NewModuleSpec, Tail, NewTail),
        Result = [Head | NewTail]
    ).

:- pred insert_type_defn(item_type_defn_info::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

insert_type_defn(New, [], [New]).
insert_type_defn(New, [Head | Tail], Result) :-
    New = item_type_defn_info(_, NewSymName, NewParams, _, _, _, _),
    Head = item_type_defn_info(_, HeadSymName, HeadParams, _, _, _, _),
    compare(CompareSymName, NewSymName, HeadSymName),
    (
        (
            CompareSymName = (<)
        ;
            CompareSymName = (=),
            list.length(NewParams, NewParamsLength),
            list.length(HeadParams, HeadParamsLength),
            compare(Compare, NewParamsLength, HeadParamsLength),
            Compare = (<)
        )
    ->
        Result = [New, Head | Tail]
    ;
        insert_type_defn(New, Tail, NewTail),
        Result = [Head | NewTail]
    ).

:- pred make_impl_type_abstract(type_defn_map::in,
    assoc_list(type_defn, item_type_defn_info)::in,
    assoc_list(type_defn, item_type_defn_info)::out) is det.

make_impl_type_abstract(TypeDefnMap, !TypeDefnPairs) :-
    (
        !.TypeDefnPairs = [TypeDefn0 - ItemTypeDefn0],
        TypeDefn0 = parse_tree_du_type(Ctors, MaybeEqCmp, MaybeDirectArgCtors)
    ->
        (
            constructor_list_represents_dummy_argument_type(TypeDefnMap, Ctors,
                MaybeEqCmp, MaybeDirectArgCtors)
        ->
            % Leave dummy types alone.
            true
        ;
            ( du_type_is_enum(Ctors, NumBits) ->
                Details = abstract_enum_type(NumBits)
            ;
                Details = abstract_type_general
            ),
            Defn = parse_tree_abstract_type(Details),
            ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn := Defn,
            !:TypeDefnPairs = [Defn - ItemTypeDefn]
        )
    ;
        true
    ).

    % Certain types, e.g. io.state and store.store(S), are just dummy types
    % used to ensure logical semantics; there is no need to actually pass them,
    % and so when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % See the documentation for `type_util.check_dummy_type' for the definition
    % of a dummy type.
    %
    % NOTE: changes here may require changes to `type_util.check_dummy_type'.
    %
:- pred constructor_list_represents_dummy_argument_type(type_defn_map::in,
    list(constructor)::in, maybe(unify_compare)::in,
    maybe(list(sym_name_and_arity))::in) is semidet.

constructor_list_represents_dummy_argument_type(TypeDefnMap,
        Ctors, MaybeEqCmp, MaybeDirectArgCtors) :-
    constructor_list_represents_dummy_argument_type_2(TypeDefnMap,
        Ctors, MaybeEqCmp, MaybeDirectArgCtors, []).

:- pred constructor_list_represents_dummy_argument_type_2(type_defn_map::in,
    list(constructor)::in, maybe(unify_compare)::in,
    maybe(list(sym_name_and_arity))::in, list(mer_type)::in) is semidet.

constructor_list_represents_dummy_argument_type_2(TypeDefnMap, [Ctor], no, no,
        CoveredTypes) :-
    Ctor = ctor(ExistQTVars, Constraints, _Name, Args, _Context),
    ExistQTVars = [],
    Constraints = [],
    (
        % A single zero-arity constructor.
        Args = []
    ;
        % A constructor with a single dummy argument.
        Args = [ctor_arg(_, ArgType, _, _)],
        ctor_arg_is_dummy_type(TypeDefnMap, ArgType, CoveredTypes) = yes
    ).

:- func ctor_arg_is_dummy_type(type_defn_map, mer_type, list(mer_type)) = bool.

ctor_arg_is_dummy_type(TypeDefnMap, Type, CoveredTypes0) = IsDummyType :-
    (
        Type = defined_type(SymName, TypeArgs, _Kind),
        ( list.member(Type, CoveredTypes0) ->
            % The type is circular.
            IsDummyType = no
        ;
            Arity = list.length(TypeArgs),
            TypeCtor = type_ctor(SymName, Arity),
            (
                check_builtin_dummy_type_ctor(TypeCtor)
                    = is_builtin_dummy_type_ctor
            ->
                IsDummyType = yes
            ;
                % Can we find a definition of the type that tells us it is a
                % dummy type?
                multi_map.search(TypeDefnMap, TypeCtor, TypeDefns),
                list.member(TypeDefn - _, TypeDefns),
                TypeDefn = parse_tree_du_type(TypeCtors, MaybeEqCmp,
                    MaybeDirectArgCtors),
                CoveredTypes = [Type | CoveredTypes0],
                constructor_list_represents_dummy_argument_type_2(TypeDefnMap,
                    TypeCtors, MaybeEqCmp, MaybeDirectArgCtors, CoveredTypes)
            ->
                IsDummyType = yes
            ;
                IsDummyType = no
            )
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        ; Type = tuple_type(_, _)
        ; Type = higher_order_type(_, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        IsDummyType = no
    ).

    % strip_unnecessary_impl_imports(NecessaryModules, !Items):
    %
    % Remove all import_module and use_module declarations for
    % modules that are not in `NecessaryModules',
    %
    % NOTE: This will only work if each item corresponding
    % to an import_module or use_module declaration only imports
    % a single module. (This should be the case, see prog_io.m.)
    %
:- pred strip_unnecessary_impl_imports(set(module_name)::in, list(item)::in,
    list(item)::out) is det.

strip_unnecessary_impl_imports(NecessaryImports, !Items) :-
    list.filter(is_not_unnecessary_impl_import(NecessaryImports), !Items).

:- pred is_not_unnecessary_impl_import(set(module_name)::in, item::in)
    is semidet.

is_not_unnecessary_impl_import(NecessaryImports, Item) :-
    ( Item = item_module_defn(ItemModuleDefn) ->
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ( ModuleDefn = md_use(Modules)
            ; ModuleDefn = md_import(Modules)
            )
        ->
            ( Modules = [ModuleName] ->
                set.member(ModuleName, NecessaryImports)
            ;
                unexpected($module, $pred, "non-singleton import or use decl")
            )
        ;
            true
        )
    ;
        true
    ).

    % strip_unnecessary_impl_types(NecessaryTypeCtors, !Items):
    %
    % Remove all type declarations for type constructors that are
    % not in NecessaryTypeCtors.
    %
:- pred strip_unnecessary_impl_types(set(type_ctor)::in,
    list(item)::in, list(item)::out) is det.

strip_unnecessary_impl_types(NecessaryTypeCtors, !Items) :-
    list.filter(is_not_unnecessary_impl_type(NecessaryTypeCtors), !Items).

:- pred is_not_unnecessary_impl_type(set(type_ctor)::in, item::in) is semidet.

is_not_unnecessary_impl_type(NecessaryTypeCtors, Item) :-
    ( Item = item_type_defn(ItemTypeDefn) ->
        ItemTypeDefn = item_type_defn_info(_, SymName, Params, _, _, _, _),
        TypeCtor = type_ctor(SymName, list.length(Params)),
        set.member(TypeCtor, NecessaryTypeCtors)
    ;
        true
    ).

    % get_requirements_of_impl_exported_types(InterfaceTypeMap, ImplTypeMap,
    %   BothTypeMap, DummyTypeCtors, NecessaryTypeCtors, Modules):
    %
    % Figure out the set of abstract equivalence type constructors
    % (i.e. the types that are exported as abstract types and which are defined
    % in the implementation section as equivalence types or as foreign types).
    % Return in NecessaryTypeCtors the smallest set containing those
    % constructors, and the set of private type constructors referred to
    % by the right hand side of any type in NecessaryTypeCtors.
    %
    % Return in DummyTypeCtors the set of dummy type constructors.
    %
    % Given a du type definition in the implementation section, we should
    % include it in AbsImplExpLhsTypeCtors if the type constructor is abstract
    % exported and the implementation section also contains a foreign_type
    % definition of the type constructor.
    %
    % Given a enumeration type definition in the implementation section, we
    % should include it in AbsImplExpEnumTypeCtors if the type constructor is
    % abstract exported.
    %
    % Return in Modules the set of modules that define the type constructors
    % in NecessaryTypeCtors.
    %
:- pred get_requirements_of_impl_exported_types(type_defn_map::in,
    type_defn_map::in, type_defn_map::in,
    set(type_ctor)::out, set(type_ctor)::out, set(module_name)::out) is det.

get_requirements_of_impl_exported_types(InterfaceTypeMap, ImplTypeMap,
        BothTypeMap, DummyTypeCtors, NecessaryTypeCtors, Modules) :-
    multi_map.to_flat_assoc_list(ImplTypeMap, ImplTypes),
    list.foldl3(
        accumulate_abs_impl_exported_type_lhs(InterfaceTypeMap, BothTypeMap),
        ImplTypes, set.init, AbsImplExpLhsTypeCtors,
        set.init, AbsImplExpEnumTypeCtors, set.init, DummyTypeCtors),
    set.fold3(accumulate_abs_impl_exported_type_rhs(ImplTypeMap),
        AbsImplExpLhsTypeCtors,
        set.init, AbsEqvRhsTypeCtors, set.init, ForeignDuFieldTypeCtors,
        set.init, Modules),
    NecessaryTypeCtors = set.union_list([AbsImplExpLhsTypeCtors,
        AbsEqvRhsTypeCtors, ForeignDuFieldTypeCtors,
        AbsImplExpEnumTypeCtors]).

:- pred accumulate_abs_impl_exported_type_lhs(type_defn_map::in,
    type_defn_map::in,
    pair(type_ctor, pair(type_defn, item_type_defn_info))::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_impl_exported_type_lhs(InterfaceTypeMap, BothTypesMap,
        TypeCtor - (TypeDefn - _Item), !AbsEqvLhsTypeCtors,
        !AbsImplExpEnumTypeCtors, !DummyTypeCtors) :-
    % A type may have multiple definitions because it may be defined both
    % as a foreign type and as a Mercury type. We grab any equivalence types
    % that are in there.
    (
        TypeDefn = parse_tree_eqv_type(_RhsType),
        map.search(InterfaceTypeMap, TypeCtor, _)
    ->
        set.insert(TypeCtor, !AbsEqvLhsTypeCtors)
    ;
        TypeDefn = parse_tree_foreign_type(_, _, _),
        map.search(InterfaceTypeMap, TypeCtor, _)
    ->
        set.insert(TypeCtor, !AbsEqvLhsTypeCtors)
    ;
        TypeDefn = parse_tree_du_type(Ctors, MaybeEqCmp, MaybeDirectArgCtors)
    ->
        (
            map.search(InterfaceTypeMap, TypeCtor, _),
            du_type_is_enum(Ctors, _NumBits)
        ->
            set.insert(TypeCtor, !AbsImplExpEnumTypeCtors)
        ;
            constructor_list_represents_dummy_argument_type(BothTypesMap,
                Ctors, MaybeEqCmp, MaybeDirectArgCtors)
        ->
            set.insert(TypeCtor, !DummyTypeCtors)
        ;
            true
        )
    ;
        true
    ).

:- pred accumulate_abs_impl_exported_type_rhs(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_impl_exported_type_rhs(ImplTypeMap, TypeCtor,
        !AbsEqvRhsTypeCtors, !ForeignDuFieldTypeCtors, !Modules) :-
    ( map.search(ImplTypeMap, TypeCtor, TypeDefns) ->
        list.foldl3(accumulate_abs_eqv_type_rhs_2(ImplTypeMap), TypeDefns,
            !AbsEqvRhsTypeCtors, !ForeignDuFieldTypeCtors, !Modules)
    ;
        true
    ).

:- pred accumulate_abs_eqv_type_rhs_2(type_defn_map::in,
    pair(type_defn, item_type_defn_info)::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs_2(ImplTypeMap, TypeDefn - _,
        !AbsEqvRhsTypeCtors, !ForeignDuFieldTypeCtors, !Modules) :-
    ( TypeDefn = parse_tree_eqv_type(RhsType) ->
        type_to_type_ctor_set(RhsType, set.init, RhsTypeCtors),
        set.difference(RhsTypeCtors, !.AbsEqvRhsTypeCtors, NewRhsTypeCtors),
        set.fold(accumulate_modules, NewRhsTypeCtors, !Modules),
        set.union(NewRhsTypeCtors, !AbsEqvRhsTypeCtors),
        set.fold3(accumulate_abs_impl_exported_type_rhs(ImplTypeMap),
            NewRhsTypeCtors, !AbsEqvRhsTypeCtors, set.init, _, !Modules)
    ; TypeDefn = parse_tree_du_type(Ctors, _, _) ->
        % There must exist a foreign type alternative to this type. As the du
        % type will be exported, we require the types of all the fields.
        ctors_to_type_ctor_set(Ctors, set.init, RhsTypeCtors),
        set.union(RhsTypeCtors, !ForeignDuFieldTypeCtors),
        set.fold(accumulate_modules, RhsTypeCtors, !Modules)
    ;
        true
    ).

:- pred accumulate_modules(type_ctor::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules(TypeCtor, !Modules) :-
    % NOTE: This assumes that everything has been module qualified.
    TypeCtor = type_ctor(SymName, _Arity),
    (
        SymName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        SymName = unqualified(_),
        unexpected($module, $pred, "unqualified type encountered")
    ).

    % Given a type, return the set of user-defined type constructors
    % occurring in it.
    %
:- pred type_to_type_ctor_set(mer_type::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

type_to_type_ctor_set(Type, !TypeCtors) :-
    ( type_to_ctor_and_args(Type, TypeCtor, Args) ->
        TypeCtor = type_ctor(SymName, _Arity),
        (
            type_ctor_is_higher_order(TypeCtor, _, _, _)
        ->
            % Higher-order types are builtin so just get the type_ctors
            % from the arguments.
            true
        ;
            type_ctor_is_tuple(TypeCtor)
        ->
            % Tuples are builtin so just get the type_ctors from the
            % arguments.
            true
        ;
            ( SymName = unqualified("int")
            ; SymName = unqualified("float")
            ; SymName = unqualified("string")
            ; SymName = unqualified("character")
            )
        ->
            % We don't need to import these modules as the types are builtin.
            true
        ;
            set.insert(TypeCtor, !TypeCtors)
        ),
        list.foldl(type_to_type_ctor_set, Args, !TypeCtors)
    ;
        true
    ).

:- pred ctors_to_type_ctor_set(list(constructor)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

ctors_to_type_ctor_set([], !TypeCtors).
ctors_to_type_ctor_set([Ctor | Ctors], !TypeCtors) :-
    Ctor = ctor(_, _, _, ConsArgs, _),
    cons_args_to_type_ctor_set(ConsArgs, !TypeCtors),
    ctors_to_type_ctor_set(Ctors, !TypeCtors).

:- pred cons_args_to_type_ctor_set(list(constructor_arg)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

cons_args_to_type_ctor_set([], !TypeCtors).
cons_args_to_type_ctor_set([Arg | Args], !TypeCtors) :-
    Arg = ctor_arg(_, Type, _, _),
    type_to_type_ctor_set(Type, !TypeCtors),
    cons_args_to_type_ctor_set(Args, !TypeCtors).

:- type type_defn_map ==
    multi_map(type_ctor, pair(type_defn, item_type_defn_info)).
:- type type_defn_pair ==
    pair(type_ctor, pair(type_defn, item_type_defn_info)).

:- pred gather_type_defns(list(item)::in, list(item)::out, list(item)::out,
    type_defn_map::out, type_defn_map::out) is det.

gather_type_defns(Items0, IntItems, ImplItems, IntTypesMap, ImplTypesMap) :-
    gather_type_defns_2(no, Items0, [], RevIntItems, [], RevImplItems,
        map.init, IntTypesMap, map.init, ImplTypesMap),
    list.reverse(RevIntItems, IntItems),
    list.reverse(RevImplItems, ImplItems).

:- pred gather_type_defns_2(bool::in, list(item)::in,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out,
    type_defn_map::in, type_defn_map::out,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defns_2(_, [], !RevIntItems, !RevImplItems,
        !IntTypesMap, !ImplTypesMap).
gather_type_defns_2(!.InInterface, [Item | Items],
        !RevIntItems, !RevImplItems, !IntTypesMap, !ImplTypesMap) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ModuleDefn = md_interface,
            NewInInterface = yes
        ;
            ModuleDefn = md_implementation,
            NewInInterface = no
        )
    ->
        !:InInterface = NewInInterface
    ;
        Item = item_type_defn(ItemTypeDefn)
    ->
        ItemTypeDefn = item_type_defn_info(_, Name, Args, Body, _, _, _),
        TypeCtor = type_ctor(Name, length(Args)),
        (
            !.InInterface = yes,
            !:RevIntItems = [Item | !.RevIntItems],
            gather_type_defn(TypeCtor, Body, ItemTypeDefn, !IntTypesMap)
        ;
            !.InInterface = no,
            % We don't add this to !RevImplItems yet -- we may be removing
            % this item.
            gather_type_defn(TypeCtor, Body, ItemTypeDefn, !ImplTypesMap)
        )
    ;
        (
            !.InInterface = yes,
            !:RevIntItems = [Item | !.RevIntItems]
        ;
            !.InInterface = no,
            !:RevImplItems = [Item | !.RevImplItems]
        )
    ),
    gather_type_defns_2(!.InInterface, Items, !RevIntItems, !RevImplItems,
        !IntTypesMap, !ImplTypesMap).

:- pred gather_type_defn(type_ctor::in, type_defn::in, item_type_defn_info::in,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defn(TypeCtor, Body, ItemTypeDefn, !DefnMap) :-
    multi_map.set(TypeCtor, Body - ItemTypeDefn, !DefnMap).

:- pred get_requirements_of_impl_typeclasses(list(item)::in,
    set(module_name)::out) is det.

get_requirements_of_impl_typeclasses(ImplItems, Modules) :-
    list.foldl(get_requirements_of_impl_typeclass,
        ImplItems, set.init, Modules).

:- pred get_requirements_of_impl_typeclass(item::in,
    set(module_name)::in, set(module_name)::out) is det.

get_requirements_of_impl_typeclass(Item, !Modules) :-
    (
        Item = item_typeclass(ItemTypeClass),
        Constraints = ItemTypeClass ^ tc_constraints,
        list.foldl(get_requirements_of_impl_from_constraint, Constraints,
            !Modules)
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_module_defn(_)
        ; Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        )
    ).

:- pred get_requirements_of_impl_from_constraint(prog_constraint::in,
    set(module_name)::in, set(module_name)::out) is det.

get_requirements_of_impl_from_constraint(Constraint, !Modules) :-
    Constraint = constraint(ClassName, Args),
    % NOTE: This assumes that everything has been module qualified.
    (
        ClassName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        ClassName = unqualified(_),
        unexpected($module, $pred, "unknown typeclass in constraint")
    ),
    get_modules_from_constraint_arg_types(Args, !Modules).

:- pred get_modules_from_constraint_arg_types(list(mer_type)::in,
    set(module_name)::in, set(module_name)::out) is det.

get_modules_from_constraint_arg_types(ArgTypes, !Modules) :-
    list.foldl(get_modules_from_constraint_arg_type, ArgTypes, !Modules).

:- pred get_modules_from_constraint_arg_type(mer_type::in,
    set(module_name)::in, set(module_name)::out) is det.

get_modules_from_constraint_arg_type(ArgType, !Modules) :-
    (
        % Do nothing for these types - they cannot affect the set of
        % implementation imports in an interface file.
        ( ArgType = type_variable(_, _)
        ; ArgType = builtin_type(_)
        )
    ;
        ArgType = defined_type(TypeName, Args, _),
        ( sym_name_get_module_name(TypeName, ModuleName) ->
            set.insert(ModuleName, !Modules)
        ;
            unexpected($module, $pred, "unknown type encountered")
        ),
        get_modules_from_constraint_arg_types(Args, !Modules)
    ;
        (
            ArgType = tuple_type(Args, _)
        ;
            ArgType = apply_n_type(_, Args, _)
        ;
            ArgType = kinded_type(KindedType, _), Args = [KindedType]
        ;
            ArgType = higher_order_type(Args0, MaybeRetType, _, _),
            (
                MaybeRetType = yes(RetType),
                Args = [RetType  | Args0]
            ;
                MaybeRetType = no,
                Args = Args0
            )
        ),
        get_modules_from_constraint_arg_types(Args, !Modules)
    ).

    % Retain only those foreign_enum pragmas that correspond to types
    % defined in the interface of a module.
    %
:- pred strip_local_foreign_enum_pragmas(type_defn_map::in,
    list(item)::in, list(item)::out) is det.

strip_local_foreign_enum_pragmas(IntTypeMap, !ImplItems) :-
    list.filter(foreign_enum_is_local(IntTypeMap), !ImplItems).

:- pred foreign_enum_is_local(type_defn_map::in, item::in) is semidet.

foreign_enum_is_local(TypeDefnMap, Item) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        Pragma = pragma_foreign_enum(FEInfo),
        FEInfo = pragma_info_foreign_enum(_Lang, TypeCtor, _Values)
    ->
        % We only add a pragma foreign_enum pragma to the interface file
        % if it corresponds to a type _definition_ in the interface of the
        % module.
        map.search(TypeDefnMap, TypeCtor, Defns),
        Defns \= [parse_tree_abstract_type(_) - _]
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred report_and_strip_clauses_in_interface(list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_and_strip_clauses_in_interface(Items0, Items, !Specs) :-
    report_and_strip_clauses_in_interface_loop(Items0, [], RevItems, !Specs),
    list.reverse(RevItems, Items).

:- pred report_and_strip_clauses_in_interface_loop(list(item)::in,
    list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_and_strip_clauses_in_interface_loop([], !RevItems, !Specs).
report_and_strip_clauses_in_interface_loop([Item0 | Items0],
        !RevItems, !Specs) :-
    % We either add Item0 to !RevItems, or a new spec to !Specs.
    (
        Item0 = item_clause(ItemClause0),
        Context = ItemClause0 ^ cl_context,
        Spec = clause_in_interface_warning("clause", Context),
        !:Specs = [Spec | !.Specs]
    ;
        Item0 = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, Context, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            Spec = clause_in_interface_warning("pragma", Context),
            !:Specs = [Spec | !.Specs]
        ;
            AllowedInInterface = yes,
            !:RevItems = [Item0 | !.RevItems]
        )
    ;
        ( Item0 = item_module_start(_)
        ; Item0 = item_module_end(_)
        ; Item0 = item_module_defn(_)
        ; Item0 = item_type_defn(_)
        ; Item0 = item_inst_defn(_)
        ; Item0 = item_mode_defn(_)
        ; Item0 = item_pred_decl(_)
        ; Item0 = item_mode_decl(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_typeclass(_)
        ; Item0 = item_instance(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_mutable(_)
        ; Item0 = item_nothing(_)
        ),
        !:RevItems = [Item0 | !.RevItems]
    ),
    report_and_strip_clauses_in_interface_loop(Items0, !RevItems, !Specs).

:- func clause_in_interface_warning(string, prog_context) = error_spec.

clause_in_interface_warning(ClauseOrPragma, Context) = Spec :-
    Pieces = [words("Warning:"), words(ClauseOrPragma),
        words("in module interface.")],
    Spec = error_spec(severity_warning, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%-----------------------------------------------------------------------------%

:- pred actually_write_interface_file(globals::in, file_name::in,
    module_name::in, string::in, maybe(timestamp)::in, list(item)::in,
    io::di, io::uo) is det.

actually_write_interface_file(Globals, _SourceFileName, ModuleName, Suffix,
        MaybeTimestamp, InterfaceItems0, !IO) :-
    % Create (e.g.) `foo.int.tmp'.
    string.append(Suffix, ".tmp", TmpSuffix),
    module_name_to_file_name(Globals, ModuleName, Suffix,
        do_create_dirs, OutputFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, TmpSuffix,
        do_not_create_dirs, TmpOutputFileName, !IO),

    globals.set_option(line_numbers, bool(no), Globals, NoLineNumGlobals),
    globals.lookup_bool_option(NoLineNumGlobals, generate_item_version_numbers,
        GenerateVersionNumbers),
    io_get_disable_generate_item_version_numbers(DisableVersionNumbers, !IO),
    ( if
        GenerateVersionNumbers = yes,
        DisableVersionNumbers = no
    then
        % Find the timestamp of the current module.
        (
            MaybeTimestamp = yes(Timestamp),

            % Read in the previous version of the file.
            read_module_ignore_errors(NoLineNumGlobals, ModuleName, Suffix,
                "Reading old interface for module",
                do_search, do_not_return_timestamp, OldItems, OldErrors,
                _OldIntFileName, _OldTimestamp, !IO),
            ( if set.is_empty(OldErrors) then
                MaybeOldItems = yes(OldItems)
            else
                % If we can't read in the old file, the timestamps will
                % all be set to the modification time of the source file.
                MaybeOldItems = no
            ),
            recompilation.version.compute_version_numbers(Timestamp,
                InterfaceItems0, MaybeOldItems, VersionNumbers),
            VersionNumberItemModuleDefn = item_module_defn_info(
                md_version_numbers(ModuleName, VersionNumbers),
                term.context_init, -1),
            VersionNumberItem = item_module_defn(VersionNumberItemModuleDefn),
            ( if
                InterfaceItems0 = [FirstItem | InterfaceItems1],
                FirstItem = item_module_defn(FirstItemModuleDefn),
                FirstItemModuleDefn =
                    item_module_defn_info(FirstModuleDefn, _, _),
                FirstModuleDefn = md_interface
            then
                InterfaceItems = [FirstItem, VersionNumberItem
                    | InterfaceItems1]
            else
                InterfaceItems = [make_pseudo_decl(md_interface),
                    VersionNumberItem | InterfaceItems0]
            )
        ;
            MaybeTimestamp = no,
            unexpected($module, $pred,
                "with `--smart-recompilation', timestamp not read")
        )
    else
        InterfaceItems = InterfaceItems0
    ),
    convert_to_mercury(NoLineNumGlobals, ModuleName, TmpOutputFileName,
        InterfaceItems, !IO),
    % Start using the original globals again.
    update_interface(Globals, OutputFileName, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given a module interface (well, a list of items), extract the
    % short interface part of that module, i.e. the exported
    % type/typeclass/inst/mode declarations, but not the exported pred or
    % constructor declarations. If the module interface imports other modules,
    % then the short interface needs to include those import_module
    % declarations only if the short interface contains some equivalence types
    % or some mode or inst definitions that might use declarations in the
    % imported modules. If the short interface is empty, or only contains
    % abstract type declarations, then it doesn't need any import_module
    % declarations.
    %
:- pred get_short_interface(list(item)::in, short_interface_kind::in,
    list(item)::out) is det.

get_short_interface(Items0, Kind, Items) :-
    get_short_interface_acc(Items0, Kind, [], RevItems),
    list.reverse(RevItems, Items1),
    maybe_strip_import_decls(Items1, Items2),
    order_items(Items2, Items).

:- pred get_short_interface_acc(list(item)::in, short_interface_kind::in,
    list(item)::in, list(item)::out) is det.

get_short_interface_acc([], _Kind, !RevItems).
get_short_interface_acc([Item | Items], Kind, !RevItems) :-
    ( make_abstract_defn(Item, Kind, AbstractItem) ->
        !:RevItems = [AbstractItem | !.RevItems]
    ; make_abstract_unify_compare(Item, Kind, AbstractItem) ->
        !:RevItems = [AbstractItem | !.RevItems]
    ;
        Include = include_in_short_interface(Item),
        (
            Include = yes,
            !:RevItems = [Item | !.RevItems]
        ;
            Include = no
        )
    ),
    get_short_interface_acc(Items, Kind, !RevItems).

:- func include_in_short_interface(item) = bool.

include_in_short_interface(Item) = Include :-
    (
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_module_defn(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_instance(_)
        ),
        Include = yes
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        % XXX This if-then-else should be a switch, or (even better)
        % we should take pragma_foreign_import_modules out of the pragma items
        % and given them their own item type.
        ( Pragma = pragma_foreign_import_module(_) ->
            Include = yes
        ;
            Include = no
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        ),
        Include = no
    ).

:- pred maybe_strip_import_decls(list(item)::in, list(item)::out) is det.

maybe_strip_import_decls(!Items) :-
    (
        some [Item] (
            list.member(Item, !.Items),
            item_needs_imports(Item) = yes
        )
    ->
        true
    ;
        list.filter(not_import_or_use_item, !Items)
    ),
    (
        some [Item] (
            list.member(Item, !.Items),
            item_needs_foreign_imports(Item) = [_ | _]
        )
    ->
        true
    ;
        NotPragmaForeignImport =
            (pred(ThisItem::in) is semidet :-
                \+ (
                    ThisItem = item_pragma(ThisItemPragma),
                    ThisItemPragma = item_pragma_info(_, Pragma, _, _),
                    Pragma = pragma_foreign_import_module(_)
                )
            ),
        list.filter(NotPragmaForeignImport, !Items)
    ).

%-----------------------------------------------------------------------------%

    % Put the given list of items into a sort of standard order. The idea is
    % that just reordering the contents of e.g. an interface section without
    % changing the set of exported entities should not cause a change in the
    % interface files. The "sort of" is because we are not doing as good a job
    % as we could. Unfortunately, doing significantly better is quite hard
    % with the current representation of the module, which is just a list of
    % items without further structure.
    %
    % This predicate requires items in the original order. One reason is that
    % if does not change the order of pred or mode declarations. If it were
    % given a reversed list in which a mode declaration came before the pred
    % declarations it refers to, the reader would create an implicit pred
    % declaration when it saw the mode declaration, and it would be confused
    % by the later appearance of the actual pred declaration.
    %
:- pred order_items(list(item)::in, list(item)::out) is det.

order_items(Items0, Items) :-
    filter_unnecessary_flips(Items0, other, Items1),
    do_order_items(Items1, Items2),
    % Delete any redundant :- interface and :- implementation markers at the
    % end, to make Items as insensitive as we can to the number of interface
    % sections in the source file. If some of the implementation sections
    % are not empty, we won't be fully successful.
    list.reverse(Items2, RevItems2),
    list.takewhile(interface_or_import_marker, RevItems2, _, RevItems),
    list.reverse(RevItems, Items).

:- pred interface_or_import_marker(item::in) is semidet.

interface_or_import_marker(Item) :-
    Item = item_module_defn(ItemModuleDefn),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
    ( ModuleDefn = md_interface
    ; ModuleDefn = md_implementation
    ).

:- pred not_import_or_use_item(item::in) is semidet.

not_import_or_use_item(Item) :-
    not import_or_use_item(Item).

:- pred import_or_use_item(item::in) is semidet.

import_or_use_item(Item) :-
    Item = item_module_defn(ItemModuleDefn),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
    ( ModuleDefn = md_import(_)
    ; ModuleDefn = md_use(_)
    ).

    % Which section of the module we are in. The "other" alternative
    % reflects my ignorance (based on the lack of documentation) of
    % the invariants that govern the items involved in the representation
    % of nested modules. -zs

:- type cur_pos
    --->    in_interface
    ;       in_implementation
    ;       other.

:- pred filter_unnecessary_flips(list(item)::in, cur_pos::in, list(item)::out)
    is det.

filter_unnecessary_flips([], _, []).
filter_unnecessary_flips([Item], _, [Item]).
filter_unnecessary_flips([Item1, Item2 | Items0], CurPos, Items) :-
    (
        CurPos = in_interface,
        Item1 = item_module_defn(ItemModuleDefn1),
        ItemModuleDefn1 = item_module_defn_info(md_implementation, _, _),
        Item2 = item_module_defn(ItemModuleDefn2),
        ItemModuleDefn2 = item_module_defn_info(md_interface, _, _)
    ->
        filter_unnecessary_flips(Items0, CurPos, Items)
    ;
        CurPos = in_implementation,
        Item1 = item_module_defn(ItemModuleDefn1),
        ItemModuleDefn1 = item_module_defn_info(md_interface, _, _),
        Item2 = item_module_defn(ItemModuleDefn2),
        ItemModuleDefn2 = item_module_defn_info(md_implementation, _, _)
    ->
        filter_unnecessary_flips(Items0, CurPos, Items)
    ;
        (
            Item1 = item_module_defn(ItemModuleDefn1),
            ItemModuleDefn1 = item_module_defn_info(md_implementation, _, _)
        ->
            NextPos = in_implementation
        ;
            Item1 = item_module_defn(ItemModuleDefn1),
            ItemModuleDefn1 = item_module_defn_info(md_interface, _, _)
        ->
            NextPos = in_interface
        ;
            Chunkable1 = chunkable_item(Item1),
            (
                Chunkable1 = yes,
                NextPos = CurPos
            ;
                Chunkable1 = no,
                NextPos = other
            )
        ),
        filter_unnecessary_flips([Item2 | Items0], NextPos, ItemsTail),
        Items = [Item1 | ItemsTail]
    ).

    % Find a chunk of items which should in most cases (but unfortunately
    % not all cases) be all the exported items, and put them in a standard
    % order, with import_module and use_module items first in lexical order,
    % then type, inst and mode definitions, again in lexical order, then
    % pred and predmode declarations, in lexical order by sym_name, and
    % finally all other items in the chunk. The chunk consists of the initial
    % prefix of items for which this reordering is safe. The chunk will then
    % be followed by the ordered versions of later chunks, if any.
    %
:- pred do_order_items(list(item)::in, list(item)::out) is det.

do_order_items([], []).
do_order_items([Item0 | Items0], OrderedItems) :-
    Chunkable0 = chunkable_item(Item0),
    (
        Chunkable0 = yes,
        list.takewhile(is_chunkable, Items0, FrontItems, RemainItems),
        list.filter(is_reorderable, [Item0 | FrontItems],
            ReorderableItems, NonReorderableItems),
        list.filter(import_or_use, ReorderableItems,
            ImportReorderableItems, NonImportReorderableItems),
        list.filter(symname_orderable, NonReorderableItems,
            SymNameItems, NonSymNameItems),
        % We rely on the sort being stable to keep the items with the same
        % sym_names in their original order.
        list.sort(compare_by_symname, SymNameItems, OrderedSymNameItems),
        do_order_items(RemainItems, OrderedRemainItems),
        OrderedItems = list.sort(ImportReorderableItems) ++
            list.sort(NonImportReorderableItems) ++
            OrderedSymNameItems ++ NonSymNameItems ++ OrderedRemainItems
    ;
        Chunkable0 = no,
        do_order_items(Items0, OrderedItemsTail),
        OrderedItems = [Item0 | OrderedItemsTail]
    ).

:- pred import_or_use(item::in) is semidet.

import_or_use(item_module_defn(ItemModuleDefn)) :-
    ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
    ( ModuleDefn = md_import(_)
    ; ModuleDefn = md_use(_)
    ).

:- pred is_reorderable(item::in) is semidet.

is_reorderable(Item) :-
    reorderable_item(Item) = yes.

    % The kinds of items for which reorderable_item returns yes can be
    % arbitrarily reordered with respect to each other and with respect to
    % other chunkable items in all kinds of interface files (.int, .int2,
    % .int3, and .int0). This predicate is not relevant to .opt and
    % .trans_opt files, since those are generated from the HLDS, not
    % from item lists.
    %
    % We should make this predicate call "unexpected" for items that should
    % never occur in interface files. However, I don't have a reliable list
    % of exactly which items those are.
    %
:- func reorderable_item(item) = bool.

reorderable_item(Item) = Reorderable :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        Reorderable = reorderable_module_defn(ModuleDefn)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        Reorderable = reorderable_pragma_type(Pragma)
    ;
        ( Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ),
        Reorderable = yes
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_clause(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        ),
        Reorderable = no
    ).

:- func reorderable_module_defn(module_defn) = bool.

reorderable_module_defn(ModuleDefn) = Reorderable :-
    (
        ( ModuleDefn = md_import(_)
        ; ModuleDefn = md_export(_)
        ; ModuleDefn = md_external(_, _)
        ; ModuleDefn = md_use(_)
        ),
        Reorderable = yes
    ;
        ( ModuleDefn = md_abstract_imported
        ; ModuleDefn = md_implementation
        ; ModuleDefn = md_imported(_)
        ; ModuleDefn = md_include_module(_)
        ; ModuleDefn = md_interface
        ; ModuleDefn = md_implementation_but_exported_to_submodules
        ; ModuleDefn = md_opt_imported
        ; ModuleDefn = md_transitively_imported
        ; ModuleDefn = md_used(_)
        ; ModuleDefn = md_version_numbers(_, _)
        ),
        Reorderable = no
    ).

:- func reorderable_pragma_type(pragma_type) = bool.

reorderable_pragma_type(Pragma) = Reorderable :-
    (
        ( Pragma = pragma_check_termination( _)
        ; Pragma = pragma_does_not_terminate( _)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_foreign_proc_export(_)
        ; Pragma = pragma_foreign_export_enum(_)
        ; Pragma = pragma_foreign_enum(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_obsolete(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_reserve_tag(_)
        ; Pragma = pragma_oisu(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_require_feature_set(_)
        ),
        Reorderable = yes
    ;
        ( Pragma = pragma_foreign_code(_)
        ; Pragma = pragma_foreign_decl(_)
        ; Pragma = pragma_foreign_import_module(_)
        ; Pragma = pragma_foreign_proc(_)
        ; Pragma = pragma_source_file(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_fact_table(_)
        ),
        Reorderable = no
    ).

:- pred is_chunkable(item::in) is semidet.

is_chunkable(Item) :-
    chunkable_item(Item) = yes.

    % Given a list of items for which chunkable_item returns yes, we need
    % to keep the relative order of the non-reorderable items, but we can
    % move the reorderable items around arbitrarily.
    %
    % We should make this predicate call "unexpected" for items that should
    % never occur in interface files. However, I don't have a reliable list
    % of exactly which items those are.
    %
:- func chunkable_item(item) = bool.

chunkable_item(Item) = Chunkable :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        Chunkable = chunkable_module_defn(ModuleDefn)
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        Chunkable = chunkable_pragma_type(Pragma)
    ;
        ( Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_nothing(_)
        ),
        Chunkable = yes
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_mutable(_)
        ),
        Chunkable = no
    ).

:- func chunkable_module_defn(module_defn) = bool.

chunkable_module_defn(ModuleDefn) = Reorderable :-
    (
        ( ModuleDefn = md_export(_)
        ; ModuleDefn = md_external(_, _)
        ; ModuleDefn = md_import(_)
        ; ModuleDefn = md_use(_)
        ),
        Reorderable = yes
    ;
        ( ModuleDefn = md_abstract_imported
        ; ModuleDefn = md_implementation
        ; ModuleDefn = md_imported(_)
        ; ModuleDefn = md_include_module(_)
        ; ModuleDefn = md_interface
        ; ModuleDefn = md_implementation_but_exported_to_submodules
        ; ModuleDefn = md_opt_imported
        ; ModuleDefn = md_transitively_imported
        ; ModuleDefn = md_used(_)
        ; ModuleDefn = md_version_numbers(_, _)
        ),
        Reorderable = no
    ).

:- func chunkable_pragma_type(pragma_type) = bool.

chunkable_pragma_type(Pragma) = Chunkable :-
    (
        ( Pragma = pragma_check_termination(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_foreign_proc_export(_)
        ; Pragma = pragma_foreign_export_enum(_)
        ; Pragma = pragma_foreign_enum(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_obsolete(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_reserve_tag(_)
        ; Pragma = pragma_oisu(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_require_feature_set(_)
        ),
        Chunkable = yes
    ;
        ( Pragma = pragma_exceptions(_)
        ; Pragma = pragma_fact_table(_)
        ; Pragma = pragma_foreign_code(_)
        ; Pragma = pragma_foreign_decl(_)
        ; Pragma = pragma_foreign_import_module(_)
        ; Pragma = pragma_foreign_proc(_)
        ; Pragma = pragma_source_file(_)
        ; Pragma = pragma_termination2_info(_)
        ),
        Chunkable = no
    ).

    % Given a list of items for which symname_ordered succeeds, we need to keep
    % the relative order of the items with the same sym_name as returned by
    % symname_ordered, but the relative order of items with different sym_names
    % doesn't matter.
    %
:- pred symname_ordered(item::in, sym_name::out) is semidet.

symname_ordered(Item, Name) :-
    (
        Item = item_pred_decl(ItemPredDecl),
        Name = ItemPredDecl ^ pf_name
    ;
        Item = item_mode_decl(ItemModeDecl),
        Name = ItemModeDecl ^ pfm_name
    ).

:- pred symname_orderable(item::in) is semidet.

symname_orderable(Item) :-
    symname_ordered(Item, _).

:- pred compare_by_symname(item::in, item::in, comparison_result::out) is det.

compare_by_symname(ItemA, ItemB, Result) :-
    (
        symname_ordered(ItemA, SymNameA),
        symname_ordered(ItemB, SymNameB)
    ->
        compare(Result, SymNameA, SymNameB)
    ;
        unexpected($module, $pred, "symname not found")
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.write_module_interface_files.
%-----------------------------------------------------------------------------%
