%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modules.m.
% Main author: fjh.
%
% This module contains all the code for handling module imports and exports,
% for computing module dependencies, and for generating makefile fragments to
% record those dependencies.
%
% The interface system works as follows:
%
% 1. a .int3 file is written, which contains all the types, typeclasses, insts
% and modes defined in the interface. Equivalence types, solver types, insts
% and modes are written in full, others are written in abstract form.  These
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
% It is used when compiling sub-modules.  The datestamp on the .date0
% file gives the last time the .int0 file was checked.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.modules.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module assoc_list.
:- import_module bool.
:- import_module digraph.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % make_private_interface(Globals, SourceFileName, SourceFileModuleName,
    %   ModuleName, MaybeTimestamp, Items):
    %
    % Given a source file name and module name, the timestamp of the source
    % file, and the list of items in that module, output the private (`.int0')
    % interface file for the module. (The private interface contains all the
    % declarations in the module, including those in the `implementation'
    % section; it is used when compiling sub-modules.)
    %
:- pred make_private_interface(globals::in, file_name::in,
    module_name::in, module_name::in, maybe(timestamp)::in, list(item)::in,
    io::di, io::uo) is det.

    % make_interface(Globals, SourceFileName, SourceFileModuleName,
    %   ModuleName, MaybeTimestamp, Items):
    %
    % Given a source file name and module name, the timestamp of the source
    % file, and the list of items in that module, output the long (`.int')
    % and short (`.int2') interface files for the module.
    %
:- pred make_interface(globals::in, file_name::in,
    module_name::in, module_name::in, maybe(timestamp)::in, list(item)::in,
    io::di, io::uo) is det.

    % Output the unqualified short interface file to <module>.int3.
    %
:- pred make_short_interface(globals::in, file_name::in, module_name::in,
    list(item)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Make an item for a module declaration or pseudo-declaration
    % such as `:- imported' (which is inserted by the compiler, but can't be
    % used in user code).
    %
:- func make_pseudo_decl(module_defn) = item.

    % append_pseudo_decl(PseudoDecl, Module0, Module):
    %
    % Append the specified module declaration to the list of items in Module0
    % to give Module.
    %
:- pred append_pseudo_decl(module_defn::in,
    module_and_imports::in, module_and_imports::out) is det.

    % replace_section_decls(IntStatusItem, ImpStatusItem, !Items):
    %
    % Replace all occurrences of `:- interface' with IntStatusItem
    % (this will usually be an item which sets the import status).
    % Replace all occurrences of `:- implementation' with ImpStatusItem.
    %
:- pred replace_section_decls(item::in, item::in,
    list(item)::in, list(item)::out) is det.

    % Remove all the imported items the list.
    %
:- pred strip_imported_items(list(item)::in, list(item)::out) is det.

%-----------------------------------------------------------------------------%

:- type module_list == assoc_list(module_name, list(item)).

    % Given a module (well, a list of items), split it into its constituent
    % sub-modules, in top-down order.
    % Also do some error checking:
    % - report an error if the `implementation' section of a sub-module
    %   is contained inside the `interface' section of its parent module
    % - check for modules declared as both nested and separate sub-modules.
    % - check for non-abstract typeclass instance declarations in module
    %   interfaces.
    %
:- pred split_into_submodules(module_name::in, list(item)::in,
    module_list::out, list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

    % grab_imported_modules(Globals, SourceFileName, SourceFileModuleName,
    %   ModuleName, NestedSubModules, HaveReadModuleMap, ModuleTimestamp,
    %   Items, Module, !IO):
    %
    % Given a source file name and the top-level module name in that file,
    % the current module name, the nested sub-modules in the file if this
    % module is the top-level module, the timestamp of the file SourceFileName
    % and the list of items in the current module, read in the private
    % interface files for all the parent modules, the long interface files
    % for all the imported modules, and the short interface files for all
    % the indirectly imported modules, and return a `module_and_imports'
    % structure containing the relevant information. HaveReadModuleMap contains
    % the interface files read during recompilation checking.
    %
:- pred grab_imported_modules(globals::in, file_name::in,
    module_name::in, module_name::in, list(module_name)::in,
    have_read_module_map::in, maybe(timestamp)::in, list(item)::in,
    module_and_imports::out, io::di, io::uo) is det.

    % grab_unqual_imported_modules(Globals, SourceFileName,
    %   SourceFileModuleName, ModuleName, Items, Module, !IO):
    %
    % Similar to grab_imported_modules, but only reads in the unqualified
    % short interfaces (.int3s), and the .int0 files for parent modules,
    % instead of reading the long interfaces and qualified short interfaces
    % (.int and int2s). Does not set the `PublicChildren', `FactDeps'
    % `ForeignIncludeFiles' fields of the module_and_imports structure.
    %
:- pred grab_unqual_imported_modules(globals::in, file_name::in,
    module_name::in, module_name::in, list(item)::in, module_and_imports::out,
    io::di, io::uo) is det.

    % process_module_private_interfaces(Globals, Ancestors,
    %   IntStatusItem, ImpStatusItem, !DirectImports, !DirectUses,
    %   !Module, !IO):
    %
    % Read the complete private interfaces for modules in Ancestors, and
    % append any imports/uses in the ancestors to the corresponding previous
    % lists.
    %
:- pred process_module_private_interfaces(globals::in,
    have_read_module_map::in, list(module_name)::in, item::in, item::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

    % process_module_long_interfaces(Globals, HaveReadModuleMap, NeedQualifier,
    %   Imports, Ext, IntStatusItem, ImpStatusItem,
    %   !IndirectImports, !ImplIndirectImports, !Module, !IO):
    %
    % Read the long interfaces for modules in Imports (unless they've already
    % been read in) from files with filename extension Ext, and append any
    % imports/uses in those modules to the IndirectImports list, and append
    % any imports/uses in the implementation of those modules to the
    % ImplIndirectImports list. Replace the `:- interface' declarations with
    % IntStatusItem, which should set the import_status of the following items.
    % Replace the `:- implementation' declarations with ImpStatusItem, which
    % should set the import_status of the following items.
    %
:- pred process_module_long_interfaces(globals::in, have_read_module_map::in,
    need_qualifier::in, list(module_name)::in, string::in, item::in, item::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
    %   IndirectImports, Ext, IntStatusItem, ImpStatusItem,
    %   !ImpIndirectImports, !Module):
    %
    % Read the short interfaces for modules in IndirectImports (unless they've
    % already been read in) and any modules that those modules import
    % (transitively) in the interface.
    %
    % Replace the `:- interface' declarations with IntStatusItem, which
    % should set the import_status of the following items. Replace the
    % `:- implementation' declarations with ImpStatusItem, which should set
    % the import_status of the following items.
    %
:- pred process_module_short_interfaces_transitively(globals::in,
    have_read_module_map::in, list(module_name)::in, string::in,
    item::in, item::in, list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces_and_impls_transitively(Globals,
    %   HaveReadModuleMap, IndirectImports, Ext, IntStatusItem, ImpStatusItem,
    %   !Module):
    %
    % Read the short interfaces for modules in IndirectImports (unless they've
    % already been read in) and any modules that those modules import
    % (transitively) in the interface or implementation.
    %
    % Replace the `:- interface' declarations with IntStatusItem, which
    % should set the import_status of the following items.
    % Replace the `:- implementation' declarations with ImpStatusItem,
    % which should set the import_status of the following items.
    %
:- pred process_module_short_interfaces_and_impls_transitively(globals::in,
    have_read_module_map::in, list(module_name)::in, string::in,
    item::in, item::in, module_and_imports::in, module_and_imports::out,
    io::di, io::uo) is det.

    % process_module_short_interfaces(Globals, HaveReadModuleMap,
    %   IntStatusItem, ImpStatusItem, Modules, Ext,
    %   !IndirectImports, !ImpIndirectImports, !Module):
    %
    % Read the short interfaces for modules in Modules (unless they've already
    % been read in). Append the modules imported by the interface of Modules to
    % !IndirectImports. Append the modules imported by the implementation of
    % Modules to !ImpIndirectImports.
    %
    % Replace the `:- interface' declarations with IntStatusItem, which should
    % set the import_status of the following items. Replace the
    % `:- implementation' declarations with ImpStatusItem, which should set
    % the import_status of the following items.
    %
:- pred process_module_short_interfaces(globals::in, have_read_module_map::in,
    list(module_name)::in, string::in, item::in, item::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_and_imports::in, module_and_imports::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % maybe_read_dependency_file(Globals, ModuleName, MaybeTransOptDeps, !IO):
    %
    % If transitive intermodule optimization has been enabled, then read
    % <ModuleName>.d to find the modules which <ModuleName>.trans_opt may
    % depend on.  Otherwise return `no'.
    %
:- pred maybe_read_dependency_file(globals::in, module_name::in,
    maybe(list(module_name))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % generate_module_dependencies(Globals, ModuleName, !IO):
    %
    % Generate the per-program makefile dependencies (`.dep') file for a
    % program whose top-level module is `ModuleName'. This involves first
    % transitively reading in all imported or ancestor modules. While we're
    % at it, we also save the per-module makefile dependency (`.d') files
    % for all those modules.
    %
:- pred generate_module_dependencies(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_file_dependencies(Globals, FileName, !IO):
    %
    % Same as generate_module_dependencies, but takes a file name instead of
    % a module name.
    %
:- pred generate_file_dependencies(globals::in, file_name::in,
    io::di, io::uo) is det.

    % generate_module_dependency_file(Globals, ModuleName, !IO):
    %
    % Generate the per module makefile dependency ('.d') file for the
    % given module.
    %
:- pred generate_module_dependency_file(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_file_dependency_file(Globals, FileName, !IO):
    %
    % Same as generate_module_dependency_file, but takes a file name instead of
    % a module name.
    %
:- pred generate_file_dependency_file(globals::in, file_name::in,
    io::di, io::uo) is det.

    % add_module_relations(LookupModuleImports, ModuleName,
    %   !IntDepsRel, !ImplDepsRel)
    %
    % Add a module's interface and implementation dependencies to IntDepsRel
    % and ImplDepsRel respectively.  Dependencies are found using the
    % LookupModuleImports function.
    %
:- pred add_module_relations(
    lookup_module_and_imports::lookup_module_and_imports,
    module_name::in, digraph(module_name)::in, digraph(module_name)::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

:- type lookup_module_and_imports == (func(module_name) = module_and_imports).
:- mode lookup_module_and_imports == in(func(in) = out is det).

%-----------------------------------------------------------------------------%
%
% These predicates are exported for use by module_imports.m.
%
% XXX They shouldn't be needed; the representation of the program should have
% all this information readily accessible.

    % get_children(Items, IncludeDeps):
    %
    % IncludeDeps is the list of sub-modules declared with `:- include_module'
    % in Items.
    %
:- pred get_children(list(item)::in, list(module_name)::out) is det.

    % Given a module (well, a list of items), extract the interface
    % part of that module, i.e. all the items between `:- interface'
    % and `:- implementation'.
    % The bodies of instance definitions are removed because
    % the instance methods have not yet been module qualified.
    %
:- pred get_interface(module_name::in, bool::in,
    list(item)::in, list(item)::out) is det.

:- pred get_foreign_self_imports(list(item)::in, list(foreign_language)::out)
    is det.

%-----------------------------------------------------------------------------%

    % Check whether a particular `pragma' declaration is allowed
    % in the interface section of a module.
    %
:- func pragma_allowed_in_interface(pragma_type) = bool.

    % Given a module name and a list of the items in that module,
    % this procedure checks if the module doesn't export anything,
    % and if so, and --warn-nothing-exported is set, it reports
    % a warning.
    %
:- pred check_for_no_exports(globals::in, list(item)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.write_deps_file.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module char.
:- import_module cord.
:- import_module dir.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module string.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%
%
% Private interfaces (.int0 files)
%

    % Read in the .int3 files that the current module depends on, and use
    % these to qualify all the declarations as much as possible. Then write
    % out the .int0 file.
    %
make_private_interface(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, MaybeTimestamp, Items0, !IO) :-
    grab_unqual_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, Items0, Module, !IO),

    % Check whether we succeeded.
    % XXX zs: why is fatal_module_errors with no_module_errors instead of
    % some_module_errors?
    module_and_imports_get_results(Module, Items1, Specs0, Error),
    (
        Error = some_module_errors,
        module_name_to_file_name(Globals, ModuleName, ".int0",
            do_not_create_dirs, FileName, !IO),
        % XXX _NumErrors
        write_error_specs(Specs0, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        io.write_strings(["Error reading interface files.\n",
            "`", FileName, "' not written.\n"], !IO)
    ;
        ( Error = no_module_errors
        ; Error = fatal_module_errors
        ),
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
            some [!IntItems, !ImplItems] (
                list.foldl3(strip_clauses_private_interface, Items3,
                    section_interface, _Section,
                    [], !:IntItems, [], !:ImplItems),
                handle_mutables_in_private_interface(ModuleName, !IntItems),
                handle_mutables_in_private_interface(ModuleName, !ImplItems),
                list.map(make_any_instances_abstract, !IntItems),
                list.map(make_any_instances_abstract, !ImplItems),
                order_items(!IntItems),
                order_items(!ImplItems),
                Items4 = [make_pseudo_decl(md_interface) | !.IntItems],
                (
                    !.ImplItems = [],
                    Items = Items4
                ;
                    !.ImplItems = [_ | _],
                    Items = Items4 ++
                        [make_pseudo_decl(md_implementation) | !.ImplItems]
                )
            ),
            write_interface_file(Globals, SourceFileName, ModuleName,
                ".int0", MaybeTimestamp, Items, !IO),
            touch_interface_datestamp(Globals, ModuleName, ".date0", !IO)
        )
    ).

:- pred make_any_instances_abstract(item::in, item::out) is det.

make_any_instances_abstract(Item0, Item) :-
    ( Item0 = item_instance(InstanceInfo0) ->
        InstanceInfo = make_instance_abstract(InstanceInfo0),
        Item = item_instance(InstanceInfo)
    ;
        Item = Item0
    ).

    % Expand any mutable declarations in the item list into the pred and mode
    % declarations for their access predicates.  Only these components of a
    % mutable declaration should be written to a private interface file.
    %
:- pred handle_mutables_in_private_interface(module_name::in,
    list(item)::in, list(item)::out) is det.

handle_mutables_in_private_interface(ModuleName, !Items) :-
    list.foldl(handle_mutable_in_private_interface(ModuleName), !.Items,
        [], !:Items).

:- pred handle_mutable_in_private_interface(module_name::in,
    item::in, list(item)::in, list(item)::out) is det.

handle_mutable_in_private_interface(ModuleName, Item, !Items) :-
    ( Item = item_mutable(ItemMutable) ->
        ItemMutable = item_mutable_info(MutableName, Type, _Value, Inst, Attrs,
            _Varset, Context, _SeqNum),
        ConstantInterface = mutable_var_constant(Attrs),
        (
            ConstantInterface = yes,
            ConstantGetPredDeclItem = constant_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            ConstantSetPredDeclItem = constant_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            list.cons(ConstantGetPredDeclItem, !Items),
            list.cons(ConstantSetPredDeclItem, !Items)
        ;
            ConstantInterface = no,
            StdGetPredDeclItem = std_get_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            StdSetPredDeclItem = std_set_pred_decl(ModuleName,
                MutableName, Type, Inst, Context),
            list.cons(StdGetPredDeclItem, !Items),
            list.cons(StdSetPredDeclItem, !Items),
            IOStateInterface = mutable_var_attach_to_io_state(Attrs),
            (
                IOStateInterface = yes,
                PureGetPredDeclItem = io_get_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                PureSetPredDeclItem = io_set_pred_decl(ModuleName,
                    MutableName, Type, Inst, Context),
                list.cons(PureGetPredDeclItem, !Items),
                list.cons(PureSetPredDeclItem, !Items)
            ;
                IOStateInterface = no
            )
        )
    ;
        list.cons(Item, !Items)
    ).

%-----------------------------------------------------------------------------%

    % Read in the .int3 files that the current module depends on, and use these
    % to qualify all items in the interface as much as possible. Then write out
    % the .int and .int2 files.
    %
make_interface(Globals, SourceFileName, SourceFileModuleName, ModuleName,
        MaybeTimestamp, Items0, !IO) :-
    some [!InterfaceItems] (
        get_interface(ModuleName, yes, Items0, !:InterfaceItems),

        % Get the .int3 files for imported modules.
        grab_unqual_imported_modules(Globals, SourceFileName,
            SourceFileModuleName, ModuleName, !.InterfaceItems, Module0, !IO),

        % Check whether we succeeded.
        module_and_imports_get_results(Module0, !:InterfaceItems,
            Specs0, Error),
        % XXX zs: why is fatal_module_errors with no_module_errors instead of
        % some_module_errors?
        (
            Error = some_module_errors,
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
        ;
            ( Error = no_module_errors
            ; Error = fatal_module_errors
            ),
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
            ( NumErrors > 0 ->
                module_name_to_file_name(Globals, ModuleName, ".int",
                    do_not_create_dirs, IntFileName, !IO),
                io.write_strings(["`", IntFileName, "' ", "not written.\n"],
                    !IO)
            ;
                % Strip out the imported interfaces, assertions are also
                % stripped since they should only be written to .opt files,
                % check for some warnings, and then write out the `.int'
                % and `int2' files and touch the `.date' file.

                strip_imported_items(!InterfaceItems),
                strip_assertions(!InterfaceItems),
                strip_unnecessary_impl_defns(!InterfaceItems),
                check_for_clauses_in_interface(!InterfaceItems, [],
                    InterfaceSpecs0),
                check_int_for_no_exports(Globals, !.InterfaceItems, ModuleName,
                    InterfaceSpecs0, InterfaceSpecs, !IO),
                write_error_specs(InterfaceSpecs, Globals,
                    0, _NumWarnings2, 0, _NumErrors2, !IO),
                % XXX _NumErrors
                order_items(!InterfaceItems),
                write_interface_file(Globals, SourceFileName, ModuleName,
                    ".int", MaybeTimestamp, !.InterfaceItems, !IO),
                get_short_interface(!.InterfaceItems, int2,
                    ShortInterfaceItems),
                write_interface_file(Globals, SourceFileName, ModuleName,
                    ".int2", MaybeTimestamp, ShortInterfaceItems, !IO),
                touch_interface_datestamp(Globals, ModuleName, ".date", !IO)
            )
        )
    ).

make_short_interface(Globals, SourceFileName, ModuleName, Items0, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.

    some [!Specs] (
        !:Specs = [],
        get_interface(ModuleName, no, Items0, InterfaceItems0),
        % Assertions are also stripped since they should only be written
        % to .opt files.
        strip_assertions(InterfaceItems0, InterfaceItems1),
        check_for_clauses_in_interface(InterfaceItems1, InterfaceItems,
            !Specs),
        get_short_interface(InterfaceItems, int3, ShortInterfaceItems0),
        module_qualify_items(ShortInterfaceItems0, ShortInterfaceItems,
            map.init, _, Globals, ModuleName, no, "", _, _, _, !Specs),
        % XXX _NumErrors
        write_error_specs(!.Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        % XXX why do we do this even if there are some errors?
        write_interface_file(Globals, SourceFileName, ModuleName, ".int3",
            no, ShortInterfaceItems, !IO),
        touch_interface_datestamp(Globals, ModuleName, ".date3", !IO)
    ).

%-----------------------------------------------------------------------------%

strip_imported_items(Items0, Items) :-
    strip_imported_items_2(Items0, [], RevItems),
    list.reverse(RevItems, Items).

:- pred strip_imported_items_2(list(item)::in, list(item)::in, list(item)::out)
    is det.

strip_imported_items_2([], !RevItems).
strip_imported_items_2([Item | Items], !RevItems) :-
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
            strip_imported_items_2(Items, !RevItems)
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
        strip_imported_items_2(Items, !RevItems)
    ).

:- pred strip_assertions(list(item)::in, list(item)::out) is det.

strip_assertions([], []).
strip_assertions([Head | Tail], Items) :-
    (
        Head = item_promise(ItemPromise),
        ItemPromise = item_promise_info(promise_type_true, _, _, _, _, _)
    ->
        strip_assertions(Tail, Items)
    ;
        strip_assertions(Tail, ItemsTail),
        Items = [Head | ItemsTail]
    ).

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
        % in the implementation.  Specifically, we require that ones
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
        % XXX If the above exception is thrown and you need a
        % workaround you can replace the call to unexpected with this code:
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
    % a single module.  (This should be the case, see prog_io.m.)
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
        % There must exist a foreign type alternative to this type.  As the du
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
    ( sym_name_get_module_name(SymName, ModuleName) ->
        set.insert(ModuleName, !Modules)
    ;
        unexpected($module, $pred, "unknown type encountered")
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
    ( sym_name_get_module_name(ClassName, ModuleName) ->
        set.insert(ModuleName, !Modules)
    ;
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

:- pred check_for_clauses_in_interface(list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_clauses_in_interface([], [], !Specs).
check_for_clauses_in_interface([Item0 | Items0], Items, !Specs) :-
    (
        Item0 = item_clause(ItemClause0),
        Context = ItemClause0 ^ cl_context,
        Spec = clause_in_interface_warning("clause", Context),
        !:Specs = [Spec | !.Specs],
        check_for_clauses_in_interface(Items0, Items, !Specs)
    ;
        Item0 = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, Context, _),
        AllowedInInterface = pragma_allowed_in_interface(Pragma),
        (
            AllowedInInterface = no,
            Spec = clause_in_interface_warning("pragma", Context),
            !:Specs = [Spec | !.Specs],
            check_for_clauses_in_interface(Items0, Items, !Specs)
        ;
            AllowedInInterface = yes,
            check_for_clauses_in_interface(Items0, Items1, !Specs),
            Items = [Item0 | Items1]
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
        check_for_clauses_in_interface(Items0, Items1, !Specs),
        Items = [Item0 | Items1]
    ).

:- func clause_in_interface_warning(string, prog_context) = error_spec.

clause_in_interface_warning(ClauseOrPragma, Context) = Spec :-
    Pieces = [words("Warning:"), words(ClauseOrPragma),
        words("in module interface.")],
    Spec = error_spec(severity_warning, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

    % strip_clauses_private_interface is used when creating the private
    % interface (`.int0') files for packages with sub-modules. It removes
    % unnecessary items and separates interface and implementation items.
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
    % We treat initialise and finalise declarations as special kinds of
    % clause, since they should always be grouped together with the clauses
    % and should not appear in private interfaces.
    %
:- pred strip_clauses_private_interface(item::in, section::in, section::out,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out) is det.

strip_clauses_private_interface(Item, !Section, !InterfaceItems, !ImplItems) :-
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
                list.cons(Item, !InterfaceItems),
                list.cons(Item, !ImplItems)
            ;
                !.Section = section_implementation,
                list.cons(Item, !ImplItems)
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
        (
            !.Section = section_interface,
            list.cons(Item, !InterfaceItems)
        ;
            !.Section = section_implementation,
            list.cons(Item, !ImplItems)
        )
    ).

:- pred split_clauses_and_decls(list(item)::in,
    list(item)::out, list(item)::out) is det.

split_clauses_and_decls([], [], []).
split_clauses_and_decls([Item | Items], !:ClauseItems, !:InterfaceItems) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ( ModuleDefn = md_interface
        ; ModuleDefn = md_implementation
        )
    ->
        split_clauses_and_decls(Items, !:ClauseItems, !:InterfaceItems)
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
        split_clauses_and_decls(Items, !:ClauseItems, !:InterfaceItems),
        !:ClauseItems = [Item | !.ClauseItems]
    ;
        split_clauses_and_decls(Items, !:ClauseItems, !:InterfaceItems),
        !:InterfaceItems = [Item | !.InterfaceItems]
    ).

pragma_allowed_in_interface(Pragma) = Allowed :-
    % XXX This comment is out of date.
    % pragma `obsolete', `terminates', `does_not_terminate'
    % `termination_info', `check_termination', `reserve_tag' and
    % `foreign_enum' pragma declarations are supposed to go in the
    % interface, but all other pragma declarations are implementation details
    % only, and should go in the implementation.

    (
        ( Pragma = pragma_foreign_code(_)
        ; Pragma = pragma_foreign_decl(_)
        ; Pragma = pragma_foreign_proc_export(_)
        ; Pragma = pragma_foreign_export_enum(_)
        ; Pragma = pragma_foreign_proc(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_fact_table(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_require_feature_set(_)
        ),
        Allowed = no
    ;
        % Note that the parser will strip out `source_file' pragmas anyway,
        % and that `reserve_tag' and `direct_arg' must be in the interface iff
        % the corresponding type definition is in the interface. This is
        % checked in make_hlds.
        ( Pragma = pragma_foreign_enum(_)
        ; Pragma = pragma_foreign_import_module(_)
        ; Pragma = pragma_obsolete(_)
        ; Pragma = pragma_source_file(_)
        ; Pragma = pragma_reserve_tag(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_check_termination(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_oisu(_)
        ),
        Allowed = yes
    ).

check_for_no_exports(Globals, Items, ModuleName, !Specs, !IO) :-
    globals.lookup_bool_option(Globals, warn_nothing_exported, ExportWarning),
    (
        ExportWarning = no
    ;
        ExportWarning = yes,
        get_interface(ModuleName, no, Items, InterfaceItems),
        check_int_for_no_exports(Globals, InterfaceItems, ModuleName,
            !Specs, !IO)
    ).

    % Given a module name and a list of the items in that module's interface,
    % this procedure checks if the module doesn't export anything, and if so,
    % and --warn-nothing-exported is set, it returns a warning.
    %
:- pred check_int_for_no_exports(globals::in, list(item)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_int_for_no_exports(Globals, [], ModuleName, !Specs, !IO) :-
    generate_no_exports_warning(Globals, ModuleName, WarnSpec, !IO),
    !:Specs = [WarnSpec | !.Specs].
check_int_for_no_exports(Globals, [Item | Items], ModuleName, !Specs, !IO) :-
    (
        (
            Item = item_nothing(_)
        ;
            Item = item_module_defn(ItemModuleDefn),
            ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
            ModuleDefn \= md_include_module(_)
        )
    ->
        % Nothing useful - keep searching.
        check_int_for_no_exports(Globals, Items, ModuleName, !Specs, !IO)
    ;
        % We found something useful - don't issue the warning.
        true
    ).

:- pred generate_no_exports_warning(globals::in, module_name::in,
    error_spec::out, io::di, io::uo) is det.

generate_no_exports_warning(Globals, ModuleName, Spec, !IO) :-
    % XXX The FileName should be passed down to here; we shouldn't have to
    % compute it again.
    module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
        FileName, !IO),
    % XXX We should use the module declaration's context, not the arbitrary
    % line number 1.
    Context = context_init(FileName, 1),
    Severity = severity_conditional(warn_nothing_exported, yes,
        severity_warning, no),
    Component = option_is_set(warn_nothing_exported, yes,
        [always([words("Warning: interface for module"),
            sym_name(ModuleName), words("does not export anything.")]),
        verbose_only(
            [words("To be useful, a module should export something."),
            words("A file should contain at least one declaration"),
            words("other than"), fixed("`:- import_module'"),
            words("in its interface section(s)."),
            words("This would normally be a"),
            fixed("`:- pred',"), fixed("`:- func',"),
            fixed("`:- type',"), fixed("`:- inst'"),
            fixed("or `:- mode'"), words("declaration.")])
        ]),
    Msg = simple_msg(Context, [Component]),
    Spec = error_spec(Severity, phase_term_to_parse_tree, [Msg]).

%-----------------------------------------------------------------------------%

:- pred write_interface_file(globals::in, file_name::in, module_name::in,
    string::in, maybe(timestamp)::in, list(item)::in, io::di, io::uo) is det.

write_interface_file(Globals, _SourceFileName, ModuleName, Suffix,
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
    (
        GenerateVersionNumbers = yes,
        DisableVersionNumbers = no
    ->
        % Find the timestamp of the current module.
        (
            MaybeTimestamp = yes(Timestamp),

            % Read in the previous version of the file.
            read_module_ignore_errors(NoLineNumGlobals, ModuleName, Suffix,
                "Reading old interface for module",
                do_search, do_not_return_timestamp, OldItems, OldError,
                _OldIntFileName, _OldTimestamp, !IO),
            (
                OldError = no_module_errors,
                MaybeOldItems = yes(OldItems)
            ;
                ( OldError = some_module_errors
                ; OldError = fatal_module_errors
                ),
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
            (
                InterfaceItems0 = [FirstItem | InterfaceItems1],
                FirstItem = item_module_defn(FirstItemModuleDefn),
                FirstItemModuleDefn =
                    item_module_defn_info(FirstModuleDefn, _, _),
                FirstModuleDefn = md_interface
            ->
                InterfaceItems = [FirstItem, VersionNumberItem
                    | InterfaceItems1]
            ;
                InterfaceItems = [make_pseudo_decl(md_interface),
                    VersionNumberItem | InterfaceItems0]
            )
        ;
            MaybeTimestamp = no,
            unexpected($module, $pred,
                "with `--smart-recompilation', timestamp not read")
        )
    ;
        InterfaceItems = InterfaceItems0
    ),
    convert_to_mercury(NoLineNumGlobals, ModuleName, TmpOutputFileName,
        InterfaceItems, !IO),
    % Start using the original globals again.
    update_interface(Globals, OutputFileName, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

grab_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, NestedChildren, HaveReadModuleMap, MaybeTimestamp, Items0,
        !:Module, !IO) :-
    % Find out which modules this one depends on.
    AncestorModules = get_ancestors(ModuleName),
    get_dependencies_int_imp(Items0, IntImportedModules0, IntUsedModules0,
        ImpImportedModules0, ImpUsedModules0),

    ImportedModules0 = IntImportedModules0 ++ ImpImportedModules0,
    UsedModules0 = IntUsedModules0 ++ ImpUsedModules0,

    some [!Specs] (
        !:Specs = [],

        module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
            FileName, !IO),
        warn_if_import_self_or_ancestor(ModuleName, FileName, AncestorModules,
            ImportedModules0, UsedModules0, !Specs),

        warn_if_duplicate_use_import_decls(ModuleName, FileName,
            IntImportedModules0, IntImportedModules1,
            IntUsedModules0, IntUsedModules1,
            ImpImportedModules0, ImpImportedModules,
            ImpUsedModules0, ImpUsedModules, !Specs),

        get_interface_and_implementation(ModuleName, no, Items0,
            InterfaceItems, ImplItems),
        get_children(InterfaceItems, PublicChildren),
        get_fact_table_dependencies(Items0, FactDeps),
        get_foreign_include_files(Items0, ForeignIncludeFiles),
        (
            MaybeTimestamp = yes(Timestamp),
            MaybeTimestamps = yes(map.singleton(ModuleName,
                module_timestamp(".m", Timestamp, may_be_unqualified)))
        ;
            MaybeTimestamp = no,
            MaybeTimestamps = no
        ),
        init_module_and_imports(SourceFileName, SourceFileModuleName,
            ModuleName, Items0, !.Specs, PublicChildren, NestedChildren,
            FactDeps, ForeignIncludeFiles, MaybeTimestamps, !:Module),

        % If this module has any separately-compiled sub-modules, then
        % we need to make everything in the implementation of this module
        % exported_to_submodules.  We do that by splitting out the
        % implementation declarations and putting them in a special
        % `implementation_but_exported_to_submodules' section.

        get_children(Items0, Children),
        (
            Children = [],
            Items1 = Items0
        ;
            Children = [_ | _],
            split_clauses_and_decls(ImplItems, Clauses, ImplDecls),
            Items1 =
                [make_pseudo_decl(md_interface) | InterfaceItems] ++
                [make_pseudo_decl(md_implementation_but_exported_to_submodules)
                    | ImplDecls] ++
                [make_pseudo_decl(md_implementation) | Clauses],
            !Module ^ mai_items_cord := cord.from_list(Items1)
        ),

        % Add `builtin' and `private_builtin' to the list of imported modules.
        add_implicit_imports(Items1, Globals,
            IntImportedModules1, IntImportedModules2,
            IntUsedModules1, IntUsedModules2),

        % Process the ancestor modules.
        %
        % Uses of the items declared in ancestor modules do not need
        % module qualifiers. Modules imported by ancestors are considered
        % to be visible in the current module.
        process_module_private_interfaces(Globals, HaveReadModuleMap,
            AncestorModules,
            make_pseudo_decl(md_imported(import_locn_interface)),
            make_pseudo_decl(md_imported(
                import_locn_ancestor_private_interface_proper)),
            IntImportedModules2, IntImportedModules,
            IntUsedModules2, IntUsedModules, !Module, !IO),

        % Process the modules imported using `import_module'.
        % Uses of these items do not need module qualifiers.
        IntIndirectImports0 = [],
        IntImpIndirectImports0 = [],
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            may_be_unqualified, IntImportedModules, ".int",
            make_pseudo_decl(md_imported(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntIndirectImports0, IntIndirectImports1,
            IntImpIndirectImports0, IntImpIndirectImports1,
            !Module, !IO),

        ImpIndirectImports0 = [],
        ImpImpIndirectImports0 = [],
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            may_be_unqualified, ImpImportedModules, ".int",
            make_pseudo_decl(md_imported(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpIndirectImports0, ImpIndirectImports1,
            ImpImpIndirectImports0, ImpImpIndirectImports1,
            !Module, !IO),

        % Process the modules imported using `use_module' .
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            must_be_qualified, IntUsedModules, ".int",
            make_pseudo_decl(md_used(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntIndirectImports1, IntIndirectImports,
            IntImpIndirectImports1, IntImpIndirectImports2,
            !Module, !IO),
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            must_be_qualified, ImpUsedModules, ".int",
            make_pseudo_decl(md_used(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpIndirectImports1, ImpIndirectImports,
            ImpImpIndirectImports1, ImpImpIndirectImports2,
            !Module, !IO),

        % Process the short interfaces for indirectly imported modules.
        % The short interfaces are treated as if they are imported
        % using `use_module'.
        append_pseudo_decl(md_transitively_imported, !Module),
        process_module_short_interfaces_transitively(Globals,
            HaveReadModuleMap, IntIndirectImports, ".int2",
            make_pseudo_decl(md_used(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntImpIndirectImports2, IntImpIndirectImports, !Module, !IO),
        process_module_short_interfaces_transitively(Globals,
            HaveReadModuleMap, ImpIndirectImports, ".int2",
            make_pseudo_decl(md_used(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpImpIndirectImports2, ImpImpIndirectImports, !Module, !IO),

        % Process the short interfaces for modules imported in the
        % implementation of indirectly imported modules. The items in these
        % modules shouldn't be visible to typechecking -- they are used for
        % fully expanding equivalence types after the semantic checking passes.
        process_module_short_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMap, IntImpIndirectImports, ".int2",
            make_pseudo_decl(md_abstract_imported),
            make_pseudo_decl(md_abstract_imported),
            !Module, !IO),
        process_module_short_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMap, ImpImpIndirectImports, ".int2",
            make_pseudo_decl(md_abstract_imported),
            make_pseudo_decl(md_abstract_imported),
            !Module, !IO),

        module_and_imports_get_results(!.Module, Items, _, _),
        check_imports_accessibility(ModuleName,
            IntImportedModules ++ IntUsedModules ++
            ImpImportedModules ++ ImpUsedModules, Items, [], AccessSpecs),
        module_and_imports_add_specs(AccessSpecs, !Module)
    ).

    % grab_unqual_imported_modules:
    %
    % Like grab_imported_modules, but gets the `.int3' files
    % instead of the `.int' and `.int2' files.
    %
grab_unqual_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, Items0, !:Module, !IO) :-
    % Find out which modules this one depends on.
    ParentDeps = get_ancestors(ModuleName),
    get_dependencies_int_imp(Items0, IntImportDeps0, IntUseDeps0,
        ImpImportDeps, ImpUseDeps),

    % Construct the initial module import structure.
    init_module_and_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, [], [], [], [], [], no, !:Module),

    % Add `builtin' and `private_builtin' to the imported modules.
    add_implicit_imports(Items0, Globals,
        IntImportDeps0, IntImportDeps, IntUseDeps0, IntUseDeps),

    % Get the .int3s and .int0s that the current module depends on.
    map.init(HaveReadModuleMap),

    % First the .int0s for parent modules.
    process_module_private_interfaces(Globals, HaveReadModuleMap, ParentDeps,
        make_pseudo_decl(md_imported(import_locn_interface)),
        make_pseudo_decl(md_imported(
            import_locn_ancestor_private_interface_proper)),
        [], ParentImportDeps, [], ParentUseDeps, !Module, !IO),

    % Then the .int3s for `:- import'-ed modules.
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, ParentImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_ancestor)),
        make_pseudo_decl(md_abstract_imported),
        [], IntIndirectImportDeps0, [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, IntImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps0, IntIndirectImportDeps1,
        [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, ImpImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        [], ImpIndirectImportDeps0,
        [], _, !Module, !IO),

    % Then (after appropriate `:- used' decls) the .int3s for `:- use'-ed
    % modules.
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        may_be_unqualified, ParentUseDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_ancestor)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps1, IntIndirectImportDeps2,
        [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        must_be_qualified, IntUseDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps2, IntIndirectImportDeps,
        [], _, !Module, !IO),
    process_module_long_interfaces(Globals, HaveReadModuleMap,
        must_be_qualified, ImpUseDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        ImpIndirectImportDeps0, ImpIndirectImportDeps,
        [], _, !Module, !IO),

    % Then (after appropriate `:- used' decl) the .int3s for indirectly
    % imported modules.
    process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        IntIndirectImportDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        [], _, !Module, !IO),

    process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        ImpIndirectImportDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        [], _, !Module, !IO),

    module_and_imports_get_results(!.Module, Items, _, _),
    check_imports_accessibility(ModuleName,
        IntImportDeps ++ IntUseDeps ++ ImpImportDeps ++ ImpUseDeps,
        Items, [], AccessSpecs),
    module_and_imports_add_specs(AccessSpecs, !Module).

%-----------------------------------------------------------------------------%

append_pseudo_decl(PseudoDecl, !Module) :-
    module_and_imports_add_items(cord.singleton(make_pseudo_decl(PseudoDecl)),
        !Module).

make_pseudo_decl(PseudoDecl) = Item :-
    ItemModuleDefn = item_module_defn_info(PseudoDecl, term.context_init, -1),
    Item = item_module_defn(ItemModuleDefn).

%-----------------------------------------------------------------------------%

    % Warn if a module imports itself, or an ancestor.
    %
:- pred warn_if_import_self_or_ancestor(module_name::in, string::in,
    list(module_name)::in, list(module_name)::in, list(module_name)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_import_self_or_ancestor(ModuleName, FileName, AncestorModules,
        ImportedModules, UsedModules, !Specs) :-
    IsImportedAncestor = (pred(Import::out) is nondet :-
        list.member(Import, AncestorModules),
        ( list.member(Import, ImportedModules)
        ; list.member(Import, UsedModules)
        )
    ),
    solutions.aggregate(IsImportedAncestor,
        warn_imported_ancestor(ModuleName, FileName), !Specs),
    (
        ( list.member(ModuleName, ImportedModules)
        ; list.member(ModuleName, UsedModules)
        )
    ->
        term.context_init(FileName, 1, Context),
        SelfPieces = [words("Warning: module"),
            sym_name(ModuleName), words("imports itself!")],
        SelfMsg = simple_msg(Context,
            [option_is_set(warn_simple_code, yes, [always(SelfPieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        SelfSpec = error_spec(Severity, phase_parse_tree_to_hlds,
            [SelfMsg]),
        !:Specs = [SelfSpec | !.Specs]
    ;
        true
    ).

:- pred warn_imported_ancestor(module_name::in, string::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_imported_ancestor(ModuleName, FileName, AncestorName, !Specs) :-
    term.context_init(FileName, 1, Context),
    MainPieces = [words("Module"), sym_name(ModuleName),
        words("imports its own ancestor, module"),
        sym_name(AncestorName), words(".")],
    VerbosePieces = [words("Every sub-module"),
        words("implicitly imports its ancestors."),
        words("There is no need to explicitly import them.")],
    Msg = simple_msg(Context,
        [option_is_set(warn_simple_code, yes,
            [always(MainPieces), verbose_only(VerbosePieces)])]),
    Severity = severity_conditional(warn_simple_code, yes,
        severity_warning, no),
    Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

    % This predicate ensures that all every import_module declaration is
    % checked against every use_module declaration, except for the case
    % where the interface has `:- use_module foo.' and the implementation
    % `:- import_module foo.'.
    %
:- pred warn_if_duplicate_use_import_decls(module_name::in, string::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_duplicate_use_import_decls(ModuleName, FileName,
        IntImportedModules0, IntImportedModules,
        IntUsedModules0, IntUsedModules,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules0, ImpUsedModules, !Specs) :-

    do_warn_if_duplicate_use_import_decls(ModuleName, FileName,
        IntImportedModules0, IntImportedModules1,
        IntUsedModules0, IntUsedModules, !Specs),
    do_warn_if_duplicate_use_import_decls(ModuleName, FileName,
        IntImportedModules1, IntImportedModules,
        ImpUsedModules0, ImpUsedModules1, !Specs),

    do_warn_if_duplicate_use_import_decls(ModuleName, FileName,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules1, ImpUsedModules, !Specs).

    % Report warnings for modules imported using both `:- use_module'
    % and `:- import_module'.  Remove the unnecessary `:- use_module'
    % declarations.
    %
:- pred do_warn_if_duplicate_use_import_decls(module_name::in, string::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_warn_if_duplicate_use_import_decls(_ModuleName, FileName,
        !ImportedModules, !UsedModules, !Specs) :-
    set.list_to_set(!.ImportedModules, ImportedSet),
    set.list_to_set(!.UsedModules, UsedSet),
    set.intersect(ImportedSet, UsedSet, BothSet),
    ( set.empty(BothSet) ->
        true
    ;
        set.to_sorted_list(BothSet, BothList),

        term.context_init(FileName, 1, Context),
        Pieces = [words("Warning:"),
            words(choose_number(BothList, "module", "modules"))] ++
            component_list_to_pieces(list.map(wrap_symname, BothList)) ++
            [words(choose_number(BothList, "is", "are")),
            words("imported using both `:- import_module'"),
            words("`:- use_module' declarations."), nl],
        Msg = simple_msg(Context,
            [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
        Severity = severity_conditional(warn_simple_code, yes,
            severity_warning, no),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],

        % Treat the modules with both types of import as if they
        % were imported using `:- import_module.'
        list.delete_elems(!.UsedModules, BothList, !:UsedModules)
    ).

:- func wrap_symname(module_name) = format_component.

wrap_symname(ModuleName) = sym_name(ModuleName).

%-----------------------------------------------------------------------------%

maybe_read_dependency_file(Globals, ModuleName, MaybeTransOptDeps, !IO) :-
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    (
        TransOpt = yes,
        globals.lookup_bool_option(Globals, verbose, Verbose),
        module_name_to_file_name(Globals, ModuleName, ".d", do_not_create_dirs,
            DependencyFileName, !IO),
        maybe_write_string(Verbose, "% Reading auto-dependency file `", !IO),
        maybe_write_string(Verbose, DependencyFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        io.open_input(DependencyFileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            io.set_input_stream(Stream, OldStream, !IO),
            module_name_to_file_name(Globals, ModuleName, ".trans_opt_date",
                do_not_create_dirs, TransOptDateFileName0, !IO),
            string.to_char_list(TransOptDateFileName0, TransOptDateFileName),
            SearchPattern = TransOptDateFileName ++ [' ', ':'],
            read_dependency_file_find_start(SearchPattern, FindResult, !IO),
            (
                FindResult = yes,
                read_dependency_file_get_modules(TransOptDeps, !IO),
                MaybeTransOptDeps = yes(TransOptDeps)
            ;
                FindResult = no,
                % error reading .d file
                MaybeTransOptDeps = no
            ),
            io.set_input_stream(OldStream, _, !IO),
            io.close_input(Stream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            OpenResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening file `", DependencyFileName,
                "' for input: ", IOErrorMessage], Message),
            report_error(Message, !IO),
            MaybeTransOptDeps = no
        )
    ;
        TransOpt = no,
        MaybeTransOptDeps = no
    ).

    % Read lines from the dependency file (module.d) until one is found
    % which begins with SearchPattern.
    %
:- pred read_dependency_file_find_start(list(char)::in, bool::out,
    io::di, io::uo) is det.

read_dependency_file_find_start(SearchPattern, Success, !IO) :-
    io.read_line(Result, !IO),
    ( Result = ok(CharList) ->
        ( list.append(SearchPattern, _, CharList) ->
            % Have found the start.
            Success = yes
        ;
            read_dependency_file_find_start(SearchPattern, Success, !IO)
        )
    ;
        Success = no
    ).

    % Read lines until one is found which does not contain whitespace
    % followed by a word which ends in .trans_opt.  Remove the .trans_opt
    % ending from all the words which are read in and return the resulting
    % list of modules.
    %
:- pred read_dependency_file_get_modules(list(module_name)::out,
    io::di, io::uo) is det.

read_dependency_file_get_modules(TransOptDeps, !IO) :-
    io.read_line(Result, !IO),
    (
        Result = ok(CharList0),
        % Remove any whitespace from the beginning of the line,
        % then take all characters until another whitespace occurs.
        list.takewhile(char.is_whitespace, CharList0, _, CharList1),
        NotIsWhitespace = (pred(Char::in) is semidet :-
            \+ char.is_whitespace(Char)
        ),
        list.takewhile(NotIsWhitespace, CharList1, CharList, _),
        string.from_char_list(CharList, FileName0),
        string.remove_suffix(FileName0, ".trans_opt", FileName)
    ->
        ( string.append("Mercury/trans_opts/", BaseFileName, FileName) ->
            ModuleFileName = BaseFileName
        ;
            ModuleFileName = FileName
        ),
        file_name_to_module_name(ModuleFileName, Module),
        read_dependency_file_get_modules(TransOptDeps0, !IO),
        TransOptDeps = [Module | TransOptDeps0]
    ;
        TransOptDeps = []
    ).

%-----------------------------------------------------------------------------%

generate_module_dependencies(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_file_dependencies(Globals, FileName, !IO) :-
    build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_module_dependency_file(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_d_file_only, do_search, ModuleName,
        DepsMap, !IO).

generate_file_dependency_file(Globals, FileName, !IO) :-
    build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO),
    generate_dependencies(Globals, output_d_file_only, do_search, ModuleName,
        DepsMap, !IO).

:- pred build_deps_map(globals::in, file_name::in,
    module_name::out, deps_map::out, io::di, io::uo) is det.

build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO) :-
    % Read in the top-level file (to figure out its module name).
    read_module_from_file(Globals, FileName, ".m", "Reading file",
        do_not_search, do_not_return_timestamp, Items, Specs0, Error,
        ModuleName, _, !IO),
    SourceFileName = FileName ++ ".m",
    split_into_submodules(ModuleName, Items, SubModuleList, Specs0, Specs),
    % XXX _NumErrors
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
    assoc_list.keys(SubModuleList, SubModuleNames),
    list.map(init_dependencies(SourceFileName, ModuleName, SubModuleNames,
        [], Error, Globals), SubModuleList, ModuleImportsList),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map, ModuleImportsList, DepsMap0, DepsMap).

:- type generate_dependencies_mode
    --->    output_d_file_only
    ;       output_all_dependencies.

:- pred generate_dependencies(globals::in, generate_dependencies_mode::in,
    maybe_search::in, module_name::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies(Globals, Mode, Search, ModuleName, DepsMap0, !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(Globals, ModuleName, Search, DepsMap0, DepsMap, !IO),

    % Check whether we could read the main `.m' file.

    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, ModuleImports),
    Error = ModuleImports ^ mai_error,
    (
        Error = fatal_module_errors,
        ModuleString = sym_name_to_string(ModuleName),
        string.append_list(["can't read source file for module `",
            ModuleString, "'."], Message),
        report_error(Message, !IO)
    ;
        ( Error = no_module_errors
        ; Error = some_module_errors
        ),
        (
            Mode = output_d_file_only
        ;
            Mode = output_all_dependencies,
            module_and_imports_get_source_file_name(ModuleImports,
                SourceFileName),
            generate_dependencies_write_dv_file(Globals, SourceFileName,
                ModuleName, DepsMap, !IO),
            generate_dependencies_write_dep_file(Globals, SourceFileName,
                ModuleName, DepsMap, !IO)
        ),

        % Compute the interface deps graph and the implementation deps
        % graph from the deps map.

        digraph.init(IntDepsGraph0),
        digraph.init(ImplDepsGraph0),
        map.values(DepsMap, DepsList),
        deps_list_to_deps_graph(DepsList, DepsMap, IntDepsGraph0, IntDepsGraph,
            ImplDepsGraph0, ImplDepsGraph),
        maybe_output_imports_graph(Globals, ModuleName,
            IntDepsGraph, ImplDepsGraph, !IO),

        % Compute the trans-opt deps ordering, by doing an approximate
        % topological sort of the implementation deps, and then finding
        % the subset of those for which of those we have (or can make)
        % trans-opt files.

        digraph.atsort(ImplDepsGraph, ImplDepsOrdering0),
        maybe_output_module_order(Globals, ModuleName, ImplDepsOrdering0, !IO),
        list.map(set.to_sorted_list, ImplDepsOrdering0, ImplDepsOrdering),
        list.condense(ImplDepsOrdering, TransOptDepsOrdering0),
        globals.lookup_accumulating_option(Globals, intermod_directories,
            IntermodDirs),
        get_opt_deps(Globals, yes, TransOptDepsOrdering0, IntermodDirs,
            ".trans_opt", TransOptDepsOrdering, !IO),

        trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
            io(!TIO)]
        (
            digraph.to_assoc_list(ImplDepsGraph, ImplDepsAL),
            io.print("ImplDepsAL:\n", !TIO),
            io.write_list(ImplDepsAL, "\n", print, !TIO),
            io.nl(!TIO)
        ),

        % Compute the indirect dependencies: they are equal to the composition
        % of the implementation dependencies with the transitive closure of the
        % implementation dependencies.  (We used to take the transitive closure
        % of the interface dependencies, but we now include implementation
        % details in the interface files).

        digraph.tc(ImplDepsGraph, TransImplDepsGraph),
        digraph.compose(ImplDepsGraph, TransImplDepsGraph, IndirectDepsGraph),

        % Compute the indirect optimization dependencies: indirect
        % dependencies including those via `.opt' or `.trans_opt' files.
        % Actually we can't compute that, since we don't know
        % which modules the `.opt' files will import!
        % Instead, we need to make a conservative (over-)approximation,
        % and assume that the each module's `.opt' file might import any
        % of that module's implementation dependencies; in actual fact,
        % it will be some subset of that.

        digraph.tc(ImplDepsGraph, IndirectOptDepsGraph),

        (
            Mode = output_d_file_only,
            DFilesToWrite = [ModuleDep]
        ;
            Mode = output_all_dependencies,
            DFilesToWrite = DepsList
        ),
        generate_dependencies_write_d_files(Globals, DFilesToWrite,
            IntDepsGraph, ImplDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptDepsOrdering, DepsMap, !IO)
    ),

    % For Java, the main target is actually a shell script which will
    % set CLASSPATH appropriately and invoke java on the appropriate
    % .class file.  Rather than generating an Mmake rule to build this
    % file when it is needed, we just generate this file "mmake depend"
    % time, since that is simpler and probably more efficient anyway.

    globals.get_target(Globals, Target),
    (
        Target = target_java,
        Mode = output_all_dependencies
    ->
        create_java_shell_script(Globals, ModuleName, _Succeeded, !IO)
    ;
        true
    ).

:- pred maybe_output_imports_graph(globals::in, module_name::in,
    digraph(sym_name)::in, digraph(sym_name)::in,
    io::di, io::uo) is det.

maybe_output_imports_graph(Globals, Module, IntDepsGraph, ImplDepsGraph,
        !IO) :-
    globals.lookup_bool_option(Globals, imports_graph, ImportsGraph),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        ImportsGraph = yes,
        module_name_to_file_name(Globals, Module, ".imports_graph",
            do_create_dirs, FileName, !IO),
        maybe_write_string(Verbose, "% Creating imports graph file `", !IO),
        maybe_write_string(Verbose, FileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        io.open_output(FileName, ImpResult, !IO),
        (
            ImpResult = ok(ImpStream),

            Deps0 = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(IntDepsGraph), digraph.init),
            Deps = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(ImplDepsGraph), Deps0),

            write_graph(ImpStream, "imports", sym_name_to_node_id, Deps, !IO),

            io.close_output(ImpStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            ImpResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening file `", FileName,
                "' for output: ", IOErrorMessage], ImpMessage),
            report_error(ImpMessage, !IO)
        )
    ;
        ImportsGraph = no
    ).

:- func filter_imports_graph(pair(sym_name, sym_name), digraph(sym_name)) =
    digraph(sym_name).

filter_imports_graph(A - B, DepsGraph) =
    (
        % Don't keep the edge if it points to a builtin-module or if the
        % relationship is between two standard library modules.
        % XXX it would be better to change this to be only keep those
        % edges for which the left-hand side is in the current directory.
        (
            any_mercury_builtin_module(B)
        ;
            is_std_lib_module_name(A, _),
            is_std_lib_module_name(B, _)
        )
    ->
        DepsGraph
    ;
        digraph.add_vertices_and_edge(A, B, DepsGraph)
    ).

:- type gen_node_name(T) == (func(T) = string).

:- pred write_graph(io.output_stream::in, string::in,
    gen_node_name(T)::in, digraph(T)::in, io::di, io::uo) is det.

write_graph(Stream, Name, GenNodeName, Graph, !IO) :-
    io.write_string(Stream, "digraph " ++ Name ++ " {\n", !IO),
    io.write_string(Stream, "label=\"" ++ Name ++ "\";\n", !IO),
    io.write_string(Stream, "center=true;\n", !IO),
    digraph.traverse(Graph, write_node(Stream, GenNodeName),
        write_edge(Stream, GenNodeName), !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred write_node(io.output_stream::in,
    gen_node_name(T)::in, T::in, io::di, io::uo) is det.

write_node(Stream, GenNodeName, Node, !IO) :-
    % Names can't contain "." so use "__"
    io.write_string(Stream, GenNodeName(Node), !IO),
    io.write_string(Stream, ";\n", !IO).

:- pred write_edge(io.output_stream::in, gen_node_name(T)::in, T::in, T::in,
    io::di, io::uo) is det.

write_edge(Stream, GenNodeName, A, B, !IO) :-
    io.write_string(Stream, GenNodeName(A), !IO),
    io.write_string(Stream, " -> ", !IO),
    io.write_string(Stream, GenNodeName(B), !IO),
    io.write_string(Stream, ";\n", !IO).

:- func sym_name_to_node_id(sym_name) = string.

sym_name_to_node_id(Name) =
    "\"" ++ sym_name_to_string(Name) ++ "\"".

:- pred maybe_output_module_order(globals::in, module_name::in,
    list(set(module_name))::in, io::di, io::uo) is det.

maybe_output_module_order(Globals, Module, DepsOrdering, !IO) :-
    globals.lookup_bool_option(Globals, generate_module_order, Order),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Order = yes,
        module_name_to_file_name(Globals, Module, ".order",
            do_create_dirs, OrdFileName, !IO),
        maybe_write_string(Verbose, "% Creating module order file `", !IO),
        maybe_write_string(Verbose, OrdFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        io.open_output(OrdFileName, OrdResult, !IO),
        (
            OrdResult = ok(OrdStream),
            io.write_list(OrdStream, DepsOrdering, "\n\n",
                write_module_scc(OrdStream), !IO),
            io.close_output(OrdStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            OrdResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening file `", OrdFileName,
                "' for output: ", IOErrorMessage], OrdMessage),
            report_error(OrdMessage, !IO)
        )
    ;
        Order = no
    ).

:- pred write_module_scc(io.output_stream::in, set(module_name)::in,
    io::di, io::uo) is det.

write_module_scc(Stream, SCC0, !IO) :-
    set.to_sorted_list(SCC0, SCC),
    io.write_list(Stream, SCC, "\n", prog_out.write_sym_name, !IO).

    % generate_dependencies_write_d_files(Globals, Modules,
    %   IntDepsRel, ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel,
    %   TransOptOrder, DepsMap, !IO):
    %
    % This predicate writes out the .d files for all the modules in the
    % Modules list.
    % IntDepsGraph gives the interface dependency graph.
    % ImplDepsGraph gives the implementation dependency graph.
    % IndirectDepsGraph gives the indirect dependency graph
    % (this includes dependencies on `*.int2' files).
    % IndirectOptDepsGraph gives the indirect optimization dependencies
    % (this includes dependencies via `.opt' and `.trans_opt' files).
    % These are all computed from the DepsMap.
    % TransOptOrder gives the ordering that is used to determine
    % which other modules the .trans_opt files may depend on.
    %
:- pred generate_dependencies_write_d_files(globals::in, list(deps)::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    list(module_name)::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_d_files(_, [], _, _, _, _, _, _, !IO).
generate_dependencies_write_d_files(Globals, [Dep | Deps],
        IntDepsGraph, ImplDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptOrder, DepsMap, !IO) :-
    some [!Module] (
        Dep = deps(_, !:Module),

        % Look up the interface/implementation/indirect dependencies
        % for this module from the respective dependency graphs,
        % and save them in the module_and_imports structure.

        module_and_imports_get_module_name(!.Module, ModuleName),
        get_dependencies_from_graph(IndirectOptDepsGraph, ModuleName,
            IndirectOptDeps),
        globals.lookup_bool_option(Globals, intermodule_optimization,
            Intermod),
        (
            Intermod = yes,
            % Be conservative with inter-module optimization -- assume a
            % module depends on the `.int', `.int2' and `.opt' files
            % for all transitively imported modules.
            IntDeps = IndirectOptDeps,
            ImplDeps = IndirectOptDeps,
            IndirectDeps = IndirectOptDeps
        ;
            Intermod = no,
            get_dependencies_from_graph(IntDepsGraph, ModuleName, IntDeps),
            get_dependencies_from_graph(ImplDepsGraph, ModuleName, ImplDeps),
            get_dependencies_from_graph(IndirectDepsGraph, ModuleName,
                IndirectDeps)
        ),

        globals.get_target(Globals, Target),
        ( Target = target_c, Lang = lang_c
        ; Target = target_java, Lang = lang_java
        ; Target = target_csharp, Lang = lang_csharp
        ; Target = target_il, Lang = lang_il
        ; Target = target_x86_64, Lang = lang_c
        ; Target = target_erlang, Lang = lang_erlang
        ),
        % Assume we need the `.mh' files for all imported modules
        % (we will if they define foreign types).
        ForeignImports = list.map(
            (func(ThisDep) = foreign_import_module_info(Lang, ThisDep,
                term.context_init)),
            IndirectOptDeps),
        !Module ^ mai_foreign_import_modules := ForeignImports,

        module_and_imports_set_int_deps(IntDeps, !Module),
        module_and_imports_set_impl_deps(ImplDeps, !Module),
        module_and_imports_set_indirect_deps(IndirectDeps, !Module),

        % Compute the trans-opt dependencies for this module. To avoid
        % the possibility of cycles, each module is only allowed to depend
        % on modules that occur later than it in the TransOptOrder.

        FindModule = (pred(OtherModule::in) is semidet :-
            ModuleName \= OtherModule
        ),
        list.takewhile(FindModule, TransOptOrder, _, TransOptDeps0),
        ( TransOptDeps0 = [_ | TransOptDeps1] ->
            % The module was found in the list.
            TransOptDeps = TransOptDeps1
        ;
            TransOptDeps = []
        ),

        % Note that even if a fatal error occured for one of the files
        % that the current Module depends on, a .d file is still produced,
        % even though it probably contains incorrect information.
        Error = !.Module ^ mai_error,
        (
            ( Error = no_module_errors
            ; Error = some_module_errors
            ),
            write_dependency_file(Globals, !.Module,
                set.list_to_set(IndirectOptDeps), yes(TransOptDeps), !IO)
        ;
            Error = fatal_module_errors
        ),
        generate_dependencies_write_d_files(Globals, Deps,
            IntDepsGraph, ImplDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptOrder, DepsMap, !IO)
    ).

:- pred get_dependencies_from_graph(deps_graph::in, module_name::in,
    list(module_name)::out) is det.

get_dependencies_from_graph(DepsGraph0, ModuleName, Deps) :-
    digraph.add_vertex(ModuleName, ModuleKey, DepsGraph0, DepsGraph),
    digraph.lookup_key_set_from(DepsGraph, ModuleKey, DepsKeysSet),
    sparse_bitset.foldl(
        (pred(Key::in, Deps0::in, [Dep | Deps0]::out) is det :-
            digraph.lookup_vertex(DepsGraph, Key, Dep)
        ), DepsKeysSet, [], Deps).

    % (Module1 -> Module2) means Module1 is imported by Module2.
:- type deps_graph == digraph(module_name).
:- type deps_graph_key == digraph_key(module_name).

    % Construct a pair of dependency graphs (the interface dependencies
    % and the implementation dependencies) for all the modules in the program.
    %
:- pred deps_list_to_deps_graph(list(deps)::in, deps_map::in,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

deps_list_to_deps_graph([], _, !IntDepsGraph, !ImplDepsGraph).
deps_list_to_deps_graph([Deps | DepsList], DepsMap, !IntDepsGraph,
        !ImplDepsGraph) :-
    Deps = deps(_, ModuleImports),
    ModuleError = ModuleImports ^ mai_error,
    ( ModuleError \= fatal_module_errors ->
        module_and_imports_to_deps_graph(ModuleImports,
            lookup_module_and_imports(DepsMap), !IntDepsGraph, !ImplDepsGraph)
    ;
        true
    ),
    deps_list_to_deps_graph(DepsList, DepsMap, !IntDepsGraph, !ImplDepsGraph).

:- func lookup_module_and_imports(deps_map, module_name) = module_and_imports.

lookup_module_and_imports(DepsMap, ModuleName) = ModuleImports :-
    map.lookup(DepsMap, ModuleName, deps(_, ModuleImports)).

add_module_relations(LookupModuleImports, ModuleName, !IntDepsGraph,
        !ImplDepsGraph) :-
    ModuleImports = LookupModuleImports(ModuleName),
    module_and_imports_to_deps_graph(ModuleImports, LookupModuleImports,
        !IntDepsGraph, !ImplDepsGraph).

:- pred module_and_imports_to_deps_graph(module_and_imports::in,
    lookup_module_and_imports::lookup_module_and_imports,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

module_and_imports_to_deps_graph(ModuleImports, LookupModuleImports,
        !IntDepsGraph, !ImplDepsGraph) :-
    % Add interface dependencies to the interface deps graph.
    %
    % Note that we need to do this both for the interface imports of this
    % module and for the *implementation* imports of its ancestors.
    % This is because if this module is defined in the implementation section
    % of its parent, then the interface of this module may depend on things
    % imported only by its parent's implementation.
    %
    % If this module was actually defined in the interface section of one
    % of its ancestors, then it should only depend on the interface imports
    % of that ancestor, so the dependencies added here are in fact more
    % conservative than they need to be in that case. However, that should
    % not be a major problem.

    ModuleName = ModuleImports ^ mai_module_name,
    ParentDeps = ModuleImports ^ mai_parent_deps,
    digraph.add_vertex(ModuleName, IntModuleKey, !IntDepsGraph),
    add_int_deps(IntModuleKey, ModuleImports, !IntDepsGraph),
    add_parent_impl_deps_list(LookupModuleImports, IntModuleKey, ParentDeps,
        !IntDepsGraph),

    % Add implementation dependencies to the impl. deps graph.
    % (The implementation dependencies are a superset of the interface
    % dependencies.)
    %
    % Note that we need to do this both for the imports of this module
    % and for the imports of its parents, because this module may depend on
    % things imported only by its parents.

    digraph.add_vertex(ModuleName, ImplModuleKey, !ImplDepsGraph),
    add_impl_deps(ImplModuleKey, ModuleImports, !ImplDepsGraph),
    add_parent_impl_deps_list(LookupModuleImports, ImplModuleKey, ParentDeps,
        !ImplDepsGraph).

    % Add interface dependencies to the interface deps graph.
    %
:- pred add_int_deps(deps_graph_key::in, module_and_imports::in,
    deps_graph::in, deps_graph::out) is det.

add_int_deps(ModuleKey, ModuleImports, !DepsGraph) :-
    AddDep = add_dep(ModuleKey),
    list.foldl(AddDep, ModuleImports ^ mai_parent_deps, !DepsGraph),
    list.foldl(AddDep, ModuleImports ^ mai_int_deps, !DepsGraph).

    % Add direct implementation dependencies for a module to the
    % implementation deps graph.
    %
:- pred add_impl_deps(deps_graph_key::in, module_and_imports::in,
    deps_graph::in, deps_graph::out) is det.

add_impl_deps(ModuleKey, ModuleImports, !DepsGraph) :-
    % The implementation dependencies are a superset of the
    % interface dependencies, so first we add the interface deps.
    add_int_deps(ModuleKey, ModuleImports, !DepsGraph),
    % then we add the impl deps
    module_and_imports_get_impl_deps(ModuleImports, ImplDeps),
    list.foldl(add_dep(ModuleKey), ImplDeps, !DepsGraph).

    % Add parent implementation dependencies for the given Parent module
    % to the impl. deps graph values for the given ModuleKey.
    %
:- pred add_parent_impl_deps(
    lookup_module_and_imports::lookup_module_and_imports,
    deps_graph_key::in, module_name::in, deps_graph::in, deps_graph::out)
    is det.

add_parent_impl_deps(LookupModuleImports, ModuleKey, Parent, !DepsGraph) :-
    ParentModuleImports = LookupModuleImports(Parent),
    add_impl_deps(ModuleKey, ParentModuleImports, !DepsGraph).

:- pred add_parent_impl_deps_list(
    lookup_module_and_imports::lookup_module_and_imports,
    deps_graph_key::in, list(module_name)::in, deps_graph::in, deps_graph::out)
    is det.

add_parent_impl_deps_list(LookupModuleImports, ModuleKey, Parents,
        !DepsGraph) :-
    list.foldl(add_parent_impl_deps(LookupModuleImports, ModuleKey), Parents,
        !DepsGraph).

    % Add a single dependency to a graph.
    %
:- pred add_dep(digraph_key(T)::in, T::in, digraph(T)::in, digraph(T)::out)
    is det.

add_dep(ModuleKey, Dep, !DepsGraph) :-
    digraph.add_vertex(Dep, DepKey, !DepsGraph),
    digraph.add_edge(ModuleKey, DepKey, !DepsGraph).

%-----------------------------------------------------------------------------%

:- pred append_to_init_list(io.output_stream::in, file_name::in,
    module_name::in, io::di, io::uo) is det.

append_to_init_list(DepStream, InitFileName, Module, !IO) :-
    InitFuncName0 = make_init_name(Module),
    string.append(InitFuncName0, "init", InitFuncName),
    io.write_strings(DepStream, [
        "\techo ""INIT ", InitFuncName, """ >> ", InitFileName, "\n"
    ], !IO).

%-----------------------------------------------------------------------------%

    % Find out which modules we need to generate C header files for,
    % assuming we're compiling with `--target asm'.
    %
:- func modules_that_need_headers(list(module_name), deps_map)
    = list(module_name).

modules_that_need_headers(Modules, DepsMap) =
    list.filter(module_needs_header(DepsMap), Modules).

    % Succeed iff we need to generate a C header file for the specified
    % module, assuming we're compiling with `--target asm'.
    %
:- pred module_needs_header(deps_map::in, module_name::in) is semidet.

module_needs_header(DepsMap, Module) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    ModuleImports ^ mai_has_foreign_code = contains_foreign_code(Langs),
    set.member(lang_c, Langs).

%-----------------------------------------------------------------------------%

process_module_private_interfaces(_, _, [], _, _, !DirectImports,
        !DirectUses, !Module, !IO).
process_module_private_interfaces(Globals, HaveReadModuleMap,
        [Ancestor | Ancestors], IntStatusItem, ImpStatusItem, !DirectImports,
        !DirectUses, !Module, !IO) :-
    ModuleName = !.Module ^ mai_module_name,
    ModAncestors0 = !.Module ^ mai_parent_deps,
    ( Ancestor = ModuleName ->
        unexpected($module, $pred, "module is its own ancestor?")
    ; list.member(Ancestor, ModAncestors0) ->
        % We've already read it.
        process_module_private_interfaces(Globals, HaveReadModuleMap,
            Ancestors, IntStatusItem, ImpStatusItem,
            !DirectImports, !DirectUses, !Module, !IO)
    ;
        maybe_return_timestamp(!.Module ^ mai_maybe_timestamps,
            ReturnTimestamp),
        maybe_read_module(Globals, HaveReadModuleMap, Ancestor, ".int0",
            "Reading private interface for module", do_search, ReturnTimestamp,
            PrivateIntItems0, PrivateIntSpecs, PrivateIntError,
            _AncestorFileName, MaybeTimestamp, !IO),

        maybe_record_timestamp(Ancestor, ".int0", may_be_unqualified,
            MaybeTimestamp, !Module),

        replace_section_decls(IntStatusItem, ImpStatusItem,
            PrivateIntItems0, PrivateIntItems),

        module_and_imports_add_items(cord.from_list(PrivateIntItems), !Module),
        module_and_imports_add_specs(PrivateIntSpecs, !Module),
        module_and_imports_add_interface_error(PrivateIntError, !Module),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        maybe_report_stats(Statistics, !IO),

        (
            PrivateIntError = fatal_module_errors,
            ModAncestors = ModAncestors0
        ;
            ( PrivateIntError = no_module_errors
            ; PrivateIntError = some_module_errors
            ),
            ModAncestors = [Ancestor | ModAncestors0]
        ),
        get_dependencies(PrivateIntItems, AncDirectImports, AncDirectUses),
        !:DirectImports = !.DirectImports ++ AncDirectImports,
        !:DirectUses = !.DirectUses ++ AncDirectUses,
        !Module ^ mai_parent_deps := ModAncestors,

        process_module_private_interfaces(Globals, HaveReadModuleMap,
            Ancestors, IntStatusItem, ImpStatusItem,
            !DirectImports, !DirectUses, !Module, !IO)
    ).

%-----------------------------------------------------------------------------%

process_module_long_interfaces(_, _, _, [], _Ext, _, _,
        !IndirectImports, !ImplIndirectImports, !Module, !IO).
process_module_long_interfaces(Globals, HaveReadModuleMap, NeedQualifier,
        [Import | Imports], Ext, IntStatusItem, ImpStatusItem,
        !IndirectImports, !ImplIndirectImports, !Module, !IO) :-
    ModuleName = !.Module ^ mai_module_name,
    ModImplementationImports0 = !.Module ^ mai_impl_deps,
    (
        % Have we already read it?
        ( Import = ModuleName
        ; list.member(Import, !.Module ^ mai_parent_deps)
        ; list.member(Import, !.Module ^ mai_int_deps)
        ; list.member(Import, ModImplementationImports0)
        )
    ->
        process_module_long_interfaces(Globals, HaveReadModuleMap,
            NeedQualifier, Imports, Ext, IntStatusItem, ImpStatusItem,
            !IndirectImports, !ImplIndirectImports, !Module, !IO)
    ;
        maybe_return_timestamp(!.Module ^ mai_maybe_timestamps,
            ReturnTimestamp),
        maybe_read_module(Globals, HaveReadModuleMap, Import, Ext,
            "Reading interface for module", do_search, ReturnTimestamp,
            LongIntItems0, LongIntSpecs, LongIntError, _LongIntFileName,
            MaybeTimestamp, !IO),

        get_dependencies_int_imp(LongIntItems0,
            IndirectImports1, IndirectUses1,
            ImplIndirectImports1, ImplIndirectUses1),
        replace_section_decls(IntStatusItem, ImpStatusItem,
            LongIntItems0, LongIntItems),

        module_and_imports_add_items(cord.from_list(LongIntItems), !Module),
        module_and_imports_add_specs(LongIntSpecs, !Module),
        module_and_imports_add_interface_error(LongIntError, !Module),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        maybe_report_stats(Statistics, !IO),

        (
            LongIntError = fatal_module_errors,
            ModImplementationImports = ModImplementationImports0
        ;
            ( LongIntError = no_module_errors
            ; LongIntError = some_module_errors
            ),
            maybe_record_timestamp(Import, Ext, NeedQualifier, MaybeTimestamp,
                !Module),
            ModImplementationImports = [Import | ModImplementationImports0]
        ),
        !:IndirectImports = !.IndirectImports ++ IndirectImports1
            ++ IndirectUses1,
        !:ImplIndirectImports = !.ImplIndirectImports
            ++ ImplIndirectImports1 ++ ImplIndirectUses1,
        !Module ^ mai_impl_deps := ModImplementationImports,

        process_module_long_interfaces(Globals, HaveReadModuleMap,
            NeedQualifier, Imports, Ext, IntStatusItem, ImpStatusItem,
            !IndirectImports, !ImplIndirectImports, !Module, !IO)
    ).

    % At this point, we've read in all the appropriate interface files,
    % including, for every imported/used module, at least the short
    % interface for that module's parent module, which will contain
    % the `include_module' declarations for any exported sub-modules
    % of the parent.  So the accessible sub-modules can be determined
    % by just calling get_accessible_children on the complete item list.
    %
    % We then go through all of the imported/used modules,
    % checking that each one is accessible.
    %
:- pred check_imports_accessibility(module_name::in, list(module_name)::in,
    list(item)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_imports_accessibility(ModuleName, Imports, Items, !Specs) :-
    get_accessible_children(Items, AccessibleSubModules),
    list.foldl(check_module_accessibility(ModuleName,
        AccessibleSubModules, Items), Imports, !Specs).

:- pred check_module_accessibility(module_name::in, list(module_name)::in,
    list(item)::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_module_accessibility(ModuleName, AccessibleSubModules, Items,
        ImportedModule, !Specs) :-
    ( ImportedModule = qualified(ParentModule, SubModule) ->
        ( list.member(ImportedModule, AccessibleSubModules) ->
            true
        ;
            % The user attempted to import an inaccessible submodule,
            % so report an error. Unfortunately we didn't get passed the
            % context(s) of the `import_module' or `use_module' declaration(s),
            % so we need to search the item list again to find them.
            FindImports = (pred(Item::in, ImportInfo::out) is semidet :-
                Item = item_module_defn(ItemModuleDefn),
                ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _),
                (
                    ModuleDefn = md_import(ItemModuleSpecs),
                    DeclName = "import_module"
                ;
                    ModuleDefn = md_use(ItemModuleSpecs),
                    DeclName = "use_module"
                ),
                list.member(ImportedModule, ItemModuleSpecs),
                ImportInfo = DeclName - Context
            ),
            list.filter_map(FindImports, Items, ImportInfos),
            (
                ImportInfos = [],
                unexpected($module, $pred, "check_parent_module")
            ;
                ImportInfos = [_ | _],
                list.foldl(
                    report_inaccessible_module_error(ModuleName,
                        ParentModule, SubModule),
                    ImportInfos, !Specs)
            )
        )
    ;
        true
    ).

:- pred report_inaccessible_module_error(module_name::in, module_name::in,
    string::in, pair(string, prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

% The error message should come out like this
% (the second sentence is included only with --verbose-errors):
% very_long_name.m:123: In module `very_long_name':
% very_long_name.m:123:   error in `import_module' declaration:
% very_long_name.m:123:   module `parent_module.sub_module' is inaccessible.
% very_long_name.m:123:   Either there was no prior `import_module' or
% very_long_name.m:123:   `use_module' declaration to import module
% very_long_name.m:123:   `parent_module', or the interface for module
% very_long_name.m:123:   `parent_module' does not contain an `include_module'
% very_long_name.m:123:   declaration for module `sub_module'.

report_inaccessible_module_error(ModuleName, ParentModule, SubModule,
        DeclName - Context, !Specs) :-
    MainPieces = [words("In module"), sym_name(ModuleName), suffix(":"), nl,
        words("error in"), quote(DeclName), words("declaration:"), nl,
        words("module"), sym_name(qualified(ParentModule, SubModule)),
        words("is inaccessible."), nl],
    VerbosePieces = [words("Either there was no prior"),
        quote("import_module"),
            words("or"), quote("use_module"),
            words("declaration to import module"), sym_name(ParentModule),
            suffix(","), words("or the interface for module"),
            sym_name(ParentModule), words("does not contain an"),
            quote("include_module"), words("declaration for module"),
            quote(SubModule), suffix("."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

process_module_short_interfaces_and_impls_transitively(Globals,
        HaveReadModuleMap, Imports, Ext, IntStatusItem, ImpStatusItem,
        !Module, !IO) :-
    process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        Imports, Ext, IntStatusItem, ImpStatusItem, [], ImpIndirectImports,
        !Module, !IO),
    (
        ImpIndirectImports = []
    ;
        ImpIndirectImports = [_ | _],
        process_module_short_interfaces_and_impls_transitively(Globals,
            HaveReadModuleMap, ImpIndirectImports, Ext,
            IntStatusItem, ImpStatusItem, !Module, !IO)
    ).

process_module_short_interfaces_transitively(Globals, HaveReadModuleMap,
        Imports, Ext, IntStatusItem, ImpStatusItem, !ImpIndirectImports,
        !Module, !IO) :-
    process_module_short_interfaces(Globals, HaveReadModuleMap, Imports, Ext,
        IntStatusItem, ImpStatusItem, [], IndirectImports,
        !ImpIndirectImports, !Module, !IO),
    (
        IndirectImports = []
    ;
        IndirectImports = [_ | _],
        process_module_short_interfaces_transitively(Globals,
            HaveReadModuleMap, IndirectImports, Ext,
            IntStatusItem, ImpStatusItem, !ImpIndirectImports, !Module, !IO)
    ).

process_module_short_interfaces(_, _, [], _, _, _, !IndirectImports,
        !ImpIndirectImports, !Module, !IO).
process_module_short_interfaces(Globals, HaveReadModuleMap, [Import | Imports],
        Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
        !ImpIndirectImports, !Module, !IO) :-
    ModIndirectImports0 = !.Module ^ mai_indirect_deps,
    (
        % check if the imported module has already been imported
        ( Import = !.Module ^ mai_module_name
        ; list.member(Import, !.Module ^ mai_parent_deps)
        ; list.member(Import, !.Module ^ mai_int_deps)
        ; list.member(Import, !.Module ^ mai_impl_deps)
        ; list.member(Import, ModIndirectImports0)
        )
    ->
        process_module_short_interfaces(Globals, HaveReadModuleMap, Imports,
            Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ;
        maybe_return_timestamp(!.Module ^ mai_maybe_timestamps,
            ReturnTimestamp),
        maybe_read_module(Globals, HaveReadModuleMap, Import, Ext,
            "Reading short interface for module", do_search,
            ReturnTimestamp, ShortIntItems0, ShortIntSpecs, ShortIntError,
            _ImportFileName, MaybeTimestamp, !IO),
        maybe_record_timestamp(Import, Ext, must_be_qualified,
            MaybeTimestamp, !Module),

        get_dependencies_int_imp(ShortIntItems0, IntImports1, IntUses1,
            ImpImports1, ImpUses1),
        replace_section_decls(IntStatusItem, ImpStatusItem,
            ShortIntItems0, ShortIntItems),

        module_and_imports_add_items(cord.from_list(ShortIntItems), !Module),
        module_and_imports_add_specs(ShortIntSpecs, !Module),
        module_and_imports_add_interface_error(ShortIntError, !Module),

        globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
        maybe_report_stats(Statistics, !IO),

        ModIndirectImports = [Import | ModIndirectImports0],
        !:IndirectImports = !.IndirectImports ++ IntImports1 ++ IntUses1,
        !:ImpIndirectImports = !.ImpIndirectImports ++ ImpImports1 ++ ImpUses1,
        !Module ^ mai_indirect_deps := ModIndirectImports,

        process_module_short_interfaces(Globals, HaveReadModuleMap, Imports,
            Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ).

replace_section_decls(IntStatusItem, ImpStatusItem, !Items) :-
    list.map(replace_section_decl(IntStatusItem, ImpStatusItem), !Items).

:- pred replace_section_decl(item::in, item::in, item::in, item::out) is det.

replace_section_decl(IntStatusItem, ImpStatusItem, Item0, Item) :-
    (
        Item0 = item_module_defn(ItemModuleDefn0),
        ItemModuleDefn0 = item_module_defn_info(ModuleDefn0, _, _),
        (
            ModuleDefn0 = md_interface,
            ItemPrime = IntStatusItem
        ;
            ModuleDefn0 = md_implementation,
            ItemPrime = ImpStatusItem
        )
    ->
        Item = ItemPrime
    ;
        Item = Item0
    ).

%-----------------------------------------------------------------------------%

get_children(Items, IncludeDeps) :-
    get_children_2(Items, [], IncludeDeps).

:- pred get_children_2(list(item)::in,
    list(module_name)::in, list(module_name)::out) is det.

get_children_2([], !IncludeDeps).
get_children_2([Item | Items], !IncludeDeps) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ModuleDefn = md_include_module(Modules)
    ->
        !:IncludeDeps = !.IncludeDeps ++ Modules
    ;
        true
    ),
    get_children_2(Items, !IncludeDeps).

    % get_accessible_children(Items, IncludeDeps):
    %
    % IncludeDeps is the list of sub-modules declared with `:- include_module'
    % in Items which are visible in the current module.
    %
:- pred get_accessible_children(list(item)::in, list(module_name)::out) is det.

get_accessible_children(Items, IncludeDeps) :-
    get_accessible_children_2(yes, Items, [], IncludeDeps).

:- pred get_accessible_children_2(bool::in, list(item)::in,
    list(module_name)::in, list(module_name)::out) is det.

get_accessible_children_2(_, [], !IncludeDeps).
get_accessible_children_2(!.Visible, [Item | Items], !IncludeDeps) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            ( ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ),
            !:Visible = no
        ;
            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ),
            !:Visible = yes
        ;
            ModuleDefn = md_include_module(Modules),
            (
                !.Visible = yes,
                !:IncludeDeps = !.IncludeDeps ++ Modules
            ;
                !.Visible = no
            )
        ;
            ( ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_version_numbers(_, _)
            )
            % Do nothing.
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
        )
    ),
    get_accessible_children_2(!.Visible, Items, !IncludeDeps).

%-----------------------------------------------------------------------------%

:- type submodule_map == map(module_name, list(item)).

    % Given a module (well, a list of items), split it into
    % its constituent sub-modules, in top-down order.
    %
split_into_submodules(ModuleName, Items0, ModuleList, !Specs) :-
    InParentInterface = no,
    split_into_submodules_2(ModuleName, Items0, InParentInterface,
        LeftOverItems, ModuleList, !Specs),

    % Check that there are no items after the end_module declaration.
    (
        LeftOverItems = []
    ;
        LeftOverItems = [FirstLeftOverItem | _],
        Context = get_item_context(FirstLeftOverItem),
        report_items_after_end_module(Context, !Specs)
    ),

    % Check for modules declared as both nested and separate sub-modules.
    get_children(Items0, NestedSubmodules),
    assoc_list.keys(ModuleList, SeparateSubModules),
    Duplicates = set.intersect(
        set.list_to_set(NestedSubmodules),
        set.list_to_set(SeparateSubModules)),
    ( set.empty(Duplicates) ->
        true
    ;
        report_duplicate_modules(Duplicates, Items0, !Specs)
    ).

:- pred split_into_submodules_2(module_name::in, list(item)::in, bool::in,
    list(item)::out, module_list::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_into_submodules_2(ModuleName, Items0, InParentInterface, Items,
        ModuleList, !Specs) :-
    InInterface0 = no,
    split_into_submodules_3(ModuleName, Items0,
        InParentInterface, InInterface0,
        ThisModuleItems, Items, SubModules, !Specs),
    map.to_assoc_list(SubModules, SubModuleList),
    ModuleList = [ModuleName - ThisModuleItems | SubModuleList].

:- pred split_into_submodules_3(module_name::in, list(item)::in, bool::in,
    bool::in, list(item)::out, list(item)::out,
    map(module_name, list(item))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_into_submodules_3(_ModuleName, [], _, _, [], [], SubModules, !Specs) :-
    map.init(SubModules).
split_into_submodules_3(ModuleName, [Item | Items1],
        InParentInterface, !.InInterface,
        ThisModuleItems, OtherItems, SubModules, !Specs) :-
    (
        % Check for a `module' declaration, which signals the start
        % of a nested module.
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart =
            item_module_start_info(SubModuleName, Context, SeqNum)
    ->
        % Parse in the items for the nested submodule.
        split_into_submodules_2(SubModuleName, Items1, !.InInterface,
            Items2, SubModules0, !Specs),

        % Parse in the remaining items for this module.
        split_into_submodules_3(ModuleName, Items2,
            InParentInterface, !.InInterface,
            ThisModuleItems0, Items3, SubModules1, !Specs),

        % Combine the submodule declarations from the previous two steps.
        list.foldl(add_submodule, SubModules0, SubModules1, SubModules),

        % Replace the nested submodule with an `include_module' declaration.
        IncludeSubModModuleDefn = md_include_module([SubModuleName]),
        IncludeSubModItemModuleDefn = item_module_defn_info(
            IncludeSubModModuleDefn, Context, SeqNum),
        IncludeSubModItem = item_module_defn(IncludeSubModItemModuleDefn),
        ThisModuleItems = [IncludeSubModItem | ThisModuleItems0],
        OtherItems = Items3
    ;
        % Check for a matching `end_module' declaration.
        Item = item_module_end(ItemModuleEnd),
        ItemModuleEnd = item_module_end_info(EndModuleName, _, _),
        EndModuleName = ModuleName
    ->
        % If so, that's the end of this module.
        ThisModuleItems = [],
        OtherItems = Items1,
        map.init(SubModules)
    ;
        % Otherwise, process the next item in this module.

        % Update the flag which records whether we're currently in the
        % interface section, and report an error if there is an
        % `implementation' section inside an `interface' section.
        ( Item = item_module_defn(ItemModuleDefn) ->
            ItemModuleDefn = item_module_defn_info(ModuleDefn, Context,
                _SeqNum),
            ( ModuleDefn = md_interface ->
                !:InInterface = yes
            ; ModuleDefn = md_implementation ->
                !:InInterface = no,
                (
                    InParentInterface = yes,
                    report_error_implementation_in_interface(ModuleName,
                        Context, !Specs)
                ;
                    InParentInterface = no
                )
            ;
                true
            )
        ;
            true
        ),

        % Check to make sure that a non-abstract instance declaration
        % does not occur in a module interface.
        (
            !.InInterface = yes,
            Item = item_instance(ItemInstance),
            ItemInstance ^ ci_method_instances \= instance_body_abstract
        ->
            InstanceContext = ItemInstance ^ ci_context,
            report_non_abstract_instance_in_interface(InstanceContext, !Specs)
        ;
            true
        ),

        % Parse the remaining items for this module.
        split_into_submodules_3(ModuleName, Items1,
            InParentInterface, !.InInterface,
            ThisModuleItems0, Items2, SubModules, !Specs),

        % Put the current item back onto the front of the item list
        % for this module.
        ThisModuleItems = [Item | ThisModuleItems0],
        OtherItems = Items2
    ).

:- pred add_submodule(pair(module_name, list(item))::in,
    submodule_map::in, submodule_map::out) is det.

add_submodule(ModuleName - ModuleItemList, !SubModules) :-
    % If the same module name occurs twice, then just append the lists of items
    % together. Perhaps we should be a bit more strict about this, for example
    % by only allowing one `:- implementation' section and one `:- interface'
    % section for each module? (That is what the Mercury language reference
    % manual mandates. On the other hand, it also says that top-level modules
    % should only have one `:- interface' and one `:- implementation' section,
    % and we don't enforce that either...)
    ( map.search(!.SubModules, ModuleName, ItemList0) ->
        list.append(ModuleItemList, ItemList0, ItemList),
        map.det_update(ModuleName, ItemList, !SubModules)
    ;
        map.det_insert(ModuleName, ModuleItemList, !SubModules)
    ).

:- pred report_error_implementation_in_interface(module_name::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_error_implementation_in_interface(ModuleName, Context, !Specs) :-
    (
        ModuleName = qualified(ParentModule0, ChildModule0),
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        ModuleName = unqualified(_),
        unexpected($module, $pred, "unqualified module name")
    ),
    Pieces = [words("In interface for module"), sym_name(ParentModule),
        suffix(":"), nl,
        words("in definition of sub-module `" ++ ChildModule ++ "':"), nl,
        words("error: `:- implementation.' declaration for sub-module\n"),
        words("occurs in interface section of parent module.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_duplicate_modules(set(module_name)::in, list(item)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_modules(Duplicates, Items, !Specs) :-
    list.filter_map(is_duplicate_error(Duplicates), Items,
        DuplicateErrorLists),
    list.condense(DuplicateErrorLists, DuplicateErrors),
    list.foldl(report_error_duplicate_module_decl, DuplicateErrors, !Specs).

:- pred is_duplicate_error(set(module_name)::in, item::in,
    list(pair(module_name, prog_context))::out) is semidet.

is_duplicate_error(DuplicatesSet, Item, SubModuleNameContexts) :-
    (
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart = item_module_start_info(SubModuleName, Context, _),
        set.member(SubModuleName, DuplicatesSet),
        SubModuleNameContexts = [SubModuleName - Context]
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _),
        ModuleDefn = md_include_module(SubModuleNames),
        set.list_to_set(SubModuleNames, SubModuleNamesSet),
        set.intersect(SubModuleNamesSet, DuplicatesSet,
            DuplicatedSubModuleNamesSet),
        set.to_sorted_list(DuplicatedSubModuleNamesSet,
            DuplicatedSubModuleNames),
        SubModuleNameContexts =
            list.map(pair_with_context(Context), DuplicatedSubModuleNames)
    ).

:- func pair_with_context(prog_context, module_name) =
    pair(module_name, prog_context).

pair_with_context(Context, ModuleName) = ModuleName - Context.

:- pred report_error_duplicate_module_decl(pair(module_name, prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_error_duplicate_module_decl(ModuleName - Context, !Specs) :-
    (
        ModuleName = qualified(ParentModule0, ChildModule0),
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        ModuleName = unqualified(_),
        unexpected($module, $pred, "unqualified module name")
    ),
    Pieces = [words("In module"), sym_name(ParentModule), suffix(":"), nl,
        words("error: sub-module `" ++ ChildModule ++ "' declared"),
        words("as both a separate sub-module and a nested sub-module.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_items_after_end_module(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_items_after_end_module(Context, !Specs) :-
    Pieces = [words("Error: item(s) after end_module declaration.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_non_abstract_instance_in_interface(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_non_abstract_instance_in_interface(Context, !Specs) :-
    Pieces = [words("Error: non-abstract instance declaration"),
        words("in module interface.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

get_interface(ModuleName, IncludeImplTypes, Items0, Items) :-
    AddToImpl = (func(_, ImplItems) = ImplItems),
    get_interface_and_implementation_2(IncludeImplTypes, Items0, no,
        [], RevItems, AddToImpl, unit, _),
    list.reverse(RevItems, Items1),
    maybe_add_foreign_import_module(ModuleName, Items1, Items2),
    order_items(Items2, Items).

:- pred get_interface_and_implementation(module_name::in, bool::in,
    list(item)::in, list(item)::out, list(item)::out) is det.

get_interface_and_implementation(ModuleName, IncludeImplTypes,
        Items0, InterfaceItems, ImplementationItems) :-
    AddToImpl = (func(ImplItem, ImplItems) = [ImplItem | ImplItems]),
    get_interface_and_implementation_2(IncludeImplTypes, Items0, no,
        [], RevIntItems, AddToImpl, [], RevImplItems),
    list.reverse(RevIntItems, InterfaceItems0),
    list.reverse(RevImplItems, ImplementationItems),
    maybe_add_foreign_import_module(ModuleName,
        InterfaceItems0, InterfaceItems).

:- pred init_module_and_imports(file_name::in,
    module_name::in, module_name::in, list(item)::in, list(error_spec)::in,
    list(module_name)::in, list(module_name)::in, list(string)::in,
    foreign_include_file_info_list::in,
    maybe(module_timestamps)::in, module_and_imports::out) is det.

init_module_and_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, Specs, PublicChildren, NestedChildren, FactDeps,
        ForeignIncludeFiles, MaybeTimestamps, Module) :-
    % XXX The reason why init_module_and_imports is here and not in
    % module_imports.m is this call. This should be fixed, preferably
    % by changing the module_and_imports structure.
    maybe_add_foreign_import_module(ModuleName, Items0, Items),
    ItemsCord = cord.from_list(Items),
    Module = module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, [], [], [], [], [], PublicChildren,
        NestedChildren, FactDeps, contains_foreign_code_unknown, [],
        ForeignIncludeFiles, contains_no_foreign_export, ItemsCord, Specs,
        no_module_errors, MaybeTimestamps, no_main, dir.this_directory).

:- pred maybe_add_foreign_import_module(module_name::in,
    list(item)::in, list(item)::out) is det.

maybe_add_foreign_import_module(ModuleName, Items0, Items) :-
    get_foreign_self_imports(Items0, Langs),
    ImportItems = list.map(make_foreign_import(ModuleName), Langs),
    Items = ImportItems ++ Items0.

:- func make_foreign_import(module_name, foreign_language) = item.

make_foreign_import(ModuleName, Lang) = Item :-
    Origin = compiler(foreign_imports),
    Info = pragma_info_foreign_import_module(Lang, ModuleName),
    Pragma = pragma_foreign_import_module(Info),
    ItemPragma = item_pragma_info(Origin, Pragma, term.context_init, -1),
    Item = item_pragma(ItemPragma).

get_foreign_self_imports(Items, Langs) :-
    list.foldl(accumulate_item_foreign_import_langs, Items, set.init, LangSet),
    set.to_sorted_list(LangSet, Langs).

:- pred accumulate_item_foreign_import_langs(item::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

accumulate_item_foreign_import_langs(Item, !LangSet) :-
    Langs = item_needs_foreign_imports(Item),
    set.insert_list(Langs, !LangSet).

:- pred get_interface_and_implementation_2(bool::in, list(item)::in, bool::in,
    list(item)::in, list(item)::out,
    func(item, T) = T::in, T::in, T::out) is det.

get_interface_and_implementation_2(_, [], _, !RevIntItems, _, !RevImplItems).
get_interface_and_implementation_2(IncludeImplTypes, [Item | Rest],
        !.InInterface, !RevIntItems, AddImplItem, !RevImplItems) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        ( ModuleDefn = md_interface
        ; ModuleDefn = md_implementation
        ; ModuleDefn = md_imported(_)
        ; ModuleDefn = md_used(_)
        )
    ->
        (
            ModuleDefn = md_interface,
            !:RevIntItems = [Item | !.RevIntItems],
            !:InInterface = yes,
            get_interface_and_implementation_2(IncludeImplTypes, Rest,
                !.InInterface, !RevIntItems, AddImplItem, !RevImplItems)
        ;
            ModuleDefn = md_implementation,
            !:RevIntItems = [Item | !.RevIntItems],
            !:InInterface = no,
            get_interface_and_implementation_2(IncludeImplTypes, Rest,
                !.InInterface, !RevIntItems, AddImplItem, !RevImplItems)
        ;
            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            )
            % Items after here are not part of this module, which is why
            % we don't have a recursive call here.
        )
    ;
        (
            !.InInterface = yes,
            ( Item = item_instance(ItemInstance) ->
                % Include the abstract version of the instance in the
                % interface, ...
                AbstractItemInstance = make_instance_abstract(ItemInstance),
                AbstractItem = item_instance(AbstractItemInstance),
                !:RevIntItems = [AbstractItem | !.RevIntItems],

                % ... and the concrete version in the implementation.
                !:RevImplItems = AddImplItem(Item, !.RevImplItems)
            ;
                !:RevIntItems = [Item | !.RevIntItems]
            )
        ;
            !.InInterface = no,
            !:RevImplItems = AddImplItem(Item, !.RevImplItems),
            (
                IncludeImplTypes = yes,
                include_in_int_file_implementation(Item) = yes
            ->
                ( make_abstract_defn(Item, int2, AbstractItem) ->
                    ItemToAdd = AbstractItem
                ; make_abstract_unify_compare(Item, int2, AbstractItem) ->
                    ItemToAdd = AbstractItem
                ;
                    ItemToAdd = Item
                ),
                !:RevIntItems = [ItemToAdd | !.RevIntItems]
            ;
                true
            )
        ),
        get_interface_and_implementation_2(IncludeImplTypes, Rest,
            !.InInterface, !RevIntItems, AddImplItem, !RevImplItems)
    ).

:- type short_interface_kind
    --->    int2    % the qualified short interface, for the .int2 file
    ;       int3.   % the unqualified short interface, for the .int3 file

    % Given a module interface (well, a list of items), extract the
    % short interface part of that module, i.e. the exported
    % type/typeclass/inst/mode declarations, but not the exported pred or
    % constructor declarations.  If the module interface imports
    % other modules, then the short interface only needs to include
    % those import_module declarations only if the short interface
    % contains some equivalence types or some mode or inst definitions
    % that might use declarations in the imported modules.
    % If the short interface is empty, or only contains abstract
    % type declarations, then it doesn't need any import_module
    % declarations.
    %
:- pred get_short_interface(list(item)::in, short_interface_kind::in,
    list(item)::out) is det.

get_short_interface(Items0, Kind, Items) :-
    get_short_interface_2(Items0, Kind, [], RevItems),
    list.reverse(RevItems, Items1),
    maybe_strip_import_decls(Items1, Items2),
    order_items(Items2, Items).

:- pred get_short_interface_2(list(item)::in, short_interface_kind::in,
    list(item)::in, list(item)::out) is det.

get_short_interface_2([], _Kind, !RevItems).
get_short_interface_2([Item | Items], Kind, !RevItems) :-
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
    get_short_interface_2(Items, Kind, !RevItems).

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

    % Could this item use items from imported modules.
    %
:- func item_needs_imports(item) = bool.

item_needs_imports(Item) = NeedsImports :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ( ItemTypeDefn ^ td_ctor_defn = parse_tree_abstract_type(_) ->
            NeedsImports = no
        ;
            NeedsImports = yes
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pragma(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        NeedsImports = yes
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_module_defn(_)
        ; Item = item_nothing(_)
        ),
        NeedsImports = no
    ).

:- func item_needs_foreign_imports(item) = list(foreign_language).

item_needs_foreign_imports(Item) = Langs :-
    (
        Item = item_mutable(_ItemMutable),
        % We can use all foreign languages.
        Langs = all_foreign_languages
    ;
        Item = item_type_defn(ItemTypeDefn),
        (
            ItemTypeDefn ^ td_ctor_defn =
                parse_tree_foreign_type(ForeignType, _, _)
        ->
            Langs = [foreign_type_language(ForeignType)]
        ;
            Langs = []
        )
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        (
            (
                Pragma = pragma_foreign_decl(FDInfo),
                FDInfo = pragma_info_foreign_decl(Lang, _, _)
            ;
                Pragma = pragma_foreign_code(FCInfo),
                FCInfo = pragma_info_foreign_code(Lang, _)
            ;
                Pragma = pragma_foreign_enum(FEInfo),
                FEInfo = pragma_info_foreign_enum(Lang, _, _)
            ;
                Pragma = pragma_foreign_proc_export(FPEInfo),
                FPEInfo = pragma_info_foreign_proc_export(Lang, _, _)
            ),
            Langs = [Lang]
        ;
            Pragma = pragma_foreign_proc(FPInfo),
            FPInfo = pragma_info_foreign_proc(Attrs, _, _, _, _, _, _),
            Langs = [get_foreign_language(Attrs)]
        ;
            ( Pragma = pragma_foreign_import_module(_)
            ; Pragma = pragma_foreign_export_enum(_)
            ; Pragma = pragma_type_spec(_)
            ; Pragma = pragma_inline(_)
            ; Pragma = pragma_no_inline(_)
            ; Pragma = pragma_unused_args(_)
            ; Pragma = pragma_exceptions(_)
            ; Pragma = pragma_trailing_info(_)
            ; Pragma = pragma_mm_tabling_info(_)
            ; Pragma = pragma_obsolete(_)
            ; Pragma = pragma_no_detism_warning(_)
            ; Pragma = pragma_source_file(_)
            ; Pragma = pragma_oisu(_)
            ; Pragma = pragma_tabled(_)
            ; Pragma = pragma_fact_table(_)
            ; Pragma = pragma_reserve_tag(_)
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
            ),
            Langs = []
        )
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_module_defn(_)
        ; Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_nothing(_)
        ),
        Langs = []
    ).

:- func include_in_int_file_implementation(item) = bool.

include_in_int_file_implementation(Item) = Include :-
    (
        % `:- typeclass declarations' may be referred to by the constructors
        % in type declarations. Since these constructors are abstractly
        % exported, we won't need the local instance declarations.
        ( Item = item_type_defn(_)
        ; Item = item_typeclass(_)
        ),
        Include = yes
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        (
            % XXX Some of these should yield an exception.
            ( ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ; ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_include_module(_)
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            Include = yes
        ;
            ModuleDefn = md_external(_, _),
            Include = no
        )
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        (
            ( Pragma = pragma_foreign_import_module(_)
            ; Pragma = pragma_foreign_enum(_)
            ),
            Include = yes
        ;
            % XXX I am not sure about the proper value of Include
            % for some of these. -zs
            ( Pragma = pragma_foreign_decl(_)
            ; Pragma = pragma_foreign_code(_)
            ; Pragma = pragma_foreign_proc(_)
            ; Pragma = pragma_foreign_proc_export(_)
            ; Pragma = pragma_foreign_export_enum(_)
            ; Pragma = pragma_type_spec(_)
            ; Pragma = pragma_inline(_)
            ; Pragma = pragma_no_inline(_)
            ; Pragma = pragma_unused_args(_)
            ; Pragma = pragma_exceptions(_)
            ; Pragma = pragma_trailing_info(_)
            ; Pragma = pragma_mm_tabling_info(_)
            ; Pragma = pragma_obsolete(_)
            ; Pragma = pragma_no_detism_warning(_)
            ; Pragma = pragma_source_file(_)
            ; Pragma = pragma_oisu(_)
            ; Pragma = pragma_tabled(_)
            ; Pragma = pragma_fact_table(_)
            ; Pragma = pragma_reserve_tag(_)
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
            ),
            Include = no
        )
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        ),
        Include = no
    ).

    % XXX make_abstract_defn should be merged with make_abstract_unify_compare
    % and made det, returning the unchanged item if it does not need to be made
    % abstract (so we can use det switches instead semidet tests in the code).

:- pred make_abstract_defn(item::in, short_interface_kind::in, item::out)
    is semidet.

make_abstract_defn(Item, ShortInterfaceKind, AbstractItem) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        TypeDefn = ItemTypeDefn ^ td_ctor_defn,
        (
            TypeDefn = parse_tree_du_type(Ctors, _, _),
            ( du_type_is_enum(Ctors, NumBits) ->
                AbstractDetails = abstract_enum_type(NumBits)
            ;
                AbstractDetails = abstract_type_general
            ),
            % For the `.int2' files, we need the full definitions of
            % discriminated union types.  Even if the functors for a type
            % are not used within a module, we may need to know them for
            % comparing insts, e.g. for comparing `ground' and `bound(...)'.
            ShortInterfaceKind = int3
        ;
            TypeDefn = parse_tree_abstract_type(AbstractDetails)
        ;
            TypeDefn = parse_tree_solver_type(_, _),
            % rafe: XXX we need to also export the details of the
            % forwarding type for the representation and the forwarding
            % pred for initialization.
            AbstractDetails = abstract_solver_type
        ;
            TypeDefn = parse_tree_eqv_type(_),
            % XXX is this right for solver types?
            AbstractDetails = abstract_type_general,
            % For the `.int2' files, we need the full definitions of
            % equivalence types. They are needed to ensure that
            % non-abstract equivalence types always get fully expanded
            % before code generation, even in modules that only indirectly
            % import the definition of the equivalence type.
            % But the full definitions are not needed for the `.int3'
            % files. So we convert equivalence types into abstract
            % types only for the `.int3' files.
            ShortInterfaceKind = int3
        ;
            TypeDefn = parse_tree_foreign_type(_, _, _),
            % We always need the definitions of foreign types
            % to handle inter-language interfacing correctly.
            AbstractDetails = abstract_type_general,
            semidet_fail
        ),
        AbstractItemTypeDefn = ItemTypeDefn ^ td_ctor_defn
            := parse_tree_abstract_type(AbstractDetails),
        AbstractItem = item_type_defn(AbstractItemTypeDefn)
    ;
        Item = item_instance(ItemInstance),
        ShortInterfaceKind = int2,
        AbstractItemInstance = make_instance_abstract(ItemInstance),
        AbstractItem = item_instance(AbstractItemInstance)
    ;
        Item = item_typeclass(ItemTypeClass),
        AbstractItemTypeClass = ItemTypeClass ^ tc_class_methods
            := class_interface_abstract,
        AbstractItem = item_typeclass(AbstractItemTypeClass)
    ).

:- pred du_type_is_enum(list(constructor)::in, int::out) is semidet.

du_type_is_enum(Ctors, NumBits) :-
    Ctors = [_, _ | _],
    all [Ctor] (
        list.member(Ctor, Ctors)
    => (
        Ctor = ctor(ExistQTVars, ExistConstraints, _Name, Args, _Context),
        ExistQTVars = [],
        ExistConstraints = [],
        Args = []
    )),
    list.length(Ctors, NumFunctors),
    int.log2(NumFunctors, NumBits).

:- pred make_abstract_unify_compare(item::in, short_interface_kind::in,
    item::out) is semidet.

make_abstract_unify_compare(Item, int2, AbstractItem) :-
    Item = item_type_defn(ItemTypeDefn),
    TypeDefn = ItemTypeDefn ^ td_ctor_defn,
    (
        TypeDefn = parse_tree_du_type(Constructors, yes(_UserEqComp),
            MaybeDirectArgCtors),
        MaybeUserEqComp = yes(abstract_noncanonical_type(non_solver_type)),
        AbstractTypeDefn = parse_tree_du_type(Constructors, MaybeUserEqComp,
            MaybeDirectArgCtors)
    ;
        TypeDefn = parse_tree_foreign_type(ForeignType,
            yes(_UserEqComp), Assertions),
        AbstractTypeDefn = parse_tree_foreign_type(ForeignType,
            yes(abstract_noncanonical_type(non_solver_type)), Assertions)
    ;
        TypeDefn = parse_tree_solver_type(SolverTypeDetails, yes(_UserEqComp)),
        AbstractTypeDefn = parse_tree_solver_type(SolverTypeDetails,
            yes(abstract_noncanonical_type(solver_type)))
    ),
    AbstractItemTypeDefn = ItemTypeDefn ^ td_ctor_defn := AbstractTypeDefn,
    AbstractItem = item_type_defn(AbstractItemTypeDefn).

    % All instance declarations must be written to `.int' files as
    % abstract instance declarations, because the method names have not yet
    % been module qualified. This could cause the wrong predicate to be
    % used if calls to the method are specialized.
    %
:- func make_instance_abstract(item_instance_info) = item_instance_info.

make_instance_abstract(Info0) = Info :-
    Info = Info0 ^ ci_method_instances := instance_body_abstract.

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

:- pred maybe_return_timestamp(maybe(T)::in, maybe_return_timestamp::out)
    is det.

maybe_return_timestamp(yes(_), do_return_timestamp).
maybe_return_timestamp(no, do_not_return_timestamp).

:- pred maybe_record_timestamp(module_name::in, string::in, need_qualifier::in,
    maybe(timestamp)::in, module_and_imports::in, module_and_imports::out)
    is det.

maybe_record_timestamp(ModuleName, Suffix, NeedQualifier, MaybeTimestamp,
        !Module) :-
    (
        !.Module ^ mai_maybe_timestamps = yes(Timestamps0),
        (
            MaybeTimestamp = yes(Timestamp),
            TimestampInfo = module_timestamp(Suffix, Timestamp, NeedQualifier),
            map.set(ModuleName, TimestampInfo, Timestamps0, Timestamps),
            !Module ^ mai_maybe_timestamps := yes(Timestamps)
        ;
            MaybeTimestamp = no
        )
    ;
        !.Module ^ mai_maybe_timestamps = no
    ).

%-----------------------------------------------------------------------------%
:- end_module modules.
%-----------------------------------------------------------------------------%
