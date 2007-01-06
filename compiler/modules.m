%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2007 The University of Melbourne.
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

:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module relation.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Succeeds iff the module referred to by the module name is one
    % of the modules in the standard library.
    %
:- pred mercury_std_library_module_name(module_name::in) is semidet.

    % module_name_to_file_name(Module, Extension, Mkdir, FileName):
    %
    % Convert a module name and file extension to the corresponding file name.
    % If `MkDir' is yes, then create any directories needed.
    %
    % Currently we use the convention that the module `foo.bar.baz' should be
    % named `foo.bar.baz.m', and allow other naming conventions with the
    % `-f' option.
    %
    % Note that this predicate is also used to create some "phony" Makefile
    % targets that do not have corresponding files, e.g. `<foo>.clean'.
    %
:- pred module_name_to_file_name(module_name::in, string::in, bool::in,
    file_name::out, io::di, io::uo) is det.

    % module_name_to_search_file_name(Module, Extension, FileName):
    %
    % As above, but for a file which might be in an installed library,
    % not the current directory.
    %
    % With `--use-grade-subdirs', the current directory's `.mih' files are in
    % `Mercury/<grade>/<arch>/Mercury/mihs', and those for installed libraries
    % are in `<prefix>/lib/mercury/lib/<grade>/<arch>/inc/Mercury/mihs'.
    %
    % handle_options.m sets up the `--c-include-directory' options so that
    % the name `<module>.mih' should be used in a context which requires
    % searching for the `.mih files, for example in a C file.
    %
    % module_name_to_file_name would return
    % `Mercury/<grade>/<arch>/Mercury/mihs/<module>.mihs',
    % which would be used when writing or removing the `.mih' file.
    %
:- pred module_name_to_search_file_name(module_name::in, string::in,
    file_name::out, io::di, io::uo) is det.

    % module_name_to_lib_file_name(Prefix, Module, Extension, MkDir,
    %       FileName):
    %
    % Like module_name_to_file_name, but also allows a prefix.
    %
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name(string::in, module_name::in, string::in,
    bool::in, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name(Module, FactTableFileName, Ext, MkDir, FileName):
    % Returns the filename to use when compiling fact table files.
    % If 'MkDir' is yes, then create any directories needed.
    %
:- pred fact_table_file_name(module_name::in, file_name::in, string::in,
    bool::in, file_name::out, io::di, io::uo) is det.

    % Convert a file name (excluding the trailing `.m') to the corresponding
    % module name.
    %
:- pred file_name_to_module_name(file_name::in, module_name::out) is det.

    % Convert a module name to a file name stem (e.g. foo.bar.baz).
    %
:- pred module_name_to_file_name(module_name::in, file_name::out) is det.

    % Convert a module name to something that is suitable
    % for use as a variable name in makefiles.
    %
:- pred module_name_to_make_var_name(module_name::in, string::out) is det.

    % Generate the list of .NET DLLs which could be referred to by this module
    % (including the module itself).
    %
    % If we are compiling a module within the standard library we should
    % reference the runtime DLLs and all other library DLLs.  If we are
    % outside the library we should just reference mercury.dll (which will
    % contain all the DLLs).
    %
:- func referenced_dlls(module_name, list(module_name)) = list(module_name).

%-----------------------------------------------------------------------------%

    % read_mod(ModuleName, Extension, Descr, Search, ReturnTimestamp,
    %       Items, Error, SourceFileName, MaybeTimestamp):
    % Given a module name and a file extension (e.g. `.m', `.int', or `int2'),
    % read in the list of items in that file.
    %
    % If Extension is ".m", and ModuleName is a nested module, then try
    % searching for different filenames: for modules such as `foo.bar.baz.m'
    % search first for `foo.bar.baz.m', then `bar.baz.m', then `baz.m'.
    % If Search is yes, search all directories given by the option
    % search_directories for the module.
    % If ReturnTimestamp is yes, attempt to return the modification time
    % of the file in MaybeTimestamp.
    % If the actual module name (as determined by the `:- module' declaration)
    % does not match the specified module name, then report an error message.
    % Return the actual source file name found (excluding the directory part).
    %
    % N.B. This reads a module given the module name. If you want to read
    % a module given the file name, use `read_mod_from_file'.
    %
:- pred read_mod(module_name::in, string::in, string::in, bool::in, bool::in,
    item_list::out, module_error::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

    % read_mod_if_changed(ModuleName, Extension, Descr, Search,
    %   OldTimestamp, Items, Error, SourceFileName, MaybeTimestamp):
    %
    % If the timestamp of the file specified by the given module name and
    % file extension is newer than OldTimestamp, read the file, returning
    % the new timestamp.
    %
    % If the file was read, MaybeTimestamp will contain the new timestamp.
    % If the timestamp was unchanged, MaybeTimestamp will be
    % `yes(OldTimestamp)'. If the file could not be read, MaybeTimestamp
    % will be `no'.
    %
:- pred read_mod_if_changed(module_name::in, string::in, string::in, bool::in,
    timestamp::in, item_list::out, module_error::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

    % Similar to read_mod, but doesn't return error messages.
    %
:- pred read_mod_ignore_errors(module_name::in, string::in, string::in,
    bool::in, bool::in, item_list::out, module_error::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

    % read_mod_from_file(SourceFileName, Extension, Descr, Search,
    %   ReturnTimestamp, Items, Error, ModuleName, MaybeTimestamp):
    %
    % Given a file name and a file extension (e.g. `.m', `.int', or `int2'),
    % read in the list of items in that file.
    % If Search is yes, search all directories given by the option
    % search_directories for the module.
    % If ReturnTimestamp is yes, attempt to return the modification time
    % of the file in MaybeTimestamp. Return the module name (as determined
    % by the `:- module' declaration, if any).
    %
    % N.B.  This reads a module given the file name. If you want to read a
    % module given the module name, use `read_mod'.
    %
:- pred read_mod_from_file(file_name::in, string::in, string::in, bool::in,
    bool::in, item_list::out, module_error::out, module_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % make_private_interface(SourceFileName, SourceFileModuleName,
    %   ModuleName, MaybeTimestamp, Items):
    %
    % Given a source file name and module name, the timestamp of the source
    % file, and the list of items in that module, output the private (`.int0')
    % interface file for the module. (The private interface contains all the
    % declarations in the module, including those in the `implementation'
    % section; it is used when compiling sub-modules.)
    %
:- pred make_private_interface(file_name::in, module_name::in, module_name::in,
    maybe(timestamp)::in, item_list::in, io::di, io::uo) is det.

    % make_interface(SourceFileName, SourceFileModuleName,
    %   ModuleName, MaybeTimestamp, Items):
    %
    % Given a source file name and module name, the timestamp of the source
    % file, and the list of items in that module, output the long (`.int')
    % and short (`.int2') interface files for the module.
    %
:- pred make_interface(file_name::in, module_name::in, module_name::in,
    maybe(timestamp)::in, item_list::in, io::di, io::uo) is det.

    % Output the unqualified short interface file to <module>.int3.
    %
:- pred make_short_interface(file_name::in, module_name::in, item_list::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % The `module_imports' structure holds information about
    % a module and the modules that it imports.

    % Note that we build this structure up as we go along.
    % When generating the dependencies (for `--generate-dependencies'),
    % the two fields that hold the direct imports do not include
    % the imports via ancestors when the module is first read in;
    % the ancestor imports are added later, once all the modules
    % have been read in.  Similarly the indirect imports field is
    % initially set to the empty list and filled in later.
    %
    % When compiling or when making interface files, the same
    % sort of thing applies: initially all the list(module_name) fields
    % except the public children field are set to empty lists,
    % and then we add ancestor modules and imported modules
    % to their respective lists as we process the interface files
    % for those imported or ancestor modules.

:- type module_imports --->
        module_imports(
            % The source file.
            source_file_name            :: file_name,

            % The name of the top-level module in the source file
            % containing the module that we are compiling.
            source_file_module_name     :: module_name,

            % The module (or sub-module) that we are compiling.
            module_name                 :: module_name,

            % The list of ancestor modules it inherits.
            parent_deps                 :: list(module_name),

            % The list of modules it directly imports in the interface
            % (imports via ancestors count as direct).
            int_deps                    :: list(module_name),

            % The list of modules it directly imports in the implementation.
            impl_deps                   :: list(module_name),

            % The list of modules it indirectly imports.
            indirect_deps               :: list(module_name),

            children                    :: list(module_name),

            % The list of its public children, i.e. child modules that
            % it includes in the interface section.
            public_children             :: list(module_name),

            % The modules included in the same source file. This field
            % is only set for the top-level module in each file.
            nested_children             :: list(module_name),

            % The list of filenames for fact tables in this module.
            fact_table_deps             :: list(string),

            % Whether or not the module contains foreign code, and if yes,
            % which languages they use.
            has_foreign_code            :: contains_foreign_code,

            % The `:- pragma foreign_import_module' declarations.
            foreign_import_modules      :: foreign_import_module_info_list,

            % Does the module contain any `:- pragma export' declarations?
            contains_foreign_export     :: contains_foreign_export,

            % The contents of the module and its imports.
            items                       :: item_list,

            % Whether an error has been encountered when reading in
            % this module.
            error                       :: module_error,

            % If we are doing smart recompilation, we need to keep
            % the timestamps of the modules read in.
            maybe_timestamps            :: maybe(module_timestamps),

            % Does this module contain main/2?
            has_main                    :: has_main,

            % The directory containing the module source.
            module_dir                  :: dir_name
        ).

:- type contains_foreign_code
    --->    contains_foreign_code(set(foreign_language))
    ;       no_foreign_code
    ;       unknown.

:- type contains_foreign_export
    --->    contains_foreign_export
    ;       no_foreign_export.

:- type has_main
    --->    has_main
    ;       no_main.

    % When doing smart recompilation record for each module
    % the suffix of the file that was read and the modification
    % time of the file.
:- type module_timestamps == map(module_name, module_timestamp).
:- type module_timestamp
    --->    module_timestamp(
                suffix          :: string,
                timestamp       :: timestamp,
                need_qualifier  :: need_qualifier
            ).

    % recompilation_check.m records each file read to avoid
    % reading it again. The string is the suffix of the file
    % name.
:- type read_modules == map(pair(module_name, string), read_module).

:- type read_module
    --->    read_module(
                module_timestamp,
                item_list,
                module_error,
                file_name
            ).

    % find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp,
    %   Items, MaybeTimestamp, Error, FileName)
    %
    % Check whether a file was read during recompilation checking.
    %
:- pred find_read_module(read_modules::in, module_name::in, string::in,
    bool::in, item_list::out, maybe(timestamp)::out, module_error::out,
    file_name::out) is semidet.

    % Some access predicates for the module_imports structure.

:- pred module_imports_get_source_file_name(module_imports::in, file_name::out)
    is det.
:- pred module_imports_get_module_name(module_imports::in, module_name::out)
    is det.
:- pred module_imports_get_impl_deps(module_imports::in,
    list(module_name)::out) is det.
:- pred module_imports_get_items(module_imports::in, item_list::out) is det.
:- pred module_imports_get_error(module_imports::in, module_error::out) is det.

:- pred module_imports_set_items(item_list::in,
    module_imports::in, module_imports::out) is det.
:- pred module_imports_set_error(module_error::in,
    module_imports::in, module_imports::out) is det.

    % Set the interface dependencies.
    %
:- pred module_imports_set_int_deps(list(module_name)::in,
    module_imports::in, module_imports::out) is det.

    % Set the implementation dependencies.
    %
:- pred module_imports_set_impl_deps(list(module_name)::in,
    module_imports::in, module_imports::out) is det.

    % Set the indirect dependencies.
    %
:- pred module_imports_set_indirect_deps(list(module_name)::in,
    module_imports::in, module_imports::out) is det.

    % Make an item_and_context for a module declaration or pseudo-declaration
    % such as `:- imported' (which is inserted by the compiler, but can't be
    % used in user code).
    %
:- func make_pseudo_decl(module_defn) = item_and_context.

    % append_pseudo_decl(PseudoDecl, Module0, Module):
    %
    % Append the specified module declaration to the list of items in Module0
    % to give Module.
    %
:- pred append_pseudo_decl(module_defn::in, module_imports::in,
    module_imports::out) is det.

    % replace_section_decls(IntStatusItem, ImpStatusItem, !Items):
    %
    % Replace all occurrences of `:- interface' with IntStatusItem
    % (this will usually be an item which sets the import status).
    % Replace all occurrences of `:- implementation' with ImpStatusItem.
    %
:- pred replace_section_decls(item_and_context::in, item_and_context::in,
    item_list::in, item_list::out) is det.

    % Remove all the imported items the list.
    %
:- pred strip_imported_items(item_list::in, item_list::out) is det.

%-----------------------------------------------------------------------------%

:- type module_list == assoc_list(module_name, item_list).

    % Given a module (well, a list of items), split it into its constituent
    % sub-modules, in top-down order.
    % Also do some error checking:
    % - report an error if the `implementation' section of a sub-module
    %   is contained inside the `interface' section of its parent module
    % - check for modules declared as both nested and separate sub-modules.
    % - check for non-abstract typeclass instance declarations in module
    %   interfaces.
    %
:- pred split_into_submodules(module_name::in, item_list::in, module_list::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

    % grab_imported_modules(SourceFileName, SourceFileModuleName,
    %   ModuleName, NestedSubModules, ReadModules, ModuleTimestamp, Items,
    %   Module, Error):
    %
    % Given a source file name and the top-level module name in that file,
    % the current module name, the nested sub-modules in the file if this
    % module is the top-level module, the timestamp of the file SourceFileName
    % and the list of items in the current module, read in the private
    % interface files for all the parent modules, the long interface files
    % for all the imported modules, and the short interface files for all
    % the indirectly imported modules, and return a `module_imports' structure
    % containing the relevant information. ReadModules contains the interface
    % files read during recompilation checking.
    %
:- pred grab_imported_modules(file_name::in, module_name::in, module_name::in,
    list(module_name)::in, read_modules::in, maybe(timestamp)::in,
    item_list::in, module_imports::out, module_error::out,
    io::di, io::uo) is det.

    % grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
    %   ModuleName, Items, Module, Error):
    %
    % Similar to grab_imported_modules, but only reads in the unqualified
    % short interfaces (.int3s), and the .int0 files for parent modules,
    % instead of reading the long interfaces and qualified short interfaces
    % (.int and int2s). Does not set the `PublicChildren' or `FactDeps'
    % fields of the module_imports structure.
    %
:- pred grab_unqual_imported_modules(file_name::in, module_name::in,
    module_name::in, item_list::in, module_imports::out, module_error::out,
    io::di, io::uo) is det.

    % process_module_private_interfaces(Ancestors,
    %   IntStatusItem, ImpStatusItem, !DirectImports, !DirectUses, !Module):
    %
    % Read the complete private interfaces for modules in Ancestors, and
    % append any imports/uses in the ancestors to the corresponding previous
    % lists.
    %
:- pred process_module_private_interfaces(read_modules::in,
    list(module_name)::in, item_and_context::in, item_and_context::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_imports::in, module_imports::out, io::di, io::uo) is det.

    % process_module_long_interfaces(ReadModules, NeedQualifier, Imports,
    %   Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
    %   !ImplIndirectImports, !Module):
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
:- pred process_module_long_interfaces(read_modules::in, need_qualifier::in,
    list(module_name)::in, string::in,
    item_and_context::in, item_and_context::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_imports::in, module_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces_transitively(ReadModules,
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
:- pred process_module_short_interfaces_transitively(read_modules::in,
    list(module_name)::in, string::in,
    item_and_context::in, item_and_context::in,
    list(module_name)::in, list(module_name)::out,
    module_imports::in, module_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces_and_impls_transitively(ReadModules,
    %   IndirectImports, Ext, IntStatusItem, ImpStatusItem, !Module):
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
:- pred process_module_short_interfaces_and_impls_transitively(
    read_modules::in, list(module_name)::in, string::in,
    item_and_context::in, item_and_context::in,
    module_imports::in, module_imports::out, io::di, io::uo) is det.

    % process_module_short_interfaces(ReadModules,
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
:- pred process_module_short_interfaces(read_modules::in,
    list(module_name)::in, string::in, item_and_context::in,
    item_and_context::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    module_imports::in, module_imports::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % write_dependency_file(Module, AllDeps, MaybeTransOptDeps):
    %
    % Write out the per-module makefile dependencies (`.d') file for the
    % specified module. AllDeps is the set of all module names which the
    % generated code for this module might depend on, i.e. all that have been
    % used or imported, directly or indirectly, into this module, including
    % via .opt or .trans_opt files, and including parent modules of nested
    % modules. MaybeTransOptDeps is a list of module names which the
    % `.trans_opt' file may depend on.  This is set to `no' if the
    % dependency list is not available.
    %
:- pred write_dependency_file(module_imports::in, set(module_name)::in,
    maybe(list(module_name))::in, io::di, io::uo) is det.

    % maybe_read_dependency_file(ModuleName, MaybeTransOptDeps):
    %
    % If transitive intermodule optimization has been enabled, then read
    % <ModuleName>.d to find the modules which <ModuleName>.trans_opt may
    % depend on.  Otherwise return `no'.
    %
:- pred maybe_read_dependency_file(module_name::in,
    maybe(list(module_name))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % generate_module_dependencies(ModuleName):
    %
    % Generate the per-program makefile dependencies (`.dep') file for a
    % program whose top-level module is `ModuleName'. This involves first
    % transitively reading in all imported or ancestor modules. While we're
    % at it, we also save the per-module makefile dependency (`.d') files
    % for all those modules.
    %
:- pred generate_module_dependencies(module_name::in, io::di, io::uo) is det.

    % generate_file_dependencies(FileName):
    %
    % Same as generate_module_dependencies, but takes a file name instead of
    % a module name.
    %
:- pred generate_file_dependencies(file_name::in, io::di, io::uo) is det.

    % generate_module_dependency_file(ModuleName):
    %
    % Generate the per module makefile dependency ('.d') file for the
    % given module.
    %
:- pred generate_module_dependency_file(module_name::in, io::di, io::uo) is det.

    % generate_file_dependency_file(FileName):
    %
    % Same as generate_module_dependency_file, but takes a file name instead of
    % a module name.
    %
:- pred generate_file_dependency_file(file_name::in, io::di, io::uo) is det.

    % add_module_relations(LookupModuleImports, ModuleName,
    %   !IntDepsRel, !ImplDepsRel)
    %
    % Add a module's interface and implementation dependencies to IntDepsRel
    % and ImplDepsRel respectively.  Dependencies are found using the
    % LookupModuleImports function.
    %
:- pred add_module_relations(lookup_module_imports::lookup_module_imports,
    module_name::in, relation(module_name)::in, relation(module_name)::out,
    relation(module_name)::in, relation(module_name)::out) is det.

:- type lookup_module_imports == (func(module_name) = module_imports).
:- mode lookup_module_imports == in(func(in) = out is det).

    % get_dependencies(Items, ImportDeps, UseDeps):
    %
    % Get the list of modules that a list of items (explicitly) depends on.
    % ImportDeps is the list of modules imported using `:- import_module',
    % UseDeps is the list of modules imported using `:- use_module'.
    % N.B. Typically you also need to consider the module's implicit
    % dependencies (see get_implicit_dependencies/3), its parent modules
    % (see get_ancestors/1) and possibly also the module's child modules
    % (see get_children/2). You may also need to consider indirect
    % dependencies.
    %
:- pred get_dependencies(item_list::in, list(module_name)::out,
    list(module_name)::out) is det.

    % get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps):
    %
    % Get the list of builtin modules (e.g. "public_builtin",
    % "private_builtin") that a list of items may implicitly depend on.
    % ImportDeps is the list of modules which should be automatically
    % implicitly imported as if via `:- import_module', and UseDeps is
    % the list which should be automatically implicitly imported as if via
    % `:- use_module'.
    %
:- pred get_implicit_dependencies(item_list::in, globals::in,
    list(module_name)::out, list(module_name)::out) is det.

    % get_ancestors(ModuleName) =  ParentDeps:
    %
    % ParentDeps is the list of ancestor modules for this module, oldest first;
    % e.g. if the ModuleName is `foo.bar.baz', then ParentDeps would be
    % [`foo', `foo.bar'].
    %
:- func get_ancestors(module_name) = list(module_name).

    % init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
    %   Error, Globals, ModuleName - Items, ModuleImports).
    %
:- pred init_dependencies(file_name::in, module_name::in,
    list(module_name)::in, module_error::in, globals::in,
    pair(module_name, item_list)::in, module_imports::out) is det.

%-----------------------------------------------------------------------------%

    % touch_interface_datestamp(ModuleName, Ext):
    %
    % Touch the datestamp file `ModuleName.Ext'. Datestamp files
    % are used to record when each of the interface files was last
    % updated.
    %
:- pred touch_interface_datestamp(module_name::in, string::in,
    io::di, io::uo) is det.

    % touch_datestamp(FileName):
    %
    % Update the modification time for the given file,
    % clobbering the contents of the file.
    %
:- pred touch_datestamp(file_name::in, io::di, io::uo) is det.

    % update_interface_return_succeeded(FileName, Succeeded):
    %
    % Call the shell script mercury_update_interface to update the
    % interface file FileName from FileName.tmp if it has changed.
    %
:- pred update_interface_return_succeeded(file_name::in, bool::out,
    io::di, io::uo) is det.

:- pred update_interface(file_name::in, io::di, io::uo) is det.

    % maybe_make_symlink(TargetFile, LinkName, Result):
    %
    % If `--use-symlinks' is set, attempt to make LinkName a
    % symlink pointing to LinkTarget.
    %
:- pred maybe_make_symlink(file_name::in, file_name::in, bool::out,
    io::di, io::uo) is det.

    % copy_file(Source, Destination, Succeeded).
    %
    % XXX This belongs in the standard library.
    %
:- pred copy_file(file_name::in, file_name::in, io.res::out,
    io::di, io::uo) is det.

    % make_symlink_or_copy_file(LinkTarget, LinkName, Succeeded):
    %
    % Attempt to make LinkName a symlink pointing to LinkTarget,
    % copying LinkTarget to LinkName if that fails (or if
    % `--use-symlinks' is not set).
    %
:- pred make_symlink_or_copy_file(file_name::in, file_name::in, bool::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Check whether a particular `pragma' declaration is allowed
    % in the interface section of a module.
    %
:- pred pragma_allowed_in_interface(pragma_type::in, bool::out) is det.

    % Given a module name and a list of the items in that module,
    % this procedure checks if the module doesn't export anything,
    % and if so, and --warn-nothing-exported is set, it reports
    % a warning.
    %
:- pred check_for_no_exports(item_list::in, module_name::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Java command-line tools utilities
%

    % Create a shell script with the same name as the given module to invoke
    % Java with the appropriate options on the class of the same name.
    %
:- pred create_java_shell_script(module_name::in, bool::out,
    io::di, io::uo) is det.

    % Strip away the path prefix for a list of .class files.
    %
:- pred list_class_files_for_jar(module_name::in, string::in, string::out,
    io::di, io::uo) is det.

    % Get the value of the Java class path from the environment. (Normally
    % it will be obtained from the CLASSPATH environment variable, but if
    % that isn't present then the java.class.path variable may be used instead.
    % This is used for the Java back-end, which doesn't support environment
    % variables properly.)
    %
:- pred get_env_classpath(string::out, io::di, io::uo) is det.

    % get_install_name_option(FileName, Option, !IO):
    %
    % Get the option string for setting the install-name of the shared library
    % FileName. This is only used for systems which support the install-name
    % option for shared libraries (such as Darwin).
    %
:- pred get_install_name_option(string::in, string::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module make.              % XXX undesirable dependency
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.source_file_map.
:- import_module recompilation.version.

:- import_module char.
:- import_module dir.
:- import_module getopt_io.
:- import_module int.
:- import_module library.
:- import_module multi_map.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module string.
:- import_module svmap.
:- import_module svrelation.
:- import_module svset.
:- import_module term.
:- import_module unit.
:- import_module varset.

%-----------------------------------------------------------------------------%

mercury_std_library_module_name(unqualified(Name)) :-
    mercury_std_library_module(Name).
mercury_std_library_module_name(qualified(Module, Name)) :-
    module_name_to_file_name(qualified(Module, Name), ModuleNameStr), 
    mercury_std_library_module(ModuleNameStr).
mercury_std_library_module_name(qualified(Module, Name)) :-
    strip_outermost_qualifier(qualified(Module, Name), "mercury", ModuleName),
    module_name_to_file_name(ModuleName, ModuleNameStr), 
    mercury_std_library_module(ModuleNameStr).

module_name_to_search_file_name(ModuleName, Ext, FileName, !IO) :-
    module_name_to_file_name(ModuleName, Ext, yes, no, FileName, !IO).

module_name_to_file_name(ModuleName, Ext, MkDir, FileName, !IO) :-
    module_name_to_file_name(ModuleName, Ext, no, MkDir, FileName, !IO).

:- pred module_name_to_file_name(module_name::in, string::in, bool::in,
    bool::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name(ModuleName, Ext, Search, MkDir, FileName, !IO) :-
    ( Ext = ".m" ->
        % Look up the module in the module->file mapping.
        source_file_map.lookup_module_source_file(ModuleName, FileName, !IO)
    ;
        string.append(sym_name_to_string(ModuleName), Ext, BaseName),
        choose_file_name(ModuleName, BaseName, Ext, Search, MkDir, FileName,
            !IO)
    ).

module_name_to_lib_file_name(Prefix, ModuleName, Ext, MkDir, FileName, !IO) :-
    BaseFileName = sym_name_to_string(ModuleName),
    string.append_list([Prefix, BaseFileName, Ext], BaseName),
    choose_file_name(ModuleName, BaseName, Ext, no, MkDir, FileName, !IO).

fact_table_file_name(ModuleName, FactTableFileName, Ext, MkDir, FileName,
        !IO) :-
    extra_link_obj_file_name(ModuleName, FactTableFileName, Ext, MkDir,
        FileName, !IO).

    % extra_link_obj_file_name(Module, ExtraLinkObjName,
    %   Ext, MkDir, FileName):
    % Returns the filename to use when compiling extra objects that must be
    % linked into the executable (currently used only for fact tables).
    % If `MkDir' is yes, make any directories necessary.
    %
:- pred extra_link_obj_file_name(module_name::in, file_name::in, string::in,
    bool::in, file_name::out, io::di, io::uo) is det.

extra_link_obj_file_name(ModuleName, ExtraLinkObjName, Ext, MkDir, FileName,
        !IO) :-
    string.append(ExtraLinkObjName, Ext, BaseName),
    choose_file_name(ModuleName, BaseName, Ext, no, MkDir, FileName, !IO).

:- pred choose_file_name(module_name::in, string::in, string::in, bool::in,
    bool::in, file_name::out, io::di, io::uo) is det.

choose_file_name(_ModuleName, BaseName, Ext, Search, MkDir, FileName, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_string_option(Globals, library_extension, LibExt),
    globals.lookup_string_option(Globals, shared_library_extension,
        SharedLibExt),
    (
        (
            UseSubdirs = no
        ;
            % If we're searching for (rather than writing) a `.mih' file,
            % use the plain file name.  This is so that searches for files
            % in installed libraries will work.  `--c-include-directory' is
            % set so that searches for files in the current directory will
            % work.
            %
            Search = yes,
            ( Ext = ".mih"
            ; Ext = ".mih.tmp"
            )
        )
    ->
        FileName = BaseName
    ;
        %
        % The source files, the final executables, library files (including
        % .init files) output files intended for use by the user, and phony
        % Mmake targets names go in the current directory
        %
        \+ (
            UseGradeSubdirs = yes,
            file_is_arch_or_grade_dependent(Globals, Ext)
        ),
        (
            % Executable files.
            ( Ext = ""
            ; Ext = ".exe"
            ; Ext = ".dll"
            % Library files.
            ; Ext = ".a"
            ; Ext = ".$A"
            ; Ext = ".so"
            ; Ext = ".dylib"
            ; Ext = ".$(EXT_FOR_SHARED_LIB)"
            ; Ext = ".jar"
            ; Ext = ".init"
                    % mercury_update_interface
                    % requires the `.init.tmp' files to
                    % be in the same directory as the
                    % `.init' files.
            ; Ext = ".init.tmp"

            % output files intended for use by the user
            % (the .h_dump* and .c_dump* MLDS dumps
            % also fit into this category, but for
            % efficiency, to keep this as a switch,
            % we deal with them below)
            ; Ext = ".mh"
                    % mercury_update_interface
                    % requires the `.mh.tmp' files to
                    % be in the same directory as the
                    % `.mh' files.
            ; Ext = ".mh.tmp"
            ; Ext = ".err"
            ; Ext = ".ugly"
            ; Ext = ".hlds_dump"
            ; Ext = ".mlds_dump"
            ; Ext = ".dependency_graph"
            ; Ext = ".order"
            % Mmake targets
            ; Ext = ".clean"
            ; Ext = ".realclean"
            ; Ext = ".depend"
            ; Ext = ".install_ints"
            ; Ext = ".install_opts"
            ; Ext = ".install_hdrs"
            ; Ext = ".install_grade_hdrs"
            ; Ext = ".check"
            ; Ext = ".ints"
            ; Ext = ".int3s"
            ; Ext = ".ss"
            ; Ext = ".pic_ss"
            ; Ext = ".ils"
            ; Ext = ".javas"
            ; Ext = ".classes"
            ; Ext = ".opts"
            ; Ext = ".trans_opts"
            )
        ;
            % Output files intended for use by the user.
            %
            ( string.prefix(Ext, ".c_dump")
            ; string.prefix(Ext, ".mih_dump")
            )
        )
    ->
        FileName = BaseName
    ;
        %
        % We need to handle a few cases specially.
        %
        (
            ( Ext = ".dir/*.o"
            ; Ext = ".dir/*.$O"
            )
        ->
            SubDirName = "dirs"
        ;
            % .$O, .pic_o and .lpic_o files need to go in the
            % same directory, so that using
            % .$(EXT_FOR_PIC_OBJECTS) will work.
            ( Ext = ".o"
            ; Ext = ".$O"
            ; Ext = ".lpic_o"
            ; Ext = ".pic_o"
            ; Ext = "$(EXT_FOR_PIC_OBJECTS)"
            ; Ext = "_init.o"
            ; Ext = "_init.$O"
            ; Ext = "_init.lpic_o"
            ; Ext = "_init.pic_o"
            ; Ext = "_init.$(EXT_FOR_PIC_OBJECTS)"
            )
        ->
            SubDirName = "os"
        ;
            % _init.c, _init.s, _init.o etc. files
            % go in the cs, ss, os etc. subdirectories
            string.append("_init.", ExtName, Ext)
        ->
            string.append(ExtName, "s", SubDirName)
        ;
            % .int.tmp, .opt.tmp, etc. files
            % need to go in the ints, opts, etc. subdirectories
            string.append(".", ExtName0, Ext),
            string.remove_suffix(ExtName0, ".tmp", ExtName)
        ->
            string.append(ExtName, "s", SubDirName)
        ;
            % `.dv' files go in the `deps' subdirectory,
            % along with the `.dep' files
            Ext = ".dv"
        ->
            SubDirName = "deps"
        ;
            % Static and shared libraries go in the `lib' subdirectory.
            ( Ext = LibExt
            ; Ext = SharedLibExt
            )
        ->
            SubDirName = "lib"
        ;
            % The usual case: `*.foo' files go in the `foos' subdirectory.
            string.append(".", ExtName, Ext)
        ->
            string.append(ExtName, "s", SubDirName)
        ;
            Ext = ""
        ->
            SubDirName = "bin"
        ;
            string.append_list(["unknown extension `", Ext, "'"], ErrorMsg),
            unexpected(this_file, ErrorMsg)
        ),
        make_file_name(SubDirName, Search, MkDir, BaseName, Ext, FileName, !IO)
    ).

file_name_to_module_name(FileName, ModuleName) :-
    ModuleName = string_to_sym_name(FileName).

module_name_to_file_name(ModuleName, FileName) :-
    FileName = sym_name_to_string(ModuleName).

module_name_to_make_var_name(ModuleName, MakeVarName) :-
    MakeVarName = sym_name_to_string(ModuleName).

maybe_make_symlink(LinkTarget, LinkName, Result, !IO) :-
    globals.io_lookup_bool_option(use_symlinks, UseSymLinks, !IO),
    (
        UseSymLinks = yes,
        io.remove_file(LinkName, _, !IO),
        io.make_symlink(LinkTarget, LinkName, LinkResult, !IO),
        Result = ( if LinkResult = ok then yes else no )
    ;
        UseSymLinks = no,
        Result = no
    ).

copy_file(Source, Destination, Res, !IO) :-
    % Try to use the system's cp command in order to preserve metadata.
    globals.io_lookup_string_option(install_command, InstallCommand, !IO),
    Command = string.join_list("   ", list.map(quote_arg,
        [InstallCommand, Source, Destination])),
    io.output_stream(OutputStream, !IO),
    invoke_system_command(OutputStream, cmd_verbose, Command, Succeeded, !IO),
    (
        Succeeded = yes,
        Res = ok
    ;
        Succeeded = no,
        io.open_binary_input(Source, SourceRes, !IO),
        (
            SourceRes = ok(SourceStream),
            io.open_binary_output(Destination, DestRes, !IO),
            (
                DestRes = ok(DestStream),
                WriteByte = io.write_byte(DestStream),
                io.binary_input_stream_foldl_io(SourceStream, WriteByte, Res,
                    !IO),
                io.close_binary_input(SourceStream, !IO),
                io.close_binary_output(DestStream, !IO)
            ;
                DestRes = error(Error),
                Res = error(Error)
            )
        ;
            SourceRes = error(Error),
            Res = error(Error)
        )
    ).

make_symlink_or_copy_file(SourceFileName, DestinationFileName, Succeeded,
        !IO) :-
    globals.io_lookup_bool_option(use_symlinks, UseSymLinks, !IO),
    (
        UseSymLinks = yes,
        LinkOrCopy = "linking",
        io.make_symlink(SourceFileName, DestinationFileName, Result, !IO)
    ;
        UseSymLinks = no,
        LinkOrCopy = "copying",
        copy_file(SourceFileName, DestinationFileName, Result, !IO)
    ),
    (
        Result = ok,
        Succeeded = yes
    ;
        Result = error(Error),
        Succeeded = no,
        io.progname_base("mercury_compile", ProgName, !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": error ", !IO),
        io.write_string(LinkOrCopy, !IO),
        io.write_string(" `", !IO),
        io.write_string(SourceFileName, !IO),
        io.write_string("' to `", !IO),
        io.write_string(DestinationFileName, !IO),
        io.write_string("': ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO),
        io.flush_output(!IO)
    ).

:- pred make_file_name(dir_name::in, bool::in, bool::in, file_name::in,
    string::in, file_name::out, io::di, io::uo) is det.

make_file_name(SubDirName, Search, MkDir, BaseName, Ext, FileName, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_string_option(Globals, fullarch, FullArch),
    (
        UseGradeSubdirs = yes,
        file_is_arch_or_grade_dependent(Globals, Ext),
        %
        % If we're searching for (rather than writing) the file, just search
        % in Mercury/<ext>s. This is so that searches for files in installed
        % libraries work.  `--intermod-directories' is set so this will
        % work.
        %
        \+ (
            Search = yes,
            ( Ext = ".opt"
            ; Ext = ".trans_opt"
            ; Ext = ".analysis"
            ; Ext = ".imdg"
            ; Ext = ".request"
            )
        )
    ->
        grade_directory_component(Globals, Grade),

        % The extra "Mercury" is needed so we can use `--intermod-directory
        % Mercury/<grade>/<fullarch>' and `--c-include
        % Mercury/<grade>/<fullarch>' to find the local `.opt' and `.mih'
        % files without messing up the search for the files for installed
        % libraries.
        DirName = "Mercury"/Grade/FullArch/"Mercury"/SubDirName
    ;
        DirName = "Mercury"/SubDirName
    ),
    (
        MkDir = yes,
        make_directory(DirName, _, !IO)
    ;
        MkDir = no
    ),
    FileName = DirName/BaseName.

:- pred file_is_arch_or_grade_dependent(globals::in, string::in) is semidet.

file_is_arch_or_grade_dependent(_, Ext) :-
    file_is_arch_or_grade_dependent_2(Ext).
file_is_arch_or_grade_dependent(Globals, Ext0) :-
    string.append(Ext, ".tmp", Ext0), % for mercury_update_interface.
    file_is_arch_or_grade_dependent(Globals, Ext).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    globals.lookup_string_option(Globals, executable_file_extension, Ext).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    (
        globals.lookup_string_option(Globals,
            object_file_extension, ObjExt)
    ;
        globals.lookup_string_option(Globals,
            pic_object_file_extension, ObjExt)
    ;
        globals.lookup_string_option(Globals,
            link_with_pic_object_file_extension, ObjExt)
    ),
    ( Ext = ObjExt
    ; Ext = "_init" ++ ObjExt
    ).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    globals.lookup_string_option(Globals, library_extension, LibExt),
    Ext = LibExt.
file_is_arch_or_grade_dependent(Globals, Ext) :-
    globals.lookup_string_option(Globals, shared_library_extension, Ext).

:- pred file_is_arch_or_grade_dependent_2(string::in) is semidet.

    % The `.used' file isn't grade dependent itself, but it contains
    % information collected while compiling a grade-dependent
    % `.c', `il', etc file.
file_is_arch_or_grade_dependent_2(".used").
file_is_arch_or_grade_dependent_2(".opt").
file_is_arch_or_grade_dependent_2(".optdate").
file_is_arch_or_grade_dependent_2(".trans_opt").
file_is_arch_or_grade_dependent_2(".trans_opt_date").
file_is_arch_or_grade_dependent_2(".analysis").
file_is_arch_or_grade_dependent_2(".analysis_date").
file_is_arch_or_grade_dependent_2(".imdg").
file_is_arch_or_grade_dependent_2(".init").
file_is_arch_or_grade_dependent_2(".request").
file_is_arch_or_grade_dependent_2(".mih").
file_is_arch_or_grade_dependent_2(".c").
file_is_arch_or_grade_dependent_2(".c_date").
file_is_arch_or_grade_dependent_2(".s").
file_is_arch_or_grade_dependent_2(".s_date").
file_is_arch_or_grade_dependent_2(".pic_s").
file_is_arch_or_grade_dependent_2(".pic_s_date").
file_is_arch_or_grade_dependent_2(".il").
file_is_arch_or_grade_dependent_2(".il_date").
file_is_arch_or_grade_dependent_2(".java").
file_is_arch_or_grade_dependent_2(".java_date").
file_is_arch_or_grade_dependent_2(".class").
file_is_arch_or_grade_dependent_2(".dir").
file_is_arch_or_grade_dependent_2(".dll").
file_is_arch_or_grade_dependent_2(".$A").
file_is_arch_or_grade_dependent_2(".a").
file_is_arch_or_grade_dependent_2("_init.c").
file_is_arch_or_grade_dependent_2("_init.$O").

%-----------------------------------------------------------------------------%
%
% Private interfaces (.int0 files)
%

    % Read in the .int3 files that the current module depends on, and use
    % these to qualify all the declarations as much as possible. Then write
    % out the .int0 file.
    %
make_private_interface(SourceFileName, SourceFileModuleName, ModuleName,
        MaybeTimestamp, Items0, !IO) :-
    grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
        ModuleName, Items0, Module, Error, !IO),

    % Check whether we succeeded.
    % XXX zs: why does this code not check for fatal_module_errors?
    ( Error = some_module_errors ->
        module_name_to_file_name(ModuleName, ".int0", no, FileName, !IO),
        io.write_strings(["Error reading interface files.\n",
            "`", FileName, "' not written.\n"], !IO)
    ;
        % Module-qualify all items.
        module_imports_get_items(Module, Items1),
        globals.io_get_globals(Globals, !IO),
        module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
        module_qualify_items(Items1, Items2, map.init, _, Globals, ModuleName,
            yes(FileName), "", _, _, _, [], Specs),
        (
            Specs = [_ | _],
            sort_error_specs(Specs, SortedSpecs),
            write_error_specs(SortedSpecs, Globals, 0, _NumWarnings,
                0, _NumErrors, !IO),
            io.write_strings(["`", FileName, "' not written.\n"], !IO)
        ;
            Specs = [],

            % Write out the `.int0' file.
            %
            % XXX The following sequence of operations relies on the fact that
            % any reversals done while processing it are undone by subsequent
            % operations. Also, we should sort the contents of the .int0 file
            % as we do for the other types of interface file. We don't do that
            % at the moment because the code for doing that cannot handle
            % the structure of lists of items that represent private
            % interfaces.

            strip_imported_items(Items2, [], Items3),
            strip_clauses_from_interface(Items3, Items4),
            handle_mutables_in_private_interface(ModuleName, Items4, Items5),
            Items6 = list.map(make_item_and_context_abstract, Items5),
            list.reverse(Items6, Items),
            write_interface_file(SourceFileName, ModuleName,
                ".int0", MaybeTimestamp,
                [make_pseudo_decl(md_interface) | Items], !IO),
            touch_interface_datestamp(ModuleName, ".date0", !IO)
        )
    ).

:- func make_item_and_context_abstract(item_and_context) = item_and_context.

make_item_and_context_abstract(ItemAndContext0) = ItemAndContext :-
    ItemAndContext0 = item_and_context(Item0, Context),
    ( make_abstract_instance(Item0, Item) ->
        ItemAndContext = item_and_context(Item, Context)
    ;
        ItemAndContext = ItemAndContext0
    ).

    % Expand any mutable declarations in the item list into the pred and mode
    % declarations for their access predicates.  Only these components of a
    % mutable declaration should be written to a private interface file.
    %
:- pred handle_mutables_in_private_interface(module_name::in,
    item_list::in, item_list::out) is det.

 handle_mutables_in_private_interface(ModuleName, !Items) :-
    list.foldl(handle_mutable_in_private_interface(ModuleName), !.Items,
        [], !:Items).

:- pred handle_mutable_in_private_interface(module_name::in,
    item_and_context::in, item_list::in, item_list::out) is det.

handle_mutable_in_private_interface(ModuleName, ItemAndContext, !Items) :-
    ItemAndContext = item_and_context(Item, Context),
    ( Item = item_mutable(MutableName, Type, _Value, Inst, Attrs, _Varset) ->
        ConstantInterface = mutable_var_constant(Attrs),
        (
            ConstantInterface = yes,
            ConstantGetPredDecl =
                constant_get_pred_decl(ModuleName, MutableName, Type, Inst),
            list.cons(item_and_context(ConstantGetPredDecl, Context), !Items),
            ConstantSetPredDecl =
                constant_set_pred_decl(ModuleName, MutableName, Type, Inst),
            list.cons(item_and_context(ConstantSetPredDecl, Context), !Items)
        ;
            ConstantInterface = no,
            StdGetPredDecl =
                std_get_pred_decl(ModuleName, MutableName, Type, Inst),
            list.cons(item_and_context(StdGetPredDecl, Context), !Items),
            StdSetPredDecl =
                std_set_pred_decl(ModuleName, MutableName, Type, Inst),
            list.cons(item_and_context(StdSetPredDecl, Context), !Items),
            IOStateInterface = mutable_var_attach_to_io_state(Attrs),
            (
                IOStateInterface = yes,
                PureGetPredDecl =
                    io_get_pred_decl(ModuleName, MutableName, Type, Inst),
                list.cons(item_and_context(PureGetPredDecl, Context), !Items),
                PureSetPredDecl =
                    io_set_pred_decl(ModuleName, MutableName, Type, Inst),
                list.cons(item_and_context(PureSetPredDecl, Context), !Items)
            ;
                IOStateInterface = no
            )
        )
    ;
        list.cons(ItemAndContext, !Items)
    ).

%-----------------------------------------------------------------------------%

    % Read in the .int3 files that the current module depends on, and use these
    % to qualify all items in the interface as much as possible. Then write out
    % the .int and .int2 files.
    %
make_interface(SourceFileName, SourceFileModuleName, ModuleName,
        MaybeTimestamp, Items0, !IO) :-
    some [!InterfaceItems] (
        get_interface(ModuleName, yes, Items0, !:InterfaceItems),

        % Get the .int3 files for imported modules.
        grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
            ModuleName, !.InterfaceItems, Module0, Error, !IO),

        % Check whether we succeeded.
        module_imports_get_items(Module0, !:InterfaceItems),
        % XXX zs: why does this code not check for fatal_module_errors?
        ( Error = some_module_errors ->
            module_name_to_file_name(ModuleName, ".int", no, IntFileName, !IO),
            module_name_to_file_name(ModuleName, ".int2", no, Int2FileName,
                !IO),
            io.write_strings(["Error reading short interface files.\n",
                "`", IntFileName, "' and ",
                "`", Int2FileName, "' not written.\n"], !IO)
        ;
            % Module-qualify all items.
            globals.io_get_globals(Globals, !IO),
            module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
            module_qualify_items(!InterfaceItems, map.init, _, Globals,
                ModuleName, yes(FileName), "", _, _, _, [], Specs),

            % We want to finish writing the interface file (and keep
            % the exit status at zero) if we found some warnings.
            globals.io_set_option(halt_at_warn, bool(no), !IO),
            sort_error_specs(Specs, SortedSpecs),
            write_error_specs(SortedSpecs, Globals, 0, _NumWarnings,
                0, NumErrors, !IO),
            ( NumErrors > 0 ->
                module_name_to_file_name(ModuleName, ".int", no, IntFileName,
                    !IO),
                io.write_strings(["`", IntFileName, "' ", "not written.\n"],
                    !IO)
            ;
                % Strip out the imported interfaces, assertions are also
                % stripped since they should only be written to .opt files,
                % check for some warnings, and then write out the `.int'
                % and `int2' files and touch the `.date' file.

                strip_imported_items(!.InterfaceItems, [], !:InterfaceItems),
                strip_assertions(!InterfaceItems),
                strip_unnecessary_impl_defns(!InterfaceItems),
                check_for_clauses_in_interface(!InterfaceItems, !IO),
                check_int_for_no_exports(!.InterfaceItems, ModuleName, !IO),
                order_items(!InterfaceItems),
                write_interface_file(SourceFileName, ModuleName, ".int",
                    MaybeTimestamp, !.InterfaceItems, !IO),
                get_short_interface(!.InterfaceItems, int2,
                    ShortInterfaceItems),
                write_interface_file(SourceFileName, ModuleName, ".int2",
                    MaybeTimestamp, ShortInterfaceItems, !IO),
                touch_interface_datestamp(ModuleName, ".date", !IO)
            )
        )
    ).

make_short_interface(SourceFileName, ModuleName, Items0, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.

    get_interface(ModuleName, no, Items0, InterfaceItems0),
    % Assertions are also stripped since they should only be written
    % to .opt files.
    strip_assertions(InterfaceItems0, InterfaceItems1),
    check_for_clauses_in_interface(InterfaceItems1, InterfaceItems, !IO),
    get_short_interface(InterfaceItems, int3, ShortInterfaceItems0),
    globals.io_get_globals(Globals, !IO),
    module_qualify_items(ShortInterfaceItems0, ShortInterfaceItems,
        map.init, _, Globals, ModuleName, no, "", _, _, _, [], Specs),
    sort_error_specs(Specs, SortedSpecs),
    write_error_specs(SortedSpecs, Globals, 0, _NumWarnings, 0, _NumErrors,
        !IO),
    % XXX why do we do this even if there are some errors?
    write_interface_file(SourceFileName, ModuleName, ".int3",
        no, ShortInterfaceItems, !IO),
    touch_interface_datestamp(ModuleName, ".date3", !IO).

%-----------------------------------------------------------------------------%

strip_imported_items(Items0, Items) :-
    strip_imported_items(Items0, [], Items).

:- pred strip_imported_items(item_list::in, item_list::in, item_list::out)
    is det.

strip_imported_items([], !Items) :-
    list.reverse(!Items).
strip_imported_items([item_and_context(Item, Context) | Rest], !Items) :-
    ( Item = item_module_defn(_, md_imported(_)) ->
        list.reverse(!Items)
    ; Item = item_module_defn(_, md_used(_)) ->
        list.reverse(!Items)
    ; Item = item_module_defn(_, md_abstract_imported) ->
        list.reverse(!Items)
    ;
        strip_imported_items(Rest,
            [item_and_context(Item, Context) | !.Items], !:Items)
    ).

:- pred strip_assertions(item_list::in, item_list::out) is det.

strip_assertions([], []).
strip_assertions([item_and_context(Item, Context) | Rest], Items) :-
    ( Item = item_promise(promise_type_true, _, _, _) ->
        strip_assertions(Rest, Items)
    ;
        strip_assertions(Rest, Items0),
        Items = [item_and_context(Item, Context) | Items0]
    ).

%-----------------------------------------------------------------------------%

:- pred strip_unnecessary_impl_defns(item_list::in, item_list::out) is det.

strip_unnecessary_impl_defns(Items0, Items) :-
    some [!IntTypesMap, !ImplTypesMap, !ImplItems] (
        gather_type_defns(no, Items0, [], IntItems0, [], !:ImplItems,
            map.init, !:IntTypesMap, map.init, !:ImplTypesMap),

        % Work out which module imports in the implementation section of
        % the interface are required by the definitions of equivalence
        % types and dummy types in the implementation.
        get_requirements_of_impl_exported_types(!.IntTypesMap, !.ImplTypesMap,
            NecessaryDummyTypeCtors, NecessaryEqvTypeCtors,
            NecessaryTypeImplImports),

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
        map.map_values(make_impl_type_abstract, !ImplTypesMap),

        % If there is an exported type declaration for a type with an abstract
        % declaration in the implementation (usually it will originally
        % have been a d.u. type), remove the declaration in the implementation.
        FindAbstractExportedTypes =
            (pred(TypeCtor::out) is nondet :-
                map.member(!.ImplTypesMap, TypeCtor, Defns),
                \+ (
                    list.member(Defn, Defns),
                    Defn \= parse_tree_abstract_type(_) - _
                ),
                multi_map.contains(!.IntTypesMap, TypeCtor)
            ),
        solutions(FindAbstractExportedTypes, AbstractExportedTypes),
        RemoveFromImplTypesMap =
            (pred(TypeCtor::in, !.ImplTypesMap::in, !:ImplTypesMap::out)
                    is det :-
                multi_map.delete(!.ImplTypesMap, TypeCtor, !:ImplTypesMap)
            ),
        list.foldl(RemoveFromImplTypesMap, AbstractExportedTypes,
            !ImplTypesMap),

        AddProjectedItem =
            (pred((_ - Item)::in, !.ImplItems::in, !:ImplItems::out)
                    is det :-
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
        set.union(NecessaryDummyTypeCtors, NecessaryEqvTypeCtors,
            AllNecessaryTypeCtors),
        strip_unnecessary_impl_types(AllNecessaryTypeCtors, !ImplItems),
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

:- inst item_context(I) == bound(item_and_context(I, ground)).

:- inst one_module_spec ==  bound(list_module(bound([ground | bound([])]))).
:- inst import_item     ==  bound(item_module_defn(ground,
                                bound(md_import(one_module_spec)))).
:- inst use_item        ==  bound(item_module_defn(ground,
                                bound(md_use(one_module_spec)))).
:- inst type_defn_item  ==  bound(item_type_defn(ground, ground, ground,
                                ground, ground)).

:- pred standardize_impl_items(item_list::in, item_list::out) is det.

standardize_impl_items(Items0, Items) :-
    do_standardize_impl_items(Items0, no, Unexpected, [], RevRemainderItems,
        [], ImportItems, [], UseItems, [], TypeDefnItems),
    (
        Unexpected = yes,
        unexpected(this_file, "standardize_impl_items: unexpected items")
        % XXX If the above exception is thrown and you need a
        % workaround you can replace the call to unexpected with this code:
        % Items = Items0
    ;
        Unexpected = no,
        list.reverse(RevRemainderItems, RemainderItems),
        list.condense([ImportItems, UseItems, TypeDefnItems, RemainderItems],
            Items)
    ).

:- pred do_standardize_impl_items(item_list::in, bool::in, bool::out,
    item_list::in, item_list::out,
    item_list::in(list_skel(item_context(import_item))),
    item_list::out(list_skel(item_context(import_item))),
    item_list::in(list_skel(item_context(use_item))),
    item_list::out(list_skel(item_context(use_item))),
    item_list::in(list_skel(item_context(type_defn_item))),
    item_list::out(list_skel(item_context(type_defn_item))))
    is det.

do_standardize_impl_items([], !Unexpected, !RevRemainderItems,
        !ImportItems, !UseItems, !TypeDefnItems).
do_standardize_impl_items([ItemAndContext | ItemAndContexts], !Unexpected,
        !RevRemainderItems, !ImportItems, !UseItems, !TypeDefnItems) :-
    ItemAndContext = item_and_context(Item, Context),
    ( Item = item_module_defn(_VarSet, ModuleDefn) ->
        (
            ModuleDefn = md_import(ImportModules),
            ( ImportModules = list_module([_ImportModule]) ->
                insert_import_module(Context, Item, !ImportItems)
            ;
                unexpected(this_file,
                    "do_standardize_impl_items: non-singleton-module import")
            )
        ;
            ModuleDefn = md_use(UseModules),
            ( UseModules = list_module([_UseModule]) ->
                insert_use_module(Context, Item, !UseItems)
            ;
                unexpected(this_file,
                    "do_standardize_impl_items: non-singleton-module use")
            )
        ;
            ( ModuleDefn = md_module(_)
            ; ModuleDefn = md_end_module(_)
            ; ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
            ; ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_export(_)
            ; ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_private_interface
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            !:Unexpected = yes
        ;
            ModuleDefn = md_include_module(_),
            !:RevRemainderItems = [ItemAndContext | !.RevRemainderItems]
        )
    ; Item = item_type_defn(_, _, _, _, _) ->
        insert_type_defn(Context, Item, !TypeDefnItems)
    ;
        !:RevRemainderItems = [ItemAndContext | !.RevRemainderItems]
    ),
    do_standardize_impl_items(ItemAndContexts, !Unexpected,
        !RevRemainderItems, !ImportItems, !UseItems, !TypeDefnItems).

:- pred insert_import_module(prog_context::in, item::in,
    item_list::in(list_skel(item_context(import_item))),
    item_list::out(list_skel(item_context(import_item)))) is det.

insert_import_module(Context, Item, [], [item_and_context(Item, Context)]).
insert_import_module(Context, Item, [Head | Tail], Result) :-
    Head = item_and_context(HeadItem, _HeadContext),
    % The lack of alias tracking prevents the compiler from figuring out
    % that this predicate is only called with values of Item for which
    % this test succeeds.
    ( Item = item_module_defn(_, md_import(list_module([ModulePrime]))) ->
        Module = ModulePrime
    ;
        unexpected(this_file, "insert_import_module: bad item")
    ),
    HeadItem = item_module_defn(_, md_import(list_module([HeadModule]))),
    compare(CompareSymName, Module, HeadModule),
    ( CompareSymName = (<) ->
        Result = [item_and_context(Item, Context), Head | Tail]
    ;
        insert_import_module(Context, Item, Tail, TailResult),
        Result = [Head | TailResult]
    ).

:- pred insert_use_module(prog_context::in, item::in,
    item_list::in(list_skel(item_context(use_item))),
    item_list::out(list_skel(item_context(use_item)))) is det.

insert_use_module(Context, Item, [], [item_and_context(Item, Context)]).
insert_use_module(Context, Item, [Head | Tail], Result) :-
    Head = item_and_context(HeadItem, _HeadContext),
    % The lack of alias tracking prevents the compiler from figuring out
    % that this predicate is only called with values of Item for which
    % this test succeeds.
    ( Item = item_module_defn(_, md_use(list_module([ModulePrime]))) ->
        Module = ModulePrime
    ;
        unexpected(this_file, "insert_import_module: bad item")
    ),
    HeadItem = item_module_defn(_, md_use(list_module([HeadModule]))),
    compare(CompareSymName, Module, HeadModule),
    ( CompareSymName = (<) ->
        Result = [item_and_context(Item, Context), Head | Tail]
    ;
        insert_use_module(Context, Item, Tail, TailResult),
        Result = [Head | TailResult]
    ).

:- pred insert_type_defn(prog_context::in, item::in(type_defn_item),
    item_list::in(list_skel(item_context(type_defn_item))),
    item_list::out(list_skel(item_context(type_defn_item)))) is det.

insert_type_defn(Context, Item, [], [item_and_context(Item, Context)]).
insert_type_defn(Context, Item, [Head | Tail], Result) :-
    Head = item_and_context(HeadItem, _HeadContext),
    Item = item_type_defn(_, SymName, Params, _, _),
    HeadItem = item_type_defn(_, HeadSymName, HeadParams, _, _),
    compare(CompareSymName, SymName, HeadSymName),
    (
        (
            CompareSymName = (<)
        ;
            CompareSymName = (=),
            list.length(Params, ParamsLength),
            list.length(HeadParams, HeadParamsLength),
            compare(Compare, ParamsLength, HeadParamsLength),
            Compare = (<)
        )
    ->
        Result = [item_and_context(Item, Context), Head | Tail]
    ;
        insert_type_defn(Context, Item, Tail, TailResult),
        Result = [Head | TailResult]
    ).

:- pred make_impl_type_abstract(type_ctor::in,
    assoc_list(type_defn, item_and_context)::in,
    assoc_list(type_defn, item_and_context)::out) is det.

make_impl_type_abstract(_TypeCtor, !TypeDefnPairs) :-
    (
        !.TypeDefnPairs = [parse_tree_du_type(Ctors, MaybeEqCmp)
            - item_and_context(Item0, Context)],
        not constructor_list_represents_dummy_argument_type(Ctors, MaybeEqCmp)
    ->
        Defn = parse_tree_abstract_type(non_solver_type),
        ( Item = Item0 ^ td_ctor_defn := Defn ->
            !:TypeDefnPairs = [Defn - item_and_context(Item, Context)]
        ;
            unexpected(this_file,
                "make_impl_type_abstract: item is not a type_defn")
        )
    ;
        true
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
:- pred strip_unnecessary_impl_imports(set(module_name)::in, item_list::in,
    item_list::out) is det.

strip_unnecessary_impl_imports(NecessaryImports, !Items) :-
    list.filter(is_necessary_impl_import(NecessaryImports), !Items).

:- pred is_necessary_impl_import(set(module_name)::in, item_and_context::in)
    is semidet.

is_necessary_impl_import(NecessaryImports, ItemAndContext) :-
    ItemAndContext = item_and_context(Item, _),
    ( Item = item_module_defn(_, Defn) ->
        (
            ( Defn = md_use(Module)
            ; Defn = md_import(Module)
            )
        ->
            ( Module = list_module([ModuleName]) ->
                set.member(ModuleName, NecessaryImports)
            ;
                unexpected(this_file, "is_necessary_impl_import: " ++
                    "non-singleton import or use decl")
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
    item_list::in, item_list::out) is det.

strip_unnecessary_impl_types(NecessaryTypeCtors, !Items) :-
    list.filter(is_necessary_impl_type(NecessaryTypeCtors), !Items).

:- pred is_necessary_impl_type(set(type_ctor)::in, item_and_context::in)
    is semidet.

is_necessary_impl_type(NecessaryTypeCtors, ItemAndContext) :-
    ItemAndContext = item_and_context(Item,  _),
    ( Item = item_type_defn(_, SymName, Params, _, _) ->
        TypeCtor = type_ctor(SymName, list.length(Params)),
        set.member(TypeCtor, NecessaryTypeCtors)
    ;
        true
    ).

    % get_requirements_of_impl_exported_types(InterfaceTypeMap, ImplTypeMap,
    %   NecessaryTypeCtors, Modules):
    %
    % Figure out the set of abstract equivalence type constructors
    % (i.e. the types that are exported as abstract types and which are defined
    % in the implementation section as equivalence types or as foreign types).
    % Return in NecessaryTypeCtors the smallest set containing those
    % constructors, and the set of private type constructors referred to
    % by the right hand side of any equivalence type in NecessaryTypeCtors.
    %
    % Given a du type definition in the implementation section, we should
    % include it in AbsEqvLhsTypeCtors if the type constructor is abstract
    % exported and the implementation section also contains a foreign_type
    % definition of the type constructor.
    %
    % Return in Modules the set of modules that define the type constructors
    % in NecessaryTypeCtors.
    %
:- pred get_requirements_of_impl_exported_types(type_defn_map::in,
    type_defn_map::in, set(type_ctor)::out, set(type_ctor)::out,
    set(module_name)::out) is det.

get_requirements_of_impl_exported_types(InterfaceTypeMap, ImplTypeMap,
        DummyTypeCtors, EqvTypeCtors, Modules) :-
    multi_map.to_flat_assoc_list(ImplTypeMap, ImplTypes),
    list.foldl2(accumulate_abs_impl_exported_type_lhs(InterfaceTypeMap),
        ImplTypes, set.init, AbsEqvLhsTypeCtors, set.init, DummyTypeCtors),
    set.fold2(accumulate_abs_eqv_type_rhs(ImplTypeMap), AbsEqvLhsTypeCtors,
        set.init, AbsEqvRhsTypeCtors, set.init, Modules),
    set.union(AbsEqvLhsTypeCtors, AbsEqvRhsTypeCtors, EqvTypeCtors).

:- pred accumulate_abs_impl_exported_type_lhs(type_defn_map::in,
    pair(type_ctor, pair(type_defn, item_and_context))::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_impl_exported_type_lhs(InterfaceTypeMap,
        TypeCtor - (TypeDefn - _ItemAndContext), !AbsEqvLhsTypeCtors,
        !DummyTypeCtors) :-
    % A type may have multiple definitions because it may be defined both
    % as a foreign type and as a Mercury type. We grab any equivalence types
    % that are in there.
    %
    (
        TypeDefn = parse_tree_eqv_type(_RhsType),
        map.search(InterfaceTypeMap, TypeCtor, _)
    ->
        svset.insert(TypeCtor, !AbsEqvLhsTypeCtors)
    ;
        TypeDefn = parse_tree_foreign_type(_, _, _),
        map.search(InterfaceTypeMap, TypeCtor, _)
    ->
        svset.insert(TypeCtor, !AbsEqvLhsTypeCtors)
    ;
        TypeDefn = parse_tree_du_type(Ctors, MaybeEqCmp),
        constructor_list_represents_dummy_argument_type(Ctors, MaybeEqCmp)
    ->
        svset.insert(TypeCtor, !DummyTypeCtors)
    ;
        true
    ).

:- pred accumulate_abs_eqv_type_rhs(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs(ImplTypeMap, TypeCtor, !AbsEqvRhsTypeCtors,
        !Modules) :-
    ( map.search(ImplTypeMap, TypeCtor, TypeDefns) ->
        list.foldl2(accumulate_abs_eqv_type_rhs_2(ImplTypeMap), TypeDefns,
            !AbsEqvRhsTypeCtors, !Modules)
    ;
        true
    ).

:- pred accumulate_abs_eqv_type_rhs_2(type_defn_map::in,
    pair(type_defn, item_and_context)::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs_2(ImplTypeMap, TypeDefn - _, !AbsEqvRhsTypeCtors,
        !Modules) :-
    ( TypeDefn = parse_tree_eqv_type(RhsType) ->
        type_to_type_ctor_set(RhsType, set.init, RhsTypeCtors),
        set.difference(RhsTypeCtors, !.AbsEqvRhsTypeCtors, NewRhsTypeCtors),
        set.fold(accumulate_modules, NewRhsTypeCtors, !Modules),
        set.union(NewRhsTypeCtors, !AbsEqvRhsTypeCtors),
        set.fold2(accumulate_abs_eqv_type_rhs(ImplTypeMap), NewRhsTypeCtors,
            !AbsEqvRhsTypeCtors, !Modules)
    ;
        true
    ).

:- pred accumulate_modules(type_ctor::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules(TypeCtor, !Modules) :-
    % NOTE: This assumes that everything has been module qualified.
    TypeCtor = type_ctor(SymName, _Arity),
    ( sym_name_get_module_name(SymName, ModuleName) ->
        svset.insert(ModuleName, !Modules)
    ;
        unexpected(this_file, "accumulate_modules/3: unknown type encountered")
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
            svset.insert(TypeCtor, !TypeCtors)
        ),
        list.foldl(type_to_type_ctor_set, Args, !TypeCtors)
    ;
        true
    ).

:- type type_defn_map == multi_map(type_ctor,
    pair(type_defn, item_and_context)).
:- type type_defn_pair == pair(type_ctor, pair(type_defn, item_and_context)).

:- pred gather_type_defns(bool::in, item_list::in,
    item_list::in, item_list::out, item_list::in, item_list::out,
    type_defn_map::in, type_defn_map::out,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defns(_, [], IntItems, reverse(IntItems),
        ImplItems, reverse(ImplItems), !IntTypes, !ImplTypes).
gather_type_defns(!.InInterface,
        [item_and_context(Item, Context) | ItemContexts],
        !IntItems, !ImplItems, !IntTypes, !ImplTypes) :-
    ( Item = item_module_defn(_, md_interface) ->
        !:InInterface = yes
    ; Item = item_module_defn(_, md_implementation) ->
        !:InInterface = no
    ; Item = item_type_defn(_, Name, Args, Body, _) ->
        TypeCtor = type_ctor(Name, length(Args)),
        (
            !.InInterface = yes,
            !:IntItems = [item_and_context(Item, Context) | !.IntItems],
            gather_type_defn(TypeCtor, Body, item_and_context(Item, Context),
                !IntTypes)
        ;
            !.InInterface = no,
            % We don't add this to !ImplItems yet --
            % we may be removing this item.
            gather_type_defn(TypeCtor, Body, item_and_context(Item, Context),
                !ImplTypes)
        )
    ;
        (
            !.InInterface = yes,
            !:IntItems = [item_and_context(Item, Context) | !.IntItems]
        ;
            !.InInterface = no,
            !:ImplItems = [item_and_context(Item, Context) | !.ImplItems]
        )
    ),
    gather_type_defns(!.InInterface, ItemContexts, !IntItems, !ImplItems,
        !IntTypes, !ImplTypes).

:- pred gather_type_defn(type_ctor::in, type_defn::in, item_and_context::in,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defn(TypeCtor, Body, Item, DefnMap0, DefnMap) :-
    multi_map.set(DefnMap0, TypeCtor, Body - Item, DefnMap).
        
:- pred get_requirements_of_impl_typeclasses(item_list::in,
    set(module_name)::out) is det.

get_requirements_of_impl_typeclasses(ImplItems, Modules) :-
    list.foldl(get_requirements_of_impl_typeclass,
        ImplItems, set.init, Modules).

:- pred get_requirements_of_impl_typeclass(item_and_context::in,
    set(module_name)::in, set(module_name)::out) is det.

get_requirements_of_impl_typeclass(item_and_context(Item, _), !Modules) :-
    (
        Item = item_typeclass(Constraints, _, _, _, _, _),
        list.foldl(get_requirements_of_impl_from_constraint, Constraints,
            !Modules)
    ;
        ( Item = item_clause(_, _, _, _, _, _)
        ; Item = item_type_defn(_, _, _, _, _)
        ; Item = item_inst_defn(_, _, _, _, _)
        ; Item = item_mode_defn(_, _, _, _, _)
        ; Item = item_module_defn(_, _)
        ; Item = item_pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _, _)
        ; Item = item_pred_or_func_mode(_, _, _, _, _, _, _)
        ; Item = item_pragma(_, _)
        ; Item = item_promise(_, _, _, _)
        ; Item = item_instance(_, _, _, _, _, _)
        ; Item = item_initialise(_, _, _)
        ; Item = item_finalise(_, _, _)
        ; Item = item_mutable(_, _, _, _, _, _)
        ; Item = item_nothing(_)
        )
    ).
        
:- pred get_requirements_of_impl_from_constraint(prog_constraint::in,
    set(module_name)::in, set(module_name)::out) is det.

get_requirements_of_impl_from_constraint(Constraint, !Modules) :-
    Constraint = constraint(ClassName, Args),
    % NOTE: this assumes that everything has been module qualified.
    ( sym_name_get_module_name(ClassName, ModuleName) ->
        svset.insert(ModuleName, !Modules)
    ;
        unexpected(this_file, "get_requirements_of_impl_from_constraint: " ++
            "unknown typeclass in constraint.")
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
            svset.insert(ModuleName, !Modules)
        ;   
            unexpected(this_file, "get_modules_from_constraint_arg: " ++
                "unknown type encountered.")
        ),
        get_modules_from_constraint_arg_types(Args, !Modules)
    ;
        ( ArgType = tuple_type(Args, _)
        ; ArgType = apply_n_type(_, Args, _)
        ; ArgType = kinded_type(KindedType, _), Args = [KindedType]
        ; ArgType = higher_order_type(Args0, MaybeRetType, _, _),
          (
                MaybeRetType = yes(RetType),
                Args = [ RetType  | Args0 ]
          ;
                MaybeRetType = no,
                Args = Args0
          )
        ),
        get_modules_from_constraint_arg_types(Args, !Modules)
    ).

%-----------------------------------------------------------------------------%

:- pred check_for_clauses_in_interface(item_list::in, item_list::out,
    io::di, io::uo) is det.

check_for_clauses_in_interface([], [], !IO).
check_for_clauses_in_interface([ItemAndContext0 | Items0], Items, !IO) :-
    ItemAndContext0 = item_and_context(Item0, Context),
    (
        Item0 = item_clause(_, _, _, _, _, _)
    ->
        ClauseWarning = [words("Warning: clause in module interface.")],
        report_warning(Context, 0, ClauseWarning, !IO),
        check_for_clauses_in_interface(Items0, Items, !IO)
    ;
        Item0 = item_pragma(_, Pragma),
        pragma_allowed_in_interface(Pragma, no)
    ->
        PragmaWarning = [words("Warning: pragma in module interface.")],
        report_warning(Context, 0, PragmaWarning, !IO),
        check_for_clauses_in_interface(Items0, Items, !IO)
    ;
        Items = [ItemAndContext0 | Items1],
        check_for_clauses_in_interface(Items0, Items1, !IO)
    ).

    % strip_clauses_from_interface is the same as
    % check_for_clauses_in_interface except that it doesn't issue any
    % warnings, and that it also strips out the `:- interface' and `:-
    % implementation' declarations.
    %
    % This is used when creating the private interface (`.int0') files for
    % packages with sub-modules.
    %
    % We treat initialise and finalise declarations as special kinds of
    % clause, since they should always be grouped together with the clauses
    % and should not appear in private interfaces.
    %
:- pred strip_clauses_from_interface(item_list::in, item_list::out) is det.

strip_clauses_from_interface(Items0, Items) :-
    split_clauses_and_decls(Items0, _Clauses, Items).

:- pred split_clauses_and_decls(item_list::in,
    item_list::out, item_list::out) is det.

split_clauses_and_decls([], [], []).
split_clauses_and_decls([ItemAndContext0 | Items0],
        ClauseItems, InterfaceItems) :-
    ItemAndContext0 = item_and_context(Item0, _Context),
    (
        ( Item0 = item_module_defn(_, md_interface)
        ; Item0 = item_module_defn(_, md_implementation)
        )
    ->
        split_clauses_and_decls(Items0, ClauseItems, InterfaceItems)
    ;
        (
            Item0 = item_clause(_,_,_,_,_,_)
        ;
            Item0 = item_pragma(_, Pragma),
            pragma_allowed_in_interface(Pragma, no)
        ;
            Item0 = item_initialise(_, _, _)
        ;
            Item0 = item_finalise(_, _, _)
        )
     ->
         split_clauses_and_decls(Items0, ClauseItems1, InterfaceItems),
         ClauseItems = [ItemAndContext0 | ClauseItems1]
    ;
        split_clauses_and_decls(Items0, ClauseItems, InterfaceItems1),
        InterfaceItems = [ItemAndContext0 | InterfaceItems1]
    ).

% pragma `obsolete', `terminates', `does_not_terminate'
% `termination_info', `check_termination', and `reserve_tag' pragma
% declarations are supposed to go in the interface, but all other pragma
% declarations are implementation details only, and should go in the
% implementation.

% XXX we should allow c_header_code;
% but if we do allow it, we should put it in the generated
% header file, which currently we don't.

pragma_allowed_in_interface(pragma_foreign_decl(_, _, _), no).
pragma_allowed_in_interface(pragma_foreign_import_module(_, _), yes).
pragma_allowed_in_interface(pragma_foreign_code(_, _), no).
pragma_allowed_in_interface(pragma_foreign_proc(_, _, _, _, _, _, _), no).
pragma_allowed_in_interface(pragma_inline(_, _), no).
pragma_allowed_in_interface(pragma_no_inline(_, _), no).
pragma_allowed_in_interface(pragma_obsolete(_, _), yes).
pragma_allowed_in_interface(pragma_foreign_export(_, _, _, _, _), no).
pragma_allowed_in_interface(pragma_import(_, _, _, _, _), no).
pragma_allowed_in_interface(pragma_source_file(_), yes).
    % yes, but the parser will strip out `source_file' pragmas anyway...
pragma_allowed_in_interface(pragma_fact_table(_, _, _), no).
pragma_allowed_in_interface(pragma_tabled(_, _, _, _, _, _), no).
    % `reserve_tag' must be in the interface iff the corresponding
    % type definition is in the interface. This is checked in make_hlds.m.
pragma_allowed_in_interface(pragma_reserve_tag(_, _), yes).
pragma_allowed_in_interface(pragma_promise_pure(_, _), no).
pragma_allowed_in_interface(pragma_promise_semipure(_, _), no).
pragma_allowed_in_interface(pragma_promise_equivalent_clauses(_, _), no).
pragma_allowed_in_interface(pragma_unused_args(_, _, _, _, _), no).
pragma_allowed_in_interface(pragma_exceptions(_, _, _, _, _), no).
pragma_allowed_in_interface(pragma_trailing_info(_, _, _, _, _), no).
pragma_allowed_in_interface(pragma_mm_tabling_info(_, _, _, _, _), no).
pragma_allowed_in_interface(pragma_type_spec(_, _, _, _, _, _, _, _), yes).
pragma_allowed_in_interface(pragma_termination_info(_, _, _, _, _), yes).
pragma_allowed_in_interface(pragma_termination2_info(_,_, _, _, _, _), yes).
pragma_allowed_in_interface(pragma_terminates(_, _), yes).
pragma_allowed_in_interface(pragma_does_not_terminate(_, _), yes).
pragma_allowed_in_interface(pragma_check_termination(_, _), yes).
pragma_allowed_in_interface(pragma_structure_sharing(_, _, _, _, _, _), yes).
pragma_allowed_in_interface(pragma_structure_reuse(_, _, _, _, _, _), yes).
pragma_allowed_in_interface(pragma_mode_check_clauses(_, _), yes).

check_for_no_exports(Items, ModuleName, !IO) :-
    globals.io_lookup_bool_option(warn_nothing_exported, ExportWarning, !IO),
    (
        ExportWarning = no
    ;
        ExportWarning = yes,
        get_interface(ModuleName, no, Items, InterfaceItems),
        check_int_for_no_exports(InterfaceItems, ModuleName, !IO)
    ).

    % Given a module name and a list of the items in that module's
    % interface, this procedure checks if the module doesn't export
    % anything, and if so, and --warn-nothing-exported is set, it reports
    % a warning.
:- pred check_int_for_no_exports(item_list::in, module_name::in,
    io::di, io::uo) is det.

check_int_for_no_exports([], ModuleName, !IO) :-
    warn_no_exports(ModuleName, !IO).
check_int_for_no_exports([item_and_context(Item, _Context) | ItemAndContexts],
        ModuleName, !IO) :-
    (
        (
            Item = item_nothing(_)
        ;
            Item = item_module_defn(_, ModuleDefn),
            ModuleDefn \= md_include_module(_)
        )
    ->
        % nothing useful - keep searching
        check_int_for_no_exports(ItemAndContexts, ModuleName, !IO)
    ;
        % we found something useful - don't issue the warning
        true
    ).

:- pred warn_no_exports(module_name::in, io::di, io::uo) is det.

warn_no_exports(ModuleName, !IO) :-
    module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
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
    Spec = error_spec(Severity, phase_term_to_parse_tree, [Msg]),
    globals.io_get_globals(Globals, !IO),
    % XXX _NumErrors
    write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors, !IO).

%-----------------------------------------------------------------------------%

:- pred write_interface_file(file_name::in, module_name::in, string::in,
    maybe(timestamp)::in, item_list::in, io::di, io::uo) is det.

write_interface_file(_SourceFileName, ModuleName, Suffix, MaybeTimestamp,
        InterfaceItemAndContexts0, !IO) :-

    % Create (e.g.) `foo.int.tmp'.
    string.append(Suffix, ".tmp", TmpSuffix),
    module_name_to_file_name(ModuleName, Suffix, yes, OutputFileName, !IO),
    module_name_to_file_name(ModuleName, TmpSuffix, no, TmpOutputFileName,
        !IO),

    globals.io_lookup_bool_option(line_numbers, LineNumbers, !IO),
    globals.io_set_option(line_numbers, bool(no), !IO),

    globals.io_lookup_bool_option(generate_item_version_numbers,
        GenerateVersionNumbers, !IO),

    (
        GenerateVersionNumbers = yes,
        % Find the timestamp of the current module.
        (
            MaybeTimestamp = yes(Timestamp),

            % Read in the previous version of the file.
            read_mod_ignore_errors(ModuleName, Suffix,
                "Reading old interface for module", yes, no,
                OldItemAndContexts, OldError, _OldIntFileName, _OldTimestamp, !IO),
            ( OldError = no_module_errors ->
                MaybeOldItemAndContexts = yes(OldItemAndContexts)
            ;
                % If we can't read in the old file, the timestamps will
                % all be set to the modification time of the source file.
                MaybeOldItemAndContexts = no
            ),
            recompilation.version.compute_version_numbers(Timestamp,
                InterfaceItemAndContexts0, MaybeOldItemAndContexts,
                VersionNumbers),
            VersionNumberItem = item_module_defn(varset.init,
                md_version_numbers(ModuleName, VersionNumbers)),
            VersionNumberItemAndContext =
                item_and_context(VersionNumberItem, term.context_init),
            (
                InterfaceItemAndContexts0 = [FirstItemAndContext |
                    InterfaceItemAndContexts1],
                FirstItemAndContext =
                    item_and_context(item_module_defn(_, md_interface), _)
            ->
                InterfaceItemAndContexts = [FirstItemAndContext,
                    VersionNumberItemAndContext | InterfaceItemAndContexts1]
            ;
                InterfaceItemAndContexts = [make_pseudo_decl(md_interface),
                    VersionNumberItemAndContext | InterfaceItemAndContexts0]
            )
        ;
            MaybeTimestamp = no,
            unexpected(this_file, "write_interface_file with " ++
                "`--smart-recompilation', timestamp not read")
        )
    ;
        GenerateVersionNumbers = no,
        InterfaceItemAndContexts = InterfaceItemAndContexts0
    ),
    convert_to_mercury(ModuleName, TmpOutputFileName, InterfaceItemAndContexts,
        !IO),
    globals.io_set_option(line_numbers, bool(LineNumbers), !IO),
    update_interface(OutputFileName, !IO).

update_interface(OutputFileName, !IO) :-
    update_interface_return_succeeded(OutputFileName, Succeeded, !IO),
    (
        Succeeded = no,
        report_error("problem updating interface files.", !IO)
    ;
        Succeeded = yes
    ).

update_interface_return_succeeded(OutputFileName, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Updating interface:\n", !IO),
    TmpOutputFileName = OutputFileName ++ ".tmp",
    io.open_binary_input(OutputFileName, OutputFileRes, !IO),
    (
        OutputFileRes = ok(OutputFileStream),
        io.open_binary_input(TmpOutputFileName, TmpOutputFileRes, !IO),
        (
            TmpOutputFileRes = ok(TmpOutputFileStream),
            binary_input_stream_cmp(OutputFileStream, TmpOutputFileStream,
                FilesDiffer, !IO),
            io.close_binary_input(OutputFileStream, !IO),
            io.close_binary_input(TmpOutputFileStream, !IO),
            (
                FilesDiffer = ok(ok(no)),
                Succeeded = yes,
                maybe_write_string(Verbose, "% ", !IO),
                maybe_write_string(Verbose, OutputFileName, !IO),
                maybe_write_string(Verbose, "' has not changed.\n", !IO),
                io.remove_file(TmpOutputFileName, _, !IO)
            ;
                FilesDiffer = ok(ok(yes)),
                update_interface_create_file("CHANGED", OutputFileName,
                    TmpOutputFileName, Succeeded, !IO)
            ;
                FilesDiffer = ok(error(TmpFileError)),
                Succeeded = no,
                io.write_string("Error reading `", !IO),
                io.write_string(TmpOutputFileName, !IO),
                io.write_string("': ", !IO),
                io.write_string(io.error_message(TmpFileError), !IO),
                io.nl(!IO)
            ;
                FilesDiffer = error(_, _),
                update_interface_create_file("been CREATED", OutputFileName,
                    TmpOutputFileName, Succeeded, !IO)
            )
        ;

            TmpOutputFileRes = error(TmpOutputFileError),
            Succeeded = no,
            io.close_binary_input(OutputFileStream, !IO),
            io.write_string("Error creating `", !IO),
            io.write_string(OutputFileName, !IO),
            io.write_string("': ", !IO),
            io.write_string(io.error_message(TmpOutputFileError), !IO),
            io.nl(!IO)
        )
    ;
        OutputFileRes = error(_),
        update_interface_create_file("been CREATED", OutputFileName,
            TmpOutputFileName, Succeeded, !IO)
    ).

:- pred binary_input_stream_cmp_2(io.binary_input_stream::in, int::in,
    bool::out, io.res(bool)::in, io.res(bool)::out,
    io::di, io::uo) is det.

binary_input_stream_cmp_2(TmpOutputFileStream, Byte, Continue, _, Differ,
        !IO) :-
    io.read_byte(TmpOutputFileStream, TmpByteResult, !IO),
    (
        TmpByteResult = ok(TmpByte),
        ( TmpByte = Byte ->
            Differ = ok(no),
            Continue = yes
        ;
            Differ = ok(yes),
            Continue = no
        )
    ;
        TmpByteResult = eof,
        Differ = ok(yes),
        Continue = no
    ;
        TmpByteResult = error(TmpByteError),
        Differ = error(TmpByteError) : io.res(bool),
        Continue = no
    ).

:- pred binary_input_stream_cmp(io.binary_input_stream::in,
    io.binary_input_stream::in, io.maybe_partial_res(io.res(bool))::out,
    io::di, io::uo) is det.

binary_input_stream_cmp(OutputFileStream, TmpOutputFileStream, FilesDiffer,
        !IO) :-
    io.binary_input_stream_foldl2_io_maybe_stop(OutputFileStream,
        binary_input_stream_cmp_2(TmpOutputFileStream),
        ok(no), FilesDiffer0, !IO),

    % Check whether there is anything left in TmpOutputFileStream
    ( FilesDiffer0 = ok(ok(no)) ->
        io.read_byte(TmpOutputFileStream, TmpByteResult2, !IO),
        (
            TmpByteResult2 = ok(_),
            FilesDiffer = ok(ok(yes))
        ;
            TmpByteResult2 = eof,
            FilesDiffer = FilesDiffer0
        ;
            TmpByteResult2 = error(Error),
            FilesDiffer = ok(error(Error))
        )
    ;
        FilesDiffer = FilesDiffer0
    ).

:- pred update_interface_create_file(string::in, string::in, string::in,
    bool::out, io::di, io::uo) is det.

update_interface_create_file(Msg, OutputFileName, TmpOutputFileName, Succeeded,
        !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose,
        "% `" ++ OutputFileName ++ "' has " ++ Msg ++ ".\n", !IO),
    copy_file(TmpOutputFileName, OutputFileName, MoveRes, !IO),
    (
        MoveRes = ok,
        Succeeded = yes
    ;
        MoveRes = error(MoveError),
        Succeeded = no,
        io.write_string("Error creating `" ++ OutputFileName ++ "': " ++
            io.error_message(MoveError), !IO),
        io.nl(!IO)
    ),
    io.remove_file(TmpOutputFileName, _, !IO).

%-----------------------------------------------------------------------------%

touch_interface_datestamp(ModuleName, Ext, !IO) :-
    module_name_to_file_name(ModuleName, Ext, yes, OutputFileName, !IO),
    touch_datestamp(OutputFileName, !IO).

touch_datestamp(OutputFileName, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose,
        "% Touching `" ++ OutputFileName ++ "'... ", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_output(OutputFileName, Result, !IO),
    (
        Result = ok(OutputStream),
        io.write_string(OutputStream, "\n", !IO),
        io.close_output(OutputStream, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        Result = error(IOError),
        io.error_message(IOError, IOErrorMessage),
        io.write_string("\nError opening `" ++ OutputFileName
            ++ "' for output: " ++ IOErrorMessage ++ ".\n", !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

grab_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
        NestedChildren, ReadModules, MaybeTimestamp, Items0, !:Module,
        Error, !IO) :-
    % Find out which modules this one depends on.
    AncestorModules = get_ancestors(ModuleName),
    get_dependencies(Items0, IntImportedModules0, IntUsedModules0,
        ImpImportedModules0, ImpUsedModules0),

    ImportedModules0 = IntImportedModules0 ++ ImpImportedModules0,
    UsedModules0 = IntUsedModules0 ++ ImpUsedModules0,

    some [!Specs] (
        !:Specs = [],

        module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
        warn_if_import_self_or_ancestor(ModuleName, FileName, AncestorModules,
            ImportedModules0, UsedModules0, !Specs),

        warn_if_duplicate_use_import_decls(ModuleName, FileName,
            IntImportedModules0, IntImportedModules1,
            IntUsedModules0, IntUsedModules1,
            ImpImportedModules0, ImpImportedModules,
            ImpUsedModules0, ImpUsedModules, !Specs),

        get_fact_table_dependencies(Items0, FactDeps),
        get_interface_and_implementation(ModuleName, no, Items0,
            InterfaceItems, ImplItems),
        get_children(InterfaceItems, PublicChildren),
        (
            MaybeTimestamp = yes(Timestamp),
            MaybeTimestamps = yes(map.det_insert(map.init, ModuleName,
                module_timestamp(".m", Timestamp, may_be_unqualified)))
        ;
            MaybeTimestamp = no,
            MaybeTimestamps = no

        ),
        init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
            Items0, PublicChildren, NestedChildren, FactDeps,
            MaybeTimestamps, !:Module),

        % If this module has any separately-compiled sub-modules, then
        % we need to make everything in the implementation of this module
        % exported_to_submodules.  We do that by splitting out the
        % implementation declarations and putting them in a special
        % `:- private_interface' section.
        %
        get_children(Items0, Children),
        (
            Children = [],
            Items1 = Items0
        ;
            Children = [_ | _],
            split_clauses_and_decls(ImplItems, Clauses, ImplDecls),
            list.condense(
                [[make_pseudo_decl(md_interface) | InterfaceItems],
                [make_pseudo_decl(md_private_interface) | ImplDecls],
                [make_pseudo_decl(md_implementation) | Clauses]], Items1),
            module_imports_set_items(Items1, !Module)
        ),

        % Add `builtin' and `private_builtin' to the list of imported modules.
        globals.io_get_globals(Globals, !IO),
        add_implicit_imports(Items1, Globals,
            IntImportedModules1, IntImportedModules2,
            IntUsedModules1, IntUsedModules2),

        % Process the ancestor modules.
        %
        % Uses of the items declared in ancestor modules do not need
        % module qualifiers. Modules imported by ancestors are considered
        % to be visible in the current module.
        process_module_private_interfaces(ReadModules, AncestorModules,
            make_pseudo_decl(
                md_imported(import_locn_ancestor_private_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntImportedModules2, IntImportedModules,
            IntUsedModules2, IntUsedModules, !Module, !IO),

        % Process the modules imported using `import_module'.
        % Uses of these items do not need module qualifiers.
        IntIndirectImports0 = [],
        IntImpIndirectImports0 = [],
        process_module_long_interfaces(ReadModules, may_be_unqualified,
            IntImportedModules, ".int",
            make_pseudo_decl(md_imported(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntIndirectImports0, IntIndirectImports1,
            IntImpIndirectImports0, IntImpIndirectImports1,
            !Module, !IO),

        ImpIndirectImports0 = [],
        ImpImpIndirectImports0 = [],
        process_module_long_interfaces(ReadModules, may_be_unqualified,
            ImpImportedModules, ".int",
            make_pseudo_decl(md_imported(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpIndirectImports0, ImpIndirectImports1,
            ImpImpIndirectImports0, ImpImpIndirectImports1,
            !Module, !IO),

        % Process the modules imported using `use_module' .
        process_module_long_interfaces(ReadModules, must_be_qualified,
            IntUsedModules, ".int",
            make_pseudo_decl(md_used(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntIndirectImports1, IntIndirectImports,
            IntImpIndirectImports1, IntImpIndirectImports2,
            !Module, !IO),
        process_module_long_interfaces(ReadModules, must_be_qualified,
            ImpUsedModules, ".int",
            make_pseudo_decl(md_used(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpIndirectImports1, ImpIndirectImports,
            ImpImpIndirectImports1, ImpImpIndirectImports2,
            !Module, !IO),

        % Process the short interfaces for indirectly imported modules.
        % The short interfaces are treated as if they are imported
        % using `use_module'.
        append_pseudo_decl(md_transitively_imported, !Module),
        process_module_short_interfaces_transitively(ReadModules,
            IntIndirectImports, ".int2",
            make_pseudo_decl(md_used(import_locn_interface)),
            make_pseudo_decl(md_abstract_imported),
            IntImpIndirectImports2, IntImpIndirectImports, !Module, !IO),
        process_module_short_interfaces_transitively(ReadModules,
            ImpIndirectImports, ".int2",
            make_pseudo_decl(md_used(import_locn_implementation)),
            make_pseudo_decl(md_abstract_imported),
            ImpImpIndirectImports2, ImpImpIndirectImports, !Module, !IO),

        % Process the short interfaces for modules imported in the
        % implementation of indirectly imported modules. The items in these
        % modules shouldn't be visible to typechecking -- they are used for
        % fully expanding equivalence types after the semantic checking passes.
        process_module_short_interfaces_and_impls_transitively(
            ReadModules, IntImpIndirectImports, ".int2",
            make_pseudo_decl(md_abstract_imported),
            make_pseudo_decl(md_abstract_imported),
            !Module, !IO),
        process_module_short_interfaces_and_impls_transitively(
            ReadModules, ImpImpIndirectImports, ".int2",
            make_pseudo_decl(md_abstract_imported),
            make_pseudo_decl(md_abstract_imported),
            !Module, !IO),

        module_imports_get_items(!.Module, Items),
        check_imports_accessibility(ModuleName,
            IntImportedModules ++ IntUsedModules ++
            ImpImportedModules ++ ImpUsedModules, Items, !Specs),

        sort_error_specs(!.Specs, SortedSpecs),
        write_error_specs(SortedSpecs, Globals, 0, _NumWarnings,
            0, _NumErrors, !IO),

        module_imports_get_error(!.Module, Error)
    ).

    % grab_unqual_imported_modules:
    %
    % Like grab_imported_modules, but gets the `.int3' files
    % instead of the `.int' and `.int2' files.
    %
grab_unqual_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, !:Module, Error, !IO) :-
    % Find out which modules this one depends on.
    ParentDeps = get_ancestors(ModuleName),
    get_dependencies(Items0, IntImportDeps0, IntUseDeps0,
        ImpImportDeps, ImpUseDeps),

    % Construct the initial module import structure.
    init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, [], [], [], no, !:Module),

    % Add `builtin' and `private_builtin' to the imported modules.
    globals.io_get_globals(Globals, !IO),
    add_implicit_imports(Items0, Globals,
        IntImportDeps0, IntImportDeps, IntUseDeps0, IntUseDeps),

    % Get the .int3s and .int0s that the current module depends on.
    map.init(ReadModules),

    % First the .int0s for parent modules.
    process_module_private_interfaces(ReadModules, ParentDeps,
        make_pseudo_decl(md_imported(import_locn_ancestor_private_interface)),
        make_pseudo_decl(md_abstract_imported),
        [], ParentImportDeps, [], ParentUseDeps, !Module, !IO),

    % Then the .int3s for `:- import'-ed modules.
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ParentImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_ancestor)),
        make_pseudo_decl(md_abstract_imported),
        [], IntIndirectImportDeps0, [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        IntImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps0, IntIndirectImportDeps1,
        [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ImpImportDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        [], ImpIndirectImportDeps0,
        [], _, !Module, !IO),

    % Then (after appropriate `:- used' decls) the .int3s for `:- use'-ed
    % modules.
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ParentUseDeps, ".int3",
        make_pseudo_decl(md_imported(import_locn_ancestor)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps1, IntIndirectImportDeps2,
        [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, must_be_qualified,
        IntUseDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        IntIndirectImportDeps2, IntIndirectImportDeps,
        [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, must_be_qualified,
        ImpUseDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        ImpIndirectImportDeps0, ImpIndirectImportDeps,
        [], _, !Module, !IO),

    % Then (after appropriate `:- used' decl) the .int3s for indirectly
    % imported modules.
    process_module_short_interfaces_transitively(ReadModules,
        IntIndirectImportDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_interface)),
        make_pseudo_decl(md_abstract_imported),
        [], _, !Module, !IO),

    process_module_short_interfaces_transitively(ReadModules,
        ImpIndirectImportDeps, ".int3",
        make_pseudo_decl(md_used(import_locn_implementation)),
        make_pseudo_decl(md_abstract_imported),
        [], _, !Module, !IO),

    some [!Specs] (
        !:Specs = [],

        module_imports_get_items(!.Module, Items),
        check_imports_accessibility(ModuleName,
            IntImportDeps ++ IntUseDeps ++ ImpImportDeps ++ ImpUseDeps,
            Items, !Specs),

        sort_error_specs(!.Specs, SortedSpecs),
        write_error_specs(SortedSpecs, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),

        module_imports_get_error(!.Module, Error)
    ).

%-----------------------------------------------------------------------------%

find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp,
        Items, MaybeTimestamp, Error, FileName) :-
    map.search(ReadModules, ModuleName - Suffix, ReadModule),
    ReadModule = read_module(ModuleTimestamp, Items, Error, FileName),
    (
        ReturnTimestamp = yes,
        ModuleTimestamp = module_timestamp(_, Timestamp, _),
        MaybeTimestamp = yes(Timestamp)
    ;
        ReturnTimestamp = no,
        MaybeTimestamp = no
    ).

:- pred init_module_imports(file_name::in, module_name::in, module_name::in,
    item_list::in, list(module_name)::in, list(module_name)::in,
    list(string)::in, maybe(module_timestamps)::in, module_imports::out)
    is det.

init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, PublicChildren, NestedChildren, FactDeps,
        MaybeTimestamps, Module) :-
    maybe_add_foreign_import_module(ModuleName, Items0, Items),
    Module = module_imports(SourceFileName, SourceFileModuleName,
        ModuleName, [], [], [], [], [], PublicChildren,
        NestedChildren, FactDeps, unknown, [],
        no_foreign_export, Items, no_module_errors,
        MaybeTimestamps, no_main, dir.this_directory).

module_imports_get_source_file_name(Module, Module ^ source_file_name).
module_imports_get_module_name(Module, Module ^ module_name).
module_imports_get_impl_deps(Module, Module ^ impl_deps).
module_imports_get_items(Module, Module ^ items).
module_imports_get_error(Module, Module ^ error).
module_imports_set_items(Items, Module, Module ^ items := Items).
module_imports_set_error(Error, Module, Module ^ error := Error).
module_imports_set_int_deps(IntDeps, Module, Module ^ int_deps := IntDeps).
module_imports_set_impl_deps(ImplDeps, Module,
        Module ^ impl_deps := ImplDeps).
module_imports_set_indirect_deps(IndirectDeps, Module,
        Module ^ indirect_deps := IndirectDeps).

append_pseudo_decl(PseudoDecl, Module0, Module) :-
    Items0 = Module0 ^ items,
    list.append(Items0, [make_pseudo_decl(PseudoDecl)], Items),
    Module = Module0 ^ items := Items.

make_pseudo_decl(PseudoDecl) =
    item_and_context(item_module_defn(varset.init, PseudoDecl),
        term.context_init).

%-----------------------------------------------------------------------------%

get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps) :-
    add_implicit_imports(Items, Globals, [], ImportDeps, [], UseDeps).

:- pred add_implicit_imports(item_list::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

add_implicit_imports(Items, Globals, !ImportDeps, !UseDeps) :-
    MercuryPublicBuiltin = mercury_public_builtin_module,
    MercuryPrivateBuiltin = mercury_private_builtin_module,
    MercuryTableBuiltin = mercury_table_builtin_module,
    MercuryProfilingBuiltin = mercury_profiling_builtin_module,
    MercuryTermSizeProfBuiltin = mercury_term_size_prof_builtin_module,
    MercuryParBuiltin = mercury_par_builtin_module,
    !:ImportDeps = [MercuryPublicBuiltin | !.ImportDeps],
    !:UseDeps = [MercuryPrivateBuiltin | !.UseDeps],
    (
        %
        % We should include MercuryTableBuiltin if the Items contain
        % a tabling pragma, or if one of --use-minimal-model and
        % --trace-table-io is specified.
        %
        (
            contains_tabling_pragma(Items)
        ;
            globals.lookup_bool_option(Globals,
                use_minimal_model_stack_copy, yes)
        ;
            globals.lookup_bool_option(Globals,
                use_minimal_model_own_stacks, yes)
        ;
            globals.lookup_bool_option(Globals, trace_table_io, yes)
        )
    ->
        !:UseDeps = [MercuryTableBuiltin | !.UseDeps]
    ;
        true
    ),
    globals.lookup_bool_option(Globals, profile_deep, Deep),
    (
        Deep = yes,
        !:UseDeps = [MercuryProfilingBuiltin | !.UseDeps]
    ;
        Deep = no
    ),
    (
        (
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        )
    ->
        !:UseDeps = [MercuryTermSizeProfBuiltin | !.UseDeps]
    ;
        true
    ),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    (
        Target = target_c,
        HighLevelCode = no,
        Parallel = yes
    ->
        !:UseDeps = [MercuryParBuiltin | !.UseDeps]
    ;
        true
    ).

:- pred contains_tabling_pragma(item_list::in) is semidet.

contains_tabling_pragma([ItemAndContext | ItemAndContexts]) :-
    (
        ItemAndContext = item_and_context(item_pragma(_, Pragma), _Context),
        Pragma = pragma_tabled(_, _, _, _, _, _)
    ;
        contains_tabling_pragma(ItemAndContexts)
    ).

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

write_dependency_file(Module, AllDepsSet, MaybeTransOptDeps, !IO) :-
    Module = module_imports(SourceFileName, SourceFileModuleName,
        ModuleName, ParentDeps, IntDeps, ImplDeps, IndirectDeps,
        _Children, InclDeps, NestedDeps, FactDeps0,
        ContainsForeignCode, ForeignImports0, _ContainsForeignExport,
        Items, _Error, _Timestamps, _HasMain, _Dir),

    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    module_name_to_make_var_name(ModuleName, MakeVarName),
    module_name_to_file_name(ModuleName, ".d", yes, DependencyFileName, !IO),
    module_name_to_file_name(ModuleName, ".trans_opt_date", no,
        TransOptDateFileName, !IO),
    %
    % To avoid problems with concurrent updates of `.d' files
    % during parallel makes, we first create the file with a
    % temporary name, and then rename it to the desired name
    % when we've finished.
    %
    io.make_temp(dir.dirname(DependencyFileName), "tmp_d",
        TmpDependencyFileName, !IO),
    maybe_write_string(Verbose, "% Writing auto-dependency file `", !IO),
    maybe_write_string(Verbose, DependencyFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_output(TmpDependencyFileName, Result, !IO),
    (
        Result = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.append_list(["error opening temporary file `",
            TmpDependencyFileName, "' for output: ",
            IOErrorMessage], Message),
        report_error(Message, !IO)
    ;
        Result = ok(DepStream),
        list.append(IntDeps, ImplDeps, LongDeps0),
        ShortDeps0 = IndirectDeps,
        set.list_to_set(LongDeps0, LongDepsSet0),
        set.delete(LongDepsSet0, ModuleName, LongDepsSet),
        set.list_to_set(ShortDeps0, ShortDepsSet0),
        set.difference(ShortDepsSet0, LongDepsSet, ShortDepsSet1),
        set.delete(ShortDepsSet1, ModuleName, ShortDepsSet),
        set.to_sorted_list(LongDepsSet, LongDeps),
        set.to_sorted_list(ShortDepsSet, ShortDeps),
        set.to_sorted_list(AllDepsSet, AllDeps),
        list.sort_and_remove_dups(FactDeps0, FactDeps),

        (
            MaybeTransOptDeps = yes(TransOptDeps0),
            set.list_to_set(TransOptDeps0, TransOptDepsSet0),
            set.intersect(TransOptDepsSet0, LongDepsSet, TransOptDepsSet),
            set.to_sorted_list(TransOptDepsSet, TransOptDateDeps),
            %
            % Note that maybe_read_dependency_file searches for
            % this exact pattern.
            %
            io.write_strings(DepStream, [TransOptDateFileName, " :"], !IO),
            write_dependencies_list(TransOptDateDeps, ".trans_opt", DepStream,
                !IO)
        ;
            MaybeTransOptDeps = no
        ),

        (
            FactDeps = [_ | _],
            io.write_strings(DepStream,
                ["\n\n", MakeVarName, ".fact_tables ="], !IO),
            write_file_dependencies_list(FactDeps, "", DepStream, !IO),
            io.nl(DepStream, !IO),
            globals.io_lookup_bool_option(assume_gmake, AssumeGmake, !IO),
            (
                AssumeGmake = yes,
                io.write_strings(DepStream, [
                    "\n\n", MakeVarName,
                    ".fact_tables.os = $(", MakeVarName,
                    ".fact_tables:%=$(os_subdir)%.$O)\n\n",
                    MakeVarName,
                    ".fact_tables.cs = $(", MakeVarName,
                    ".fact_tables:%=$(cs_subdir)%.c)\n\n"
                ], !IO)
            ;
                AssumeGmake = no,
                io.write_strings(DepStream,
                    [MakeVarName, ".fact_tables.cs ="], !IO),
                write_fact_table_dependencies_list(ModuleName,
                    FactDeps, ".c", DepStream, !IO),
                io.write_strings(DepStream, ["\n\n",
                    MakeVarName, ".fact_tables.os ="], !IO),
                write_fact_table_dependencies_list(ModuleName,
                    FactDeps, ".$O", DepStream, !IO),
                io.nl(DepStream, !IO)
            )
        ;
            FactDeps = []
        ),

        ( string.remove_suffix(SourceFileName, ".m", SourceFileBase) ->
            ErrFileName = SourceFileBase ++ ".err"
        ;
            unexpected(this_file, "source file doesn't end in `.m'")
        ),
        module_name_to_file_name(ModuleName, ".optdate", no, OptDateFileName,
            !IO),
        module_name_to_file_name(ModuleName, ".c_date", no, CDateFileName,
            !IO),
        module_name_to_file_name(ModuleName, ".s_date", no, AsmDateFileName,
            !IO),
        module_name_to_file_name(ModuleName, ".pic_s_date", no,
            PicAsmDateFileName, !IO),
        module_name_to_file_name(ModuleName, ".$O", no, ObjFileName, !IO),
        module_name_to_file_name(ModuleName, ".il_date", no, ILDateFileName,
            !IO),
        module_name_to_file_name(ModuleName, ".java_date", no,
            JavaDateFileName, !IO),
        % XXX Why is the extension hardcoded to .pic_o here?  That looks
        % wrong.  It should probably be .$(EXT_FOR_PIC_OBJECT) - juliensf.
        module_name_to_file_name(ModuleName, ".pic_o", no, PicObjFileName,
            !IO),
        module_name_to_file_name(ModuleName, ".int0", no, Int0FileName, !IO),
        io.write_strings(DepStream, ["\n\n",
            OptDateFileName, " ",
            TransOptDateFileName, " ",
            ErrFileName, " ",
            CDateFileName, " ",
            AsmDateFileName, " ",
            PicAsmDateFileName, " ",
            ILDateFileName, " ",
            JavaDateFileName
        ] , !IO),
        io.write_strings(DepStream, [" : ", SourceFileName], !IO),
        % If the module contains nested sub-modules then `.int0'
        % file must first be built.
        (
            InclDeps = [_ | _],
            io.write_strings(DepStream, [" ", Int0FileName], !IO)
        ;
            InclDeps = []
        ),
        write_dependencies_list(ParentDeps, ".int0", DepStream, !IO),
        write_dependencies_list(LongDeps, ".int", DepStream, !IO),
        write_dependencies_list(ShortDeps, ".int2", DepStream, !IO),

        NestedExts = [
            ".optdate",
            ".trans_opt_date",
            ".c_date",
            ".s_date",
            ".pic_s_date",
            ".dir/*.$O",
            ".il_date",
            ".java_date"],

        % If a module contains nested-submodules then we need to build
        % the nested children before attempting to build the parent module.
        (
            NestedDeps = []
        ;
            NestedDeps = [_ | _],
            Write = (pred(Ext::in, !.LIO::di, !:LIO::uo) is det :-
                module_name_to_file_name(ModuleName, Ext, no, ExtName, !LIO),
                io.write_strings(DepStream, ["\n\n", ExtName, " : "], !LIO),
                write_dependencies_list(NestedDeps, Ext, DepStream, !LIO)
            ),
            list.foldl(Write, NestedExts, !IO)
        ),
        (
            FactDeps = [_ | _],
            io.write_strings(DepStream, [
                " \\\n\t$(", MakeVarName, ".fact_tables)\n\n",
                "$(", MakeVarName, ".fact_tables.os) : $(",
                MakeVarName, ".fact_tables) ",
                SourceFileName, "\n\n",
                "$(", MakeVarName, ".fact_tables.cs) : ",
                ObjFileName, "\n"
            ], !IO)
        ;
            FactDeps = []
        ),

        globals.io_lookup_bool_option(use_opt_files, UseOptFiles, !IO),
        globals.io_lookup_bool_option(intermodule_optimization, Intermod, !IO),
        globals.io_lookup_accumulating_option(intermod_directories,
            IntermodDirs, !IO),

        % If intermodule_optimization is enabled then all the .mh files
        % must exist because it is possible that the .c file imports them
        % directly or indirectly.
        (
            Intermod = yes,
            io.write_strings(DepStream, ["\n\n", ObjFileName, " : "], !IO),
            write_dependencies_list(AllDeps, ".mh", DepStream, !IO)
        ;
            Intermod = no
        ),
        (
            ( Intermod = yes
            ; UseOptFiles = yes
            )
        ->
            io.write_strings(DepStream, [
                "\n\n",
                TransOptDateFileName, " ",
                ErrFileName, " ",
                CDateFileName, " ",
                AsmDateFileName, " ",
                PicAsmDateFileName, " ",
                ILDateFileName, " ",
                JavaDateFileName, " : "
            ], !IO),

            % The target (e.g. C) file only depends on the .opt files
            % from the current directory, so that inter-module optimization
            % works when the .opt files for the library are unavailable.
            % This is only necessary because make doesn't allow conditional
            % dependencies. The dependency on the current module's .opt file
            % is to make sure the module gets type-checked without having
            % the definitions of abstract types from other modules.
            %
            % XXX The code here doesn't correctly handle dependencies
            % on `.int' and `.int2' files needed by the `.opt' files.
            globals.io_lookup_bool_option(transitive_optimization, TransOpt,
                !IO),
            globals.io_lookup_bool_option(use_trans_opt_files, UseTransOpt,
                !IO),

            (
                ( TransOpt = yes
                ; UseTransOpt = yes
                )
            ->
                bool.not(UseTransOpt, BuildOptFiles),
                get_both_opt_deps(BuildOptFiles, [ModuleName | LongDeps],
                    IntermodDirs, OptDeps, TransOptDeps, !IO),
                OptInt0Deps = sort_and_remove_dups(
                    condense(list.map(get_ancestors, OptDeps))),
                write_dependencies_list(OptDeps, ".opt", DepStream, !IO),
                write_dependencies_list(OptInt0Deps, ".int0", DepStream, !IO),

                io.write_strings(DepStream, [
                    "\n\n",
                    ErrFileName, " ",
                    CDateFileName, " ",
                    AsmDateFileName, " ",
                    PicAsmDateFileName, " ",
                    ILDateFileName, " ",
                    JavaDateFileName, " : "
                ], !IO),
                write_dependencies_list(TransOptDeps, ".trans_opt", DepStream,
                    !IO)
            ;
                bool.not(UseOptFiles, BuildOptFiles),
                get_opt_deps(BuildOptFiles, [ModuleName | LongDeps],
                    IntermodDirs, ".opt", OptDeps, !IO),
                OptInt0Deps = sort_and_remove_dups(
                    condense(list.map(get_ancestors, OptDeps))),
                write_dependencies_list(OptDeps, ".opt", DepStream, !IO),
                write_dependencies_list(OptInt0Deps, ".int0", DepStream, !IO)
            )
        ;
            true
        ),

        globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
        globals.io_get_target(CompilationTarget, !IO),
        (
            HighLevelCode = yes,
            CompilationTarget = target_c
        ->
            % For --high-level-code with --target c, we need to make sure that
            % we generate the header files for imported modules before
            % compiling the C files, since the generated C files
            % #include those header files.

            io.write_strings(DepStream, [
                "\n\n",
                PicObjFileName, " ",
                ObjFileName, " :"
            ], !IO),
            write_dependencies_list(AllDeps, ".mih", DepStream, !IO)
        ;
            true
        ),

        % We need to tell make how to make the header files. The header files
        % are actually built by the same command that creates the .c or .s
        % file, so we just make them depend on the .c or .s files.
        % This is needed for the --high-level-code rule above, and for
        % the rules introduced for `:- pragma foreign_import_module'
        % declarations. In some grades the header file won't actually be built
        % (e.g. LLDS grades for modules not containing `:- pragma export'
        % declarations), but this rule won't do any harm.

        module_name_to_file_name(ModuleName, ".c", no, CFileName, !IO),
        module_name_to_file_name(ModuleName, ".s", no, AsmFileName, !IO),
        module_name_to_file_name(ModuleName, ".mh", no, HeaderFileName, !IO),
        module_name_to_file_name(ModuleName, ".mih", no, HeaderFileName2, !IO),
        io.write_strings(DepStream, [
            "\n\n",
            "ifeq ($(TARGET_ASM),yes)\n",
            HeaderFileName, " ", HeaderFileName2, " : ", AsmFileName, "\n",
            "else\n",
            HeaderFileName, " ",  HeaderFileName2, " : ", CFileName, "\n",
            "endif"
        ], !IO),

        %
        % The `.module_dep' file is made as a side effect of
        % creating the `.c', `.s', `.il', or `.java'.
        %
        module_name_to_file_name(ModuleName, ".il", no, ILFileName, !IO),
        module_name_to_file_name(ModuleName, ".java", no, JavaFileName, !IO),
        module_name_to_file_name(ModuleName, make_module_dep_file_extension,
            no, ModuleDepFileName, !IO),
        io.write_strings(DepStream, [
            "\n\n",
            "ifeq ($(TARGET_ASM),yes)\n",
            ModuleDepFileName, " : ", AsmFileName, "\n",
            "else\n",
            " ifeq ($(findstring il,$(GRADE)),il)\n",
            ModuleDepFileName, " : ", ILFileName, "\n",
            "  ifeq ($(findstring java,$(GRADE)),java)\n",
            ModuleDepFileName, " : ", JavaFileName, "\n",
            "  else\n",
            ModuleDepFileName, " : ", CFileName, "\n",
            "  endif\n",
            " endif\n",
            "endif"
        ], !IO),

        % The .date and .date0 files depend on the .int0 files for the parent
        % modules, and the .int3 files for the directly and indirectly imported
        % modules.
        %
        % For nested sub-modules, the `.date' files for the parent modules
        % also depend on the same things as the `.date' files for this module,
        % since all the `.date' files will get produced by a single mmc
        % command. Similarly for `.date0' files, except these don't depend
        % on the `.int0' files, because when doing the
        % `--make-private-interface' for nested modules, mmc will process
        % the modules in outermost to innermost order so as to produce each
        % `.int0' file before it is needed.

        module_name_to_file_name(ModuleName, ".date", no, DateFileName, !IO),
        module_name_to_file_name(ModuleName, ".date0", no, Date0FileName, !IO),
        io.write_strings(DepStream, [
            "\n\n", DateFileName, " ",
            Date0FileName
        ], !IO),
        write_dependencies_list(ParentDeps, ".date", DepStream, !IO),
        io.write_strings(DepStream, [" : ", SourceFileName], !IO),
        write_dependencies_list(ParentDeps, ".int0", DepStream, !IO),
        write_dependencies_list(LongDeps, ".int3", DepStream, !IO),
        write_dependencies_list(ShortDeps, ".int3", DepStream, !IO),

        io.write_strings(DepStream, ["\n\n", Date0FileName], !IO),
        write_dependencies_list(ParentDeps, ".date0", DepStream, !IO),
        io.write_strings(DepStream, [" : ", SourceFileName], !IO),
        write_dependencies_list(LongDeps, ".int3", DepStream, !IO),
        write_dependencies_list(ShortDeps, ".int3", DepStream, !IO),
        io.write_string(DepStream, "\n\n", !IO),

        % If we can pass the module name rather than the file name, then do so.
        % `--smart-recompilation' doesn't work if the file name is passed
        % and the module name doesn't match the file name.

        have_source_file_map(HaveMap, !IO),
        (
            HaveMap = yes,
            module_name_to_file_name(SourceFileModuleName, ModuleArg)
        ;
            HaveMap = no,
            ModuleArg = SourceFileName
        ),

        globals.io_get_target(Target, !IO),
        globals.io_lookup_bool_option(sign_assembly, SignAssembly, !IO),
        globals.io_get_globals(Globals, !IO),

        % If we are on the IL backend, add the dependency that the
        % top level dll of a nested module hierachy depends on all
        % of it sub-modules dlls, as they are referenced from
        % inside the top level dll.
        % XXX Do we need to do the same for Java?

        module_name_to_file_name(ModuleName, ".dll", no, DllFileName, !IO),
        module_name_to_file_name(ModuleName, ".class", no, ClassFileName, !IO),
        SubModules = submodules(ModuleName, AllDeps),
        (
            Target = target_il,
            SubModules = [_ | _]
        ->
            io.write_strings(DepStream, [DllFileName, " : "], !IO),
            write_dll_dependencies_list(SubModules, "", DepStream, !IO),
            io.nl(DepStream, !IO)
        ;
            true
        ),

        (
            ContainsForeignCode = contains_foreign_code(LangSet),
            ForeignImports = ForeignImports0
        ;
            ContainsForeignCode = unknown,
            get_item_list_foreign_code(Globals, Items, LangSet,
                ForeignImports1, _),
            % If we're generating the `.dep' file, ForeignImports0 will contain
            % a conservative approximation to the set of foreign imports
            % needed which will include imports required by imported modules.
            (
                ForeignImports0 = [],
                ForeignImports = ForeignImports1
            ;
                ForeignImports0 = [_ | _],
                ForeignImports = ForeignImports0
            )
        ;
            ContainsForeignCode = no_foreign_code,
            set.init(LangSet),
            ForeignImports = ForeignImports0
        ),

        % Handle dependencies introduced by
        % `:- pragma foreign_import_module' declarations.

        list.filter_map(
            (pred(ForeignImportMod::in, Import::out) is semidet :-
                Import = foreign_import_module_name_from_module(
                    ForeignImportMod, SourceFileModuleName),

                % XXX We can't include mercury.dll as mmake can't find it,
                % but we know that it exists.
                Import \= unqualified("mercury")
            ), ForeignImports, ForeignImportedModules),
        (
            ForeignImportedModules = []
        ;
            ForeignImportedModules = [_ | _],
            (
                Target = target_il,
                ForeignImportTargets = [DllFileName],
                ForeignImportExt = ".dll"
            ;
                Target = target_java,
                ForeignImportTargets = [ClassFileName],
                ForeignImportExt = ".java"
            ;
                Target = target_c,
                % NOTE: for C (and asm) the possible targets might be a .o
                % file _or_ a .pic_o file.  We need to include dependencies
                % for the latter otherwise invoking mmake with a <module>.pic_o
                % target will break.
                ForeignImportTargets = [ObjFileName, PicObjFileName],
                ForeignImportExt = ".mh"
            ;
                Target = target_asm,
                ForeignImportTargets = [ObjFileName, PicObjFileName],
                ForeignImportExt = ".mh"
            ),
            WriteForeignImportTarget = (pred(ForeignImportTarget::in,
                    !.IO::di, !:IO::uo) is det :-
                io.write_string(DepStream, "\n\n", !IO),
                io.write_string(DepStream, ForeignImportTarget, !IO),
                io.write_string(DepStream, " : ", !IO),
                write_dependencies_list(ForeignImportedModules,
                    ForeignImportExt, DepStream, !IO),
                io.write_string(DepStream, "\n\n", !IO)
            ),
            list.foldl(WriteForeignImportTarget, ForeignImportTargets,
                !IO)
        ),

        (
            Target = target_il,
            not set.empty(LangSet)
        ->
            Langs = set.to_sorted_list(LangSet),
            list.foldl(write_foreign_dependency_for_il(DepStream,
                ModuleName, AllDeps, ForeignImports), Langs, !IO)
        ;
            true
        ),

        % If we are signing the assembly, then we will need the strong key
        % to sign the il file with so add a dependency that the il file
        % requires the strong name file `mercury.sn'. Also add the variable
        % ILASM_KEYFLAG-<module> which is used to build the command line
        % for ilasm.
        (
            Target = target_il,
            SignAssembly = yes
        ->
            module_name_to_make_var_name(ModuleName, ModuleNameString),
            module_name_to_file_name(ModuleName, ".il", no, IlFileName, !IO),

            io.write_strings(DepStream, [
                "ILASM_KEYFLAG-", ModuleNameString,
                    " = /keyf=mercury.sn\n",
                IlFileName, " : mercury.sn\n"], !IO)
        ;
            true
        ),

        module_name_to_file_name(ModuleName, ".int", no, IntFileName, !IO),
        module_name_to_file_name(ModuleName, ".int2", no, Int2FileName, !IO),
        module_name_to_file_name(ModuleName, ".int3", no, Int3FileName, !IO),
        module_name_to_file_name(ModuleName, ".opt", no, OptFileName, !IO),
        module_name_to_file_name(ModuleName, ".trans_opt", no,
            TransOptFileName, !IO),
        module_name_to_file_name(ModuleName, ".date3", no, Date3FileName, !IO),

        % We add some extra dependencies to the generated `.d' files, so that
        % local `.int', `.opt', etc. files shadow the installed versions
        % properly (e.g. for when you're trying to build a new version
        % of an installed library). This saves the user from having to add
        % these explicitly if they have multiple libraries installed
        % in the same installation hierarchy which aren't independent (e.g.
        % one uses another). These extra dependencies are necessary due to
        % the way the combination of search paths and pattern rules
        % works in Make.
        %
        % Be very careful about changing the following rules. The `@:' is a
        % silent do-nothing command. It is used to force GNU Make to recheck
        % the timestamp on the target file.  (It is a pity that GNU Make
        % doesn't have a way of handling these sorts of rules in a
        % nicer manner.)

        io.write_strings(DepStream, [
            "\n",
            Int0FileName, " : ", Date0FileName, "\n",
            "\t@:\n",
            IntFileName, " : ", DateFileName, "\n",
            "\t@:\n",
            Int2FileName, " : ", DateFileName, "\n",
            "\t@:\n",
            Int3FileName, " : ", Date3FileName, "\n",
            "\t@:\n",
            OptFileName, " : ", OptDateFileName, "\n",
            "\t@:\n",
            TransOptFileName, " : ", TransOptDateFileName, "\n",
            "\t@:\n"
        ], !IO),

        globals.io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
        (
            UseSubdirs = yes,
            io.nl(DepStream, !IO),
            list.foldl(write_subdirs_shorthand_rule(DepStream, ModuleName),
                [".c", ".$O", ".pic_o", ".s", ".pic_s",
                ".java", ".class", ".il", ".dll"], !IO)
        ;
            UseSubdirs = no
        ),

        ( SourceFileName \= default_source_file(ModuleName) ->
            % The pattern rules in Mmake.rules won't work, since the source
            % file name doesn't match the expected source file name for this
            % module name. This can occur due to just the use of different
            % source file names, or it can be due to the use of nested modules.
            % So we need to output hard-coded rules in this case.
            %
            % The rules output below won't work in the case of nested modules
            % with parallel makes, because it will end up invoking the same
            % command twice (since it produces two output files) at the same
            % time.
            %
            % Any changes here will require corresponding changes to
            % scripts/Mmake.rules. See that file for documentation
            % on these rules.

            io.write_strings(DepStream, [
                "\n",
                Date0FileName, " : ", SourceFileName, "\n",
                "\t$(MCPI) $(ALL_GRADEFLAGS) ",
                    "$(ALL_MCPIFLAGS) ", ModuleArg, "\n",
                DateFileName, " : ", SourceFileName, "\n",
                "\t$(MCI) $(ALL_GRADEFLAGS) $(ALL_MCIFLAGS) ",
                    ModuleArg, "\n",
                Date3FileName, " : ", SourceFileName, "\n",
                "\t$(MCSI) $(ALL_GRADEFLAGS) $(ALL_MCSIFLAGS) ",
                    ModuleArg, "\n",
                OptDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCOI) $(ALL_GRADEFLAGS) $(ALL_MCOIFLAGS) ",
                    ModuleArg, "\n",
                TransOptDateFileName, " : ", SourceFileName,
                    "\n",
                "\t$(MCTOI) $(ALL_GRADEFLAGS) ",
                    "$(ALL_MCTOIFLAGS) ", ModuleArg, "\n",
                CDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    ModuleArg, " $(ERR_REDIRECT)\n",
                "ifeq ($(TARGET_ASM),yes)\n",
                AsmDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--target-code-only ", ModuleArg,
                    " $(ERR_REDIRECT)\n",
                PicAsmDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--target-code-only --pic ",
                    "\\\n",
                "\t\t--cflags ""$(GCCFLAGS_FOR_PIC)"" ",
                    ModuleArg, " $(ERR_REDIRECT)\n",
                "endif # TARGET_ASM\n",
                ILDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--il-only ", ModuleArg,
                    " $(ERR_REDIRECT)\n",
                JavaDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--java-only ", ModuleArg,
                    " $(ERR_REDIRECT)\n"
            ], !IO)
        ;
            true
        ),

        io.close_output(DepStream, !IO),
        io.rename_file(TmpDependencyFileName, DependencyFileName, Result3,
            !IO),
        (
            Result3 = error(_),
            % On some systems, we need to remove the existing file
            % first, if any.  So try again that way.
            io.remove_file(DependencyFileName, Result4, !IO),
            (
                Result4 = error(Error4),
                maybe_write_string(Verbose, " failed.\n", !IO),
                maybe_flush_output(Verbose, !IO),
                io.error_message(Error4, ErrorMsg),
                string.append_list(["can't remove file `", DependencyFileName,
                    "': ", ErrorMsg], Message),
                report_error(Message, !IO)
            ;
                Result4 = ok,
                io.rename_file(TmpDependencyFileName, DependencyFileName,
                    Result5, !IO),
                (
                    Result5 = error(Error5),
                    maybe_write_string(Verbose, " failed.\n", !IO),
                    maybe_flush_output(Verbose, !IO),
                    io.error_message(Error5, ErrorMsg),
                    string.append_list(["can't rename file `",
                        TmpDependencyFileName, "' as `", DependencyFileName,
                        "': ", ErrorMsg], Message),
                    report_error(Message, !IO)
                ;
                    Result5 = ok,
                    maybe_write_string(Verbose, " done.\n", !IO)
                )
            )
        ;
            Result3 = ok,
            maybe_write_string(Verbose, " done.\n", !IO)
        )
    ).

    % Generate the following dependency.  This dependency is
    % needed because module__cpp_code.dll might refer to
    % high level data in any of the mercury modules it
    % imports plus itself.
    % We also generate a dependency on the .il file, so that mmake
    % knows we need to generate the .il file to get the foreign language
    % source file (e.g. .cpp file).
    %
    % For example, for MC++ we generate:
    %
    %   <module>__cpp_code.dll : <module>.dll <imports>.dll
    %   <module>__cpp_code.cpp : <module>.il
    %
    % (the rule to generate .dll from .cpp is a pattern rule in
    % scripts/Mmake.rules).
    %
:- pred write_foreign_dependency_for_il(io.output_stream::in,sym_name::in,
    list(module_name)::in, foreign_import_module_info_list::in,
    foreign_language::in, io::di, io::uo) is det.

write_foreign_dependency_for_il(DepStream, ModuleName, AllDeps,
        ForeignImports, ForeignLang, !IO) :-
    (
        ForeignModuleName = foreign_language_module_name(ModuleName,
            ForeignLang),
        ForeignExt = foreign_language_file_extension(ForeignLang)
    ->
        module_name_to_make_var_name(ForeignModuleName,
            ForeignModuleNameString),
        module_name_to_file_name(ForeignModuleName, ForeignExt, no,
            ForeignFileName, !IO),
        module_name_to_file_name(ModuleName, ".il", no, IlFileName, !IO),
        module_name_to_file_name(ModuleName, ".dll", no, DllFileName, !IO),
        module_name_to_file_name(ForeignModuleName, ".dll", no,
            ForeignDllFileName, !IO),

        io.write_strings(DepStream, [
            ForeignDllFileName, " : ", DllFileName], !IO),
            % XXX This change doesn't work correctly because
            % mmake can't find the dlls which don't reside
            % in the current directory.
        % write_dll_dependencies_list(ModuleName, AllDeps, DepStream,
        %   !IO),
        io.nl(DepStream, !IO),

        io.write_strings(DepStream, [
            ForeignFileName, " : ", IlFileName, "\n\n"], !IO),

        ( ForeignLang = lang_csharp ->
            % Store in the variable
            % CSHARP_ASSEMBLY_REFS-foreign_code_name
            % the command line argument to reference all the
            % dlls the foreign code module references.
            io.write_strings(DepStream,
                ["CSHARP_ASSEMBLY_REFS-", ForeignModuleNameString, "="], !IO),
            (
                mercury_std_library_module_name(ModuleName)
            ->
                Prefix = "/addmodule:"
            ;
                Prefix = "/r:"
            ),
            ForeignDeps = list.map(
                (func(M) = foreign_import_module_name_from_module(M,
                    ModuleName)),
                ForeignImports),
            Deps = AllDeps ++ ForeignDeps,
            write_dll_dependencies_list(referenced_dlls(ModuleName, Deps),
                Prefix, DepStream, !IO),
            io.nl(DepStream, !IO)
        ;
            true
        )
    ;
        % This foreign language doesn't generate an external file
        % so there are no dependencies to generate.
        true
    ).

    % With `--use-subdirs', allow users to type `mmake module.c'
    % rather than `mmake Mercury/cs/module.c'.
    %
:- pred write_subdirs_shorthand_rule(io.output_stream::in,
    module_name::in, string::in, io::di, io::uo) is det.

write_subdirs_shorthand_rule(DepStream, ModuleName, Ext, !IO) :-
    module_name_to_file_name(ModuleName, ModuleStr),
    module_name_to_file_name(ModuleName, Ext, no, Target, !IO),
    ShorthandTarget = ModuleStr ++ Ext,
    io.write_string(DepStream, ".PHONY: ", !IO),
    io.write_string(DepStream, ShorthandTarget, !IO),
    io.nl(DepStream, !IO),
    io.write_string(DepStream, ShorthandTarget, !IO),
    io.write_string(DepStream, ": ", !IO),
    io.write_string(DepStream, Target, !IO),
    io.nl(DepStream, !IO).

maybe_read_dependency_file(ModuleName, MaybeTransOptDeps, !IO) :-
    globals.io_lookup_bool_option(transitive_optimization, TransOpt, !IO),
    (
        TransOpt = yes,
        globals.io_lookup_bool_option(verbose, Verbose, !IO),
        module_name_to_file_name(ModuleName, ".d", no, DependencyFileName,
            !IO),
        maybe_write_string(Verbose, "% Reading auto-dependency file `", !IO),
        maybe_write_string(Verbose, DependencyFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        io.open_input(DependencyFileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            io.set_input_stream(Stream, OldStream, !IO),
            module_name_to_file_name(ModuleName, ".trans_opt_date",
                no, TransOptDateFileName0, !IO),
            string.to_char_list(TransOptDateFileName0,
                TransOptDateFileName),
            list.append(TransOptDateFileName, [' ', ':'], SearchPattern),
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
            string.append_list(["error opening file `",
                DependencyFileName, "' for input: ",
                IOErrorMessage], Message),
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

    % get_both_opt_deps(Deps, Directories, OptDeps, TransOptDeps):
    %
    % For each dependency, search intermod_directories for a .m file.
    % If it exists, add it to both output lists. Otherwise, if a .opt
    % file exists, add it to the OptDeps list, and if a .trans_opt
    % file exists, add it to the TransOptDeps list.
    % If --use-opt-files is set, don't look for `.m' files, since
    % we are not building `.opt' files, only using those which
    % are available.
    % XXX This won't find nested sub-modules.
    % XXX Use `mmc --make' if that matters.
    %
:- pred get_both_opt_deps(bool::in, list(module_name)::in, list(string)::in,
    list(module_name)::out, list(module_name)::out, io::di, io::uo) is det.

get_both_opt_deps(_, [], _, [], [], !IO).
get_both_opt_deps(BuildOptFiles, [Dep | Deps], IntermodDirs,
        !:OptDeps, !:TransOptDeps, !IO) :-
    get_both_opt_deps(BuildOptFiles, Deps, IntermodDirs,
        !:OptDeps, !:TransOptDeps, !IO),
    (
        BuildOptFiles = yes,
        search_for_module_source(IntermodDirs, Dep, Result1, !IO),
        (
            Result1 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            !:TransOptDeps = [Dep | !.TransOptDeps],
            io.seen(!IO),
            Found = yes
        ;
            Result1 = error(_),
            Found = no
        )
    ;
        BuildOptFiles = no,
        Found = no
    ),
    (
        Found = no,
        module_name_to_file_name(Dep, ".opt", no, OptName, !IO),
        search_for_file(IntermodDirs, OptName, Result2, !IO),
        (
            Result2 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            io.seen(!IO)
        ;
            Result2 = error(_)
        ),
        module_name_to_file_name(Dep, ".trans_opt", no, TransOptName, !IO),
        search_for_file(IntermodDirs, TransOptName, Result3, !IO),
        (
            Result3 = ok(_),
            !:TransOptDeps = [Dep | !.TransOptDeps],
            io.seen(!IO)
        ;
            Result3 = error(_)
        )
    ;
        Found = yes
    ).

    % For each dependency, search intermod_directories for a .Suffix
    % file or a .m file, filtering out those for which the search fails.
    % If --use-opt-files is set, only look for `.opt' files,
    % not `.m' files.
    % XXX This won't find nested sub-modules.
    % XXX Use `mmc --make' if that matters.
:- pred get_opt_deps(bool::in, list(module_name)::in, list(string)::in,
    string::in, list(module_name)::out, io::di, io::uo) is det.

get_opt_deps(_, [], _, _, [], !IO).
get_opt_deps(BuildOptFiles, [Dep | Deps], IntermodDirs, Suffix, !:OptDeps,
        !IO) :-
    get_opt_deps(BuildOptFiles, Deps, IntermodDirs, Suffix, !:OptDeps, !IO),
    (
        BuildOptFiles = yes,
        search_for_module_source(IntermodDirs, Dep, Result1, !IO),
        (
            Result1 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            Found = yes,
            io.seen(!IO)
        ;
            Result1 = error(_),
            Found = no
        )
    ;
        BuildOptFiles = no,
        Found = no
    ),
    (
        Found = no,
        module_name_to_search_file_name(Dep, Suffix, OptName, !IO),
        search_for_file(IntermodDirs, OptName, Result2, !IO),
        (
            Result2 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            io.seen(!IO)
        ;
            Result2 = error(_)
        )
    ;
        Found = yes
    ).

%-----------------------------------------------------------------------------%

generate_module_dependencies(ModuleName, !IO) :-
    map.init(DepsMap),
    SearchIncludePath = no,
    generate_dependencies(output_all_dependencies,
        SearchIncludePath, ModuleName, DepsMap, !IO).

generate_file_dependencies(FileName, !IO) :-
    build_deps_map(FileName, ModuleName, DepsMap, !IO),
    SearchIncludePath = no,
    generate_dependencies(output_all_dependencies,
        SearchIncludePath, ModuleName, DepsMap, !IO).

generate_module_dependency_file(ModuleName, !IO) :-
    map.init(DepsMap),
    SearchIncludePath = yes,
    generate_dependencies(output_d_file_only,
        SearchIncludePath, ModuleName, DepsMap, !IO).

generate_file_dependency_file(FileName, !IO) :-
    build_deps_map(FileName, ModuleName, DepsMap, !IO),
    SearchIncludePath = yes,
    generate_dependencies(output_d_file_only,
        SearchIncludePath, ModuleName, DepsMap, !IO).

:- pred build_deps_map(file_name::in, module_name::out, deps_map::out,
    io::di, io::uo) is det.

build_deps_map(FileName, ModuleName, DepsMap, !IO) :-
    % read in the top-level file (to figure out its module name)
    read_mod_from_file(FileName, ".m", "Reading file", no, no, Items, Error,
        ModuleName, _, !IO),
    string.append(FileName, ".m", SourceFileName),
    split_into_submodules(ModuleName, Items, SubModuleList, [], Specs),
    sort_error_specs(Specs, SortedSpecs),
    globals.io_get_globals(Globals, !IO),
    write_error_specs(SortedSpecs, Globals, 0, _NumWarnings, 0, _NumErrors,
        !IO),
    assoc_list.keys(SubModuleList, SubModuleNames),
    list.map(init_dependencies(SourceFileName, ModuleName, SubModuleNames,
        Error, Globals), SubModuleList, ModuleImportsList),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map, ModuleImportsList, DepsMap0, DepsMap).

:- type generate_dependencies_mode
    --->    output_d_file_only
    ;       output_all_dependencies
    .

:- pred generate_dependencies(generate_dependencies_mode::in, bool::in,
    module_name::in, deps_map::in,
    io::di, io::uo) is det.

generate_dependencies(Mode, Search, ModuleName, DepsMap0, !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(set.make_singleton_set(ModuleName), Search,
        DepsMap0, DepsMap, !IO),

    %
    % Check whether we could read the main `.m' file.
    %
    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, ModuleImports),
    module_imports_get_error(ModuleImports, Error),
    ( Error = fatal_module_errors ->
        ModuleString = sym_name_to_string(ModuleName),
        string.append_list(["can't read source file for module `",
            ModuleString, "'."], Message),
        report_error(Message, !IO)
    ;
        (
            Mode = output_d_file_only,
            true
        ;
            Mode = output_all_dependencies,
            module_imports_get_source_file_name(ModuleImports, SourceFileName),
            generate_dependencies_write_dv_file(SourceFileName, ModuleName,
                DepsMap, !IO),
            generate_dependencies_write_dep_file(SourceFileName, ModuleName,
                DepsMap, !IO)
        ),

        %
        % Compute the interface deps relation and the implementation deps
        % relation from the deps map.
        %
        relation.init(IntDepsRel0),
        relation.init(ImplDepsRel0),
        map.values(DepsMap, DepsList),
        deps_list_to_deps_rel(DepsList, DepsMap,
            IntDepsRel0, IntDepsRel, ImplDepsRel0, ImplDepsRel),

        %
        % Compute the trans-opt deps ordering, by doing an approximate
        % topological sort of the implementation deps, and then finding
        % the subset of those for which of those we have (or can make)
        % trans-opt files.
        %
        relation.atsort(ImplDepsRel, ImplDepsOrdering0),
        maybe_output_module_order(ModuleName, ImplDepsOrdering0, !IO),
        list.map(set.to_sorted_list, ImplDepsOrdering0, ImplDepsOrdering),
        list.condense(ImplDepsOrdering, TransOptDepsOrdering0),
        globals.io_lookup_accumulating_option(intermod_directories,
            IntermodDirs, !IO),
        get_opt_deps(yes, TransOptDepsOrdering0, IntermodDirs, ".trans_opt",
            TransOptDepsOrdering, !IO),

        % relation.to_assoc_list(ImplDepsRel, ImplDepsAL),
        % print("ImplDepsAL:\n", !IO),
        % write_list(ImplDepsAL, "\n", print, !IO), nl(!IO),

        %
        % Compute the indirect dependencies: they are equal to the composition
        % of the implementation dependencies with the transitive closure of the
        % implementation dependencies.  (We used to take the transitive closure
        % of the interface dependencies, but we now include implementation
        % details in the interface files).
        %
        relation.tc(ImplDepsRel, TransImplDepsRel),
        relation.compose(ImplDepsRel, TransImplDepsRel, IndirectDepsRel),

        %
        % Compute the indirect optimization dependencies: indirect
        % dependencies including those via `.opt' or `.trans_opt' files.
        % Actually we can't compute that, since we don't know
        % which modules the `.opt' files will import!
        % Instead, we need to make a conservative (over-)approximation,
        % and assume that the each module's `.opt' file might import any
        % of that module's implementation dependencies; in actual fact,
        % it will be some subset of that.
        %
        relation.tc(ImplDepsRel, IndirectOptDepsRel),

        % write_relations("Rel", IntDepsRel, TransIntDepsRel,
        %   ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel),

        (
            Mode = output_d_file_only,
            DFilesToWrite = [ModuleDep]
        ;
            Mode = output_all_dependencies,
            DFilesToWrite = DepsList
        ),
        generate_dependencies_write_d_files(DFilesToWrite,
            IntDepsRel, ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel,
            TransOptDepsOrdering, DepsMap, !IO)
    ),
    %
    % For Java, the main target is actually a shell script which will
    % set CLASSPATH appropriately and invoke java on the appropriate
    % .class file.  Rather than generating an Mmake rule to build this
    % file when it is needed, we just generate this file "mmake depend"
    % time, since that is simpler and probably more efficient anyway.
    %
    globals.io_get_target(Target, !IO),
    (
        Target = target_java,
        Mode = output_all_dependencies
    ->
        create_java_shell_script(ModuleName, _Succeeded, !IO)
    ;
        true
    ).

%   % Output the various relations into a file which can be
%   % processed by the dot package to draw the relations.
%   %
% :- pred write_relations(string::in, relation(sym_name)::in,
%   relation(sym_name)::in, relation(sym_name)::in,
%   relation(sym_name)::in, relation(sym_name)::in, io::di, io::uo) is det.
%
% write_relations(FileName, IntDepsRel, TransIntDepsRel,
%       ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel) -->
%   io.open_output(FileName, Result),
%   ( { Result = ok(Stream) } ->
%       write_relation(Stream, "IntDepsRel", IntDepsRel),
%       write_relation(Stream, "TransIntDepsRel", TransIntDepsRel),
%       write_relation(Stream, "ImplDepsRel", ImplDepsRel),
%       write_relation(Stream, "IndirectDepsRel", IndirectDepsRel),
%       write_relation(Stream, "IndirectOptDepsRel",
%           IndirectOptDepsRel)
%   ;
%       { error("unable to open file: " ++ FileName) }
%   ).
%
% :- pred write_relation(io.output_stream::in, string::in,
%   relation(sym_name)::in, io::di, io::uo) is det.
%
% write_relation(Stream, Name, Relation) -->
%   io.write_string(Stream, "digraph " ++ Name ++ " {\n"),
%   io.write_string(Stream, "label=\"" ++ Name ++ "\";\n"),
%   io.write_string(Stream, "center=true;\n"),
%   relation.traverse(Relation, write_node(Stream), write_edge(Stream)),
%   io.write_string(Stream, "}\n").
%
% :- pred write_node(io.output_stream::in, sym_name::in, io::di, io::uo)
%   is det.
%
% write_node(Stream, Node) -->
%   { sym_name_to_string(Node, "__", NodeStr) },
%   io.write_string(Stream, NodeStr),
%   io.write_string(Stream, ";\n").
%
% :- pred write_edge(io.output_stream::in, sym_name::in, sym_name::in,
%   io::di, io::uo) is det.
%
% write_edge(Stream, A, B) -->
%   { sym_name_to_string(A, "__", AStr) },
%   { sym_name_to_string(B, "__", BStr) },
%   io.write_string(Stream, AStr),
%   io.write_string(Stream, " -> "),
%   io.write_string(Stream, BStr),
%   io.write_string(Stream, ";\n").

:- pred maybe_output_module_order(module_name::in, list(set(module_name))::in,
    io::di, io::uo) is det.

maybe_output_module_order(Module, DepsOrdering, !IO) :-
    globals.io_lookup_bool_option(generate_module_order, Order, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    (
        Order = yes,
        module_name_to_file_name(Module, ".order", yes, OrdFileName, !IO),
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

    % generate_dependencies_write_d_files(Modules, IntDepsRel, ImplDepsRel,
    %   IndirectDepsRel, IndirectOptDepsRel, TransOptOrder, DepsMap, !IO):
    %
    % This predicate writes out the .d files for all the modules in the
    % Modules list.
    % IntDepsRel gives the interface dependency relation.
    % ImplDepsRel gives the implementation dependency relation
    % IndirectDepsRel gives the indirect dependency relation
    % (this includes dependencies on `*.int2' files).
    % IndirectOptDepsRel gives the indirect optimization dependencies
    % (this includes dependencies via `.opt' and `.trans_opt' files).
    % These are all computed from the DepsMap.
    % TransOptOrder gives the ordering that is used to determine
    % which other modules the .trans_opt files may depend on.
    %
:- pred generate_dependencies_write_d_files(list(deps)::in,
    deps_rel::in, deps_rel::in, deps_rel::in, deps_rel::in,
    list(module_name)::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_d_files([], _, _, _, _, _, _, !IO).
generate_dependencies_write_d_files([Dep | Deps],
        IntDepsRel, ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel,
        TransOptOrder, DepsMap, !IO) :-
    some [!Module] (
        Dep = deps(_, !:Module),

        %
        % Look up the interface/implementation/indirect dependencies
        % for this module from the respective dependency relations,
        % and save them in the module_imports structure.
        %
        module_imports_get_module_name(!.Module, ModuleName),
        get_dependencies_from_relation(IndirectOptDepsRel, ModuleName,
            IndirectOptDeps),
        globals.io_lookup_bool_option(intermodule_optimization, Intermod,
            !IO),
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
            get_dependencies_from_relation(IntDepsRel, ModuleName, IntDeps),
            get_dependencies_from_relation(ImplDepsRel, ModuleName, ImplDeps),
            get_dependencies_from_relation(IndirectDepsRel, ModuleName,
                IndirectDeps)
        ),

        globals.io_get_target(Target, !IO),
        ( Target = target_c, Lang = lang_c
        ; Target = target_asm, Lang = lang_c
        ; Target = target_java, Lang = lang_java
        ; Target = target_il, Lang = lang_il
        ),
        % Assume we need the `.mh' files for all imported modules
        % (we will if they define foreign types).
        ForeignImports = list.map(
            (func(ThisDep) = foreign_import_module_info(Lang, ThisDep,
                term.context_init)),
            IndirectOptDeps),
        !:Module = !.Module ^ foreign_import_modules := ForeignImports,

        module_imports_set_int_deps(IntDeps, !Module),
        module_imports_set_impl_deps(ImplDeps, !Module),
        module_imports_set_indirect_deps(IndirectDeps, !Module),

        %
        % Compute the trans-opt dependencies for this module. To avoid
        % the possibility of cycles, each module is only allowed to depend
        % on modules that occur later than it in the TransOptOrder.
        %
        FindModule = (pred(OtherModule::in) is semidet :-
            ModuleName \= OtherModule
        ),
        list.takewhile(FindModule, TransOptOrder, _, TransOptDeps0),
        ( TransOptDeps0 = [ _ | TransOptDeps1 ] ->
            % The module was found in the list.
            TransOptDeps = TransOptDeps1
        ;
            TransOptDeps = []
        ),

        % Note that even if a fatal error occured for one of the files
        % that the current Module depends on, a .d file is still produced,
        % even though it probably contains incorrect information.
        module_imports_get_error(!.Module, Error),
        ( Error \= fatal_module_errors ->
            write_dependency_file(!.Module, set.list_to_set(IndirectOptDeps),
                yes(TransOptDeps), !IO)
        ;
            true
        ),
        generate_dependencies_write_d_files(Deps, IntDepsRel, ImplDepsRel,
            IndirectDepsRel, IndirectOptDepsRel, TransOptOrder, DepsMap, !IO)
    ).

:- pred get_dependencies_from_relation(deps_rel::in, module_name::in,
    list(module_name)::out) is det.

get_dependencies_from_relation(DepsRel0, ModuleName, Deps) :-
    svrelation.add_element(ModuleName, ModuleKey, DepsRel0, DepsRel),
    relation.lookup_key_set_from(DepsRel, ModuleKey, DepsKeysSet),
    sparse_bitset.foldl(
        (pred(Key::in, Deps0::in, [Dep | Deps0]::out) is det :-
            relation.lookup_key(DepsRel, Key, Dep)
        ), DepsKeysSet, [], Deps).

% This is the data structure we use to record the dependencies.
% We keep a map from module name to information about the module.

:- type deps_map == map(module_name, deps).
:- type deps
    --->    deps(
                bool,       % have we processed this module yet?
                module_imports
            ).

    % (Module1 deps_rel Module2) means Module1 is imported by Module2.
:- type deps_rel == relation(module_name).

:- pred generate_deps_map(set(module_name)::in, bool::in,
    deps_map::in, deps_map::out, io::di, io::uo) is det.

generate_deps_map(!.Modules, Search, !DepsMap, !IO) :-
    ( set.remove_least(!.Modules, Module, !:Modules) ->
        % Look up the module's dependencies, and determine whether
        % it has been processed yet.
        lookup_dependencies(Module, Search, Done, !DepsMap, ModuleImports,
            !IO),

        % If the module hadn't been processed yet, then add its imports,
        % parents, and public children to the list of dependencies we need
        % to generate, and mark it as having been processed.
        (
            Done = no,
            map.set(!.DepsMap, Module, deps(yes, ModuleImports), !:DepsMap),
            ForeignImportedModules =
                list.map(
                    (func(foreign_import_module_info(_, ImportedModule, _))
                        = ImportedModule),
                    ModuleImports ^ foreign_import_modules),
            list.condense(
                [ModuleImports ^ parent_deps,
                ModuleImports ^ int_deps,
                ModuleImports ^ impl_deps,
                ModuleImports ^ public_children, % a.k.a. incl_deps
                ForeignImportedModules],
                ModulesToAdd),
            % We could keep a list of the modules we have already processed
            % and subtract it from ModulesToAddSet here, but doing that
            % actually leads to a small slowdown.
            set.list_to_set(ModulesToAdd, ModulesToAddSet),
            set.union(ModulesToAddSet, !Modules)
        ;
            Done = yes
        ),
        % Recursively process the remaining modules.
        generate_deps_map(!.Modules, Search, !DepsMap, !IO)
    ;
        % If we can't remove the smallest, then the set of modules to be
        % processed is empty.
        true
    ).

    % Construct a pair of dependency relations (the interface dependencies
    % and the implementation dependencies) for all the modules in the program.
    %
:- pred deps_list_to_deps_rel(list(deps)::in, deps_map::in,
    deps_rel::in, deps_rel::out, deps_rel::in, deps_rel::out) is det.

deps_list_to_deps_rel([], _, !IntRel, !ImplRel).
deps_list_to_deps_rel([Deps | DepsList], DepsMap, !IntRel, !ImplRel) :-
    Deps = deps(_, ModuleImports),
    ModuleError = ModuleImports ^ error,
    ( ModuleError \= fatal_module_errors ->
        module_imports_to_deps_rel(ModuleImports,
            lookup_module_imports(DepsMap), !IntRel, !ImplRel)
    ;
        true
    ),
    deps_list_to_deps_rel(DepsList, DepsMap, !IntRel, !ImplRel).

:- func lookup_module_imports(deps_map, module_name) = module_imports.

lookup_module_imports(DepsMap, ModuleName) = ModuleImports :-
    map.lookup(DepsMap, ModuleName, deps(_, ModuleImports)).

add_module_relations(LookupModuleImports, ModuleName, !IntRel, !ImplRel) :-
    ModuleImports = LookupModuleImports(ModuleName),
    module_imports_to_deps_rel(ModuleImports, LookupModuleImports,
        !IntRel, !ImplRel).

:- pred module_imports_to_deps_rel(module_imports::in,
    lookup_module_imports::lookup_module_imports,
    deps_rel::in, deps_rel::out, deps_rel::in, deps_rel::out) is det.

module_imports_to_deps_rel(ModuleImports, LookupModuleImports,
        !IntRel, !ImplRel) :-
    %
    % Add interface dependencies to the interface deps relation.
    %
    % Note that we need to do this both for the interface imports
    % of this module and for the *implementation* imports of
    % its ancestors.  This is because if this module is defined
    % in the implementation section of its parent, then the
    % interface of this module may depend on things
    % imported only by its parent's implementation.
    %
    % If this module was actually defined in the interface
    % section of one of its ancestors, then it should only
    % depend on the interface imports of that ancestor,
    % so the dependencies added here are in fact more
    % conservative than they need to be in that case.
    % However, that should not be a major problem.
    %
    ModuleName = ModuleImports ^ module_name,
    ParentDeps = ModuleImports ^ parent_deps,
    svrelation.add_element(ModuleName, IntModuleKey, !IntRel),
    add_int_deps(IntModuleKey, ModuleImports, !IntRel),
    add_parent_impl_deps_list(LookupModuleImports, IntModuleKey, ParentDeps,
        !IntRel),

    %
    % Add implementation dependencies to the impl. deps relation.
    % (The implementation dependencies are a superset of the
    % interface dependencies.)
    %
    % Note that we need to do this both for the imports
    % of this module and for the imports of its parents,
    % because this module may depend on things imported
    % only by its parents.
    %
    svrelation.add_element(ModuleName, ImplModuleKey, !ImplRel),
    add_impl_deps(ImplModuleKey, ModuleImports, !ImplRel),
    add_parent_impl_deps_list(LookupModuleImports, ImplModuleKey, ParentDeps,
        !ImplRel).

    % Add interface dependencies to the interface deps relation.
    %
:- pred add_int_deps(relation_key::in, module_imports::in,
    deps_rel::in, deps_rel::out) is det.

add_int_deps(ModuleKey, ModuleImports, Rel0, Rel) :-
    AddDep = add_dep(ModuleKey),
    list.foldl(AddDep, ModuleImports ^ parent_deps, Rel0, Rel1),
    list.foldl(AddDep, ModuleImports ^ int_deps, Rel1, Rel).

    % Add direct implementation dependencies for a module to the
    % implementation deps relation.
    %
:- pred add_impl_deps(relation_key::in, module_imports::in,
    deps_rel::in, deps_rel::out) is det.

add_impl_deps(ModuleKey, ModuleImports, !Rel) :-
    % The implementation dependencies are a superset of the
    % interface dependencies, so first we add the interface deps.
    add_int_deps(ModuleKey, ModuleImports, !Rel),
    % then we add the impl deps
    module_imports_get_impl_deps(ModuleImports, ImplDeps),
    list.foldl(add_dep(ModuleKey), ImplDeps, !Rel).

    % Add parent implementation dependencies for the given Parent module
    % to the impl. deps relation values for the given ModuleKey.
    %
:- pred add_parent_impl_deps(lookup_module_imports::lookup_module_imports,
    relation_key::in, module_name::in, deps_rel::in, deps_rel::out) is det.

add_parent_impl_deps(LookupModuleImports, ModuleKey, Parent, !Rel) :-
    ParentModuleImports = LookupModuleImports(Parent),
    add_impl_deps(ModuleKey, ParentModuleImports, !Rel).

:- pred add_parent_impl_deps_list(lookup_module_imports::lookup_module_imports,
    relation_key::in, list(module_name)::in, deps_rel::in, deps_rel::out)
    is det.

add_parent_impl_deps_list(LookupModuleImports, ModuleKey, Parents, !Rel) :-
    list.foldl(add_parent_impl_deps(LookupModuleImports, ModuleKey), Parents,
        !Rel).

    % Add a single dependency to a relation.
    %
:- pred add_dep(relation_key::in, T::in, relation(T)::in, relation(T)::out)
    is det.

add_dep(ModuleRelKey, Dep, !Rel) :-
    svrelation.add_element(Dep, DepRelKey, !Rel),
    svrelation.add(ModuleRelKey, DepRelKey, !Rel).

:- type submodule_kind
    --->    toplevel
    ;       nested_submodule
    ;       separate_submodule.

    % Check if a module is a top-level module, a nested sub-module,
    % or a separate sub-module.
    %
:- func get_submodule_kind(module_name, deps_map) = submodule_kind.

get_submodule_kind(ModuleName, DepsMap) = Kind :-
    Ancestors = get_ancestors(ModuleName),
    ( list.last(Ancestors, Parent) ->
        map.lookup(DepsMap, ModuleName, deps(_, ModuleImports)),
        map.lookup(DepsMap, Parent, deps(_, ParentImports)),
        ModuleFileName = ModuleImports ^ source_file_name,
        ParentFileName = ParentImports ^ source_file_name,
        ( ModuleFileName = ParentFileName ->
            Kind = nested_submodule
        ;
            Kind = separate_submodule
        )
    ;
        Kind = toplevel
    ).

%-----------------------------------------------------------------------------%

    % Write out the `.dv' file, using the information collected in the
    % deps_map data structure.
    %
:- pred generate_dependencies_write_dv_file(file_name::in, module_name::in,
    deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_dv_file(SourceFileName, ModuleName, DepsMap,
        !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    module_name_to_file_name(ModuleName, ".dv", yes, DvFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DvFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io.open_output(DvFileName, DvResult, !IO),
    (
        DvResult = ok(DvStream),
        generate_dv_file(SourceFileName, ModuleName, DepsMap, DvStream, !IO),
        io.close_output(DvStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        DvResult = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.append_list(["error opening file `", DvFileName,
            "' for output: ", IOErrorMessage], DvMessage),
        report_error(DvMessage, !IO)
    ).

    % Write out the `.dep' file, using the information collected in the
    % deps_map data structure.
    %
:- pred generate_dependencies_write_dep_file(file_name::in, module_name::in,
    deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_dep_file(SourceFileName, ModuleName, DepsMap,
        !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    module_name_to_file_name(ModuleName, ".dep", yes, DepFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DepFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io.open_output(DepFileName, DepResult, !IO),
    (
        DepResult = ok(DepStream),
        generate_dep_file(SourceFileName, ModuleName, DepsMap, DepStream, !IO),
        io.close_output(DepStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        DepResult = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.append_list(["error opening file `", DepFileName,
            "' for output: ", IOErrorMessage], DepMessage),
        report_error(DepMessage, !IO)
    ).

:- pred compare_module_names(module_name::in, module_name::in,
    comparison_result::out) is det.

compare_module_names(Sym1, Sym2, Result) :-
    Str1 = sym_name_to_string(Sym1),
    Str2 = sym_name_to_string(Sym2),
    compare(Result, Str1, Str2).

:- pred generate_dv_file(file_name::in, module_name::in, deps_map::in,
    io.output_stream::in, io::di, io::uo) is det.

generate_dv_file(SourceFileName, ModuleName, DepsMap, DepStream, !IO) :-
    io.write_string(DepStream,
        "# Automatically generated dependency variables for module `", !IO),
    ModuleNameString = sym_name_to_string(ModuleName),
    io.write_string(DepStream, ModuleNameString, !IO),
    io.write_string(DepStream, "'\n", !IO),
    io.write_string(DepStream, "# generated from source file `", !IO),
    io.write_string(DepStream, SourceFileName, !IO),
    io.write_string(DepStream, "'\n", !IO),

    library.version(Version),
    io.write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io.write_string(DepStream, Version, !IO),
    io.write_string(DepStream, ".\n\n", !IO),

    map.keys(DepsMap, Modules0),
    select_ok_modules(Modules0, DepsMap, Modules1),
    list.sort(compare_module_names, Modules1, Modules),

    module_name_to_make_var_name(ModuleName, MakeVarName),
    list.map(get_source_file(DepsMap), Modules, SourceFiles0),
    list.sort_and_remove_dups(SourceFiles0, SourceFiles),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ms =", !IO),
    write_file_dependencies_list(SourceFiles, ".m", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".errs =", !IO),
    write_file_dependencies_list(SourceFiles, ".err", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mods =", !IO),
    write_dependencies_list(Modules, "", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    globals.io_get_target(Target, !IO),
    ( 
        Target = target_il,
        ForeignModulesAndExts = foreign_modules(Modules, DepsMap)
    ;
        ( Target = target_c
        ; Target = target_java
        ; Target = target_asm
        ),
        ForeignModulesAndExts = []
    ),
    ForeignModules = assoc_list.keys(ForeignModulesAndExts),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign =", !IO),
    write_dependencies_list(ForeignModules, "", DepStream, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    globals.io_lookup_bool_option(assume_gmake, Gmake, !IO),
    (
        Gmake = yes,
        string.append(MakeVarName, ".mods", ModsVarName),
        Basis = yes(ModsVarName - ""),

        string.append(MakeVarName, ".foreign", ForeignVarName),
        ForeignBasis = yes(ForeignVarName - "")
    ;
        Gmake = no,
        Basis = no,
        ForeignBasis = no
    ),

    get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs),

    MakeFileName = (pred(M - E::in, F::out, IO0::di, IO::uo) is det :-
        module_name_to_file_name(M, E, yes, F0, IO0, IO),
        F = "$(os_subdir)" ++ F0
    ),

    list.map_foldl(MakeFileName, ForeignModulesAndExts, ForeignFileNames,
        !IO),

        % .foreign_cs are the source files which have had
        % foreign code placed in them.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign_cs = ", !IO),
    write_file_dependencies_list(ForeignFileNames, "", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

        % The dlls which contain the foreign_code.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign_dlls = ", !IO),
    write_compact_dependencies_list(ForeignModules, "$(dlls_subdir)",
        ".dll", ForeignBasis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".init_cs = ", !IO),
    write_compact_dependencies_list(Modules, "$(cs_subdir)", ".c",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".cs = $(", !IO),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".init_cs) ", !IO),
    write_extra_link_dependencies_list(ExtraLinkObjs, ".c", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dlls = ", !IO),
    write_compact_dependencies_list(Modules, "$(dlls_subdir)", ".dll",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_ss = ", !IO),
    write_compact_dependencies_list(Modules, "$(ss_subdir)", ".s",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_ss = ", !IO),
    write_compact_dependencies_list(Modules, "$(pic_ss_subdir)", ".pic_s",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_s_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(s_dates_subdir)",
        ".s_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_s_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(pic_s_dates_subdir)",
        ".pic_s_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_os = ", !IO),
    write_compact_dependencies_list(Modules, "$(os_subdir)", ".$O",
        Basis, DepStream, !IO),
    write_extra_link_dependencies_list(ExtraLinkObjs, ".$O", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_os = ", !IO),
    write_compact_dependencies_list(Modules, "$(os_subdir)",
        ".$(EXT_FOR_PIC_OBJECTS)", Basis, DepStream, !IO),
    write_extra_link_dependencies_list(ExtraLinkObjs,
        ".$(EXT_FOR_PIC_OBJECTS)", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    IsNested = (pred(Mod::in) is semidet :-
        get_submodule_kind(Mod, DepsMap) = nested_submodule
    ),
    (
        % For --target asm, we only generate separate object files
        % for top-level modules and separate sub-modules, not for
        % nested sub-modules.
        Target = target_asm,
        list.filter(IsNested, Modules, NestedModules, MainModules),
        NestedModules = [_ | _]
    ->
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".ss = ", !IO),
        write_dependencies_list(MainModules, ".s", DepStream, !IO),
        io.write_string(DepStream, "\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".pic_ss = ", !IO),
        write_dependencies_list(MainModules, ".pic_s", DepStream, !IO),
        io.write_string(DepStream, "\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".s_dates = ", !IO),
        write_dependencies_list(MainModules, ".s_date", DepStream, !IO),
        io.write_string(DepStream, "\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".pic_s_dates = ", !IO),
        write_dependencies_list(MainModules, ".pic_s_date", DepStream, !IO),
        io.write_string(DepStream, "\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".os = ", !IO),
        write_dependencies_list(MainModules, ".$O", DepStream, !IO),
        write_extra_link_dependencies_list(ExtraLinkObjs, ".$O", DepStream,
            !IO),
        io.write_string(DepStream, "\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".pic_os = ", !IO),
        write_dependencies_list(MainModules, ".$(EXT_FOR_PIC_OBJECTS)",
            DepStream, !IO),
        write_extra_link_dependencies_list(ExtraLinkObjs,
            ".$(EXT_FOR_PIC_OBJECTS)", DepStream, !IO),
        io.write_string(DepStream, "\n", !IO)
    ;
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".ss = $(", !IO),
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".all_ss)\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".pic_ss = $(", !IO),
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".all_pic_ss)\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".s_dates = $(", !IO),
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".all_s_dates)\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".pic_s_dates = $(", !IO),
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".all_pic_s_dates)\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".os = $(", !IO),
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".all_os)\n", !IO),

        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".pic_os = $(", !IO),
        io.write_string(DepStream, MakeVarName, !IO),
        io.write_string(DepStream, ".all_pic_os)\n", !IO)
    ),

    %
    % $(foo.cs_or_ss) contains the names of the generated intermediate
    % files between `.m' and `.o' files. This is used in foo.dep
    % to make sure the intermediate files are generated before the
    % object files, so that errors are reported as soon as possible.
    %
    % If TARGET_ASM=yes, we define $(foo.cs_or_ss) to be $(foo.ss),
    % otherwise it is defined to be $(foo.cs).
    %
    io.write_strings(DepStream, [
        "ifeq ($(TARGET_ASM),yes)\n",
        MakeVarName, ".cs_or_ss =$(", MakeVarName, ".ss)\n",
        "else\n",
        MakeVarName, ".cs_or_ss =$(", MakeVarName, ".cs)\n",
        "endif\n\n"
    ], !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".useds = ", !IO),
    write_compact_dependencies_list(Modules, "$(useds_subdir)", ".used",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ils = ", !IO),
    write_compact_dependencies_list(Modules, "$(ils_subdir)", ".il",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".javas = ", !IO),
    write_compact_dependencies_list(Modules, "$(javas_subdir)", ".java",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".classes = ", !IO),
    write_compact_dependencies_list(Modules, "$(classes_subdir)", ".class",
        Basis, DepStream, !IO),
    io.write_string(DepStream, " ", !IO),
    % The Java compiler creates a .class file for each class
    % within the original .java file.  The filenames of all
    % these can be matched with `module\$*.class', hence the
    % "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    % XXX This relies on GNU Make.
    io.write_string(DepStream, "$(wildcard ", !IO),
    write_compact_dependencies_list(Modules, "$(classes_subdir)",
        "\\$$*.class", Basis, DepStream, !IO),
    io.write_string(DepStream, ")\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dirs = ", !IO),
    write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dir_os = ", !IO),
    write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir/*.$O",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(dates_subdir)", ".date",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".date0s = ", !IO),
    write_compact_dependencies_list(Modules, "$(date0s_subdir)", ".date0",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".date3s = ", !IO),
    write_compact_dependencies_list(Modules, "$(date3s_subdir)", ".date3",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".optdates = ", !IO),
    write_compact_dependencies_list(Modules, "$(optdates_subdir)",
        ".optdate", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".trans_opt_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(trans_opt_dates_subdir)",
        ".trans_opt_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".c_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(c_dates_subdir)",
        ".c_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".il_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(il_dates_subdir)",
        ".il_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".java_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(java_dates_subdir)",
        ".java_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ds = ", !IO),
    write_compact_dependencies_list(Modules, "$(ds_subdir)", ".d",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".module_deps = ", !IO),
    write_compact_dependencies_list(Modules, "$(module_deps_subdir)",
        make_module_dep_file_extension, Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mihs = ", !IO),
    globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
    (
        HighLevelCode = yes,
        ( ( Target = target_c ; Target = target_asm ) ->
            % For the `--target c' MLDS back-end, we
            % generate `.mih' files for every module.
            % Likewise for the `--target asm' back-end.
            % (For the `--target asm' back-end,
            % we previously used to do that only for modules
            % that contain C code, but this caused trouble
            % when trying to interoperate between compiled
            % with `--target c' and code compiled with
            % `--target asm', so now we generate them
            % unconditionally.)
            write_compact_dependencies_list(Modules,
                "$(mihs_subdir)", ".mih", Basis, DepStream, !IO)
        ;
            % For the IL and Java targets, currently we don't generate
            % `.mih' files at all; although perhaps we should...
            true
        )
    ;
        % For the LLDS back-end, we don't use `.mih' files at all
        HighLevelCode = no
    ),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mhs = ", !IO),
    ( ( Target = target_c ; Target = target_asm ) ->
        write_compact_dependencies_list(Modules, "", ".mh",
            Basis, DepStream, !IO)
    ;
        true
    ),
    io.write_string(DepStream, "\n", !IO),

    % The `<module>.all_mihs' variable is like `<module>.mihs' except
    % that it contains header files for all the modules, regardless
    % of the grade or --target option.  It is used by the rule for
    % `mmake realclean', which should remove anything that could have
    % been automatically generated, even if the grade or --target option
    % has changed.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_mihs = ", !IO),
    write_compact_dependencies_list(Modules, "$(mihs_subdir)", ".mih",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except
    % that it contains header files for all the modules, as for
    % `<module>.all_mihs' above.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_mhs = ", !IO),
    write_compact_dependencies_list(Modules, "", ".mh", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ints = ", !IO),
    write_compact_dependencies_list(Modules, "$(ints_subdir)", ".int",
        Basis, DepStream, !IO),
    write_compact_dependencies_separator(Basis, DepStream, !IO),
    write_compact_dependencies_list(Modules, "$(int2s_subdir)", ".int2",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % `.int0' files are only generated for modules with sub-modules.
    %
    % XXX The dependencies for nested submodules are wrong - we
    % currently end up generating .int0 files for nested submodules that
    % don't have any children (the correct thing is done for separate
    % submodules).  The following commented out code generates the
    % correct rules for .int0 files; it and the line below can be
    % uncommented when the dependency problem is fixed.
    %
    % ModulesWithSubModules = list.filter(
    %   (pred(Module::in) is semidet :-
    %       map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    %       ModuleImports ^ children = [_ | _]
    %   ), Modules),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".int0s = ", !IO),
    %
    % These next two lines are a workaround for the bug described above.
    %
    write_compact_dependencies_list(Modules, "$(int0s_subdir)", ".int0",
        Basis, DepStream, !IO),
    write_compact_dependencies_separator(Basis, DepStream, !IO),
    %
    % End of workaround - it can be deleted when the bug described above
    % is fixed.  When that happens the following line needs to be
    % uncommented.
    %
    %write_dependencies_list(ModulesWithSubModules, ".int0", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".int3s = ", !IO),
    write_compact_dependencies_list(Modules, "$(int3s_subdir)", ".int3",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".opts = ", !IO),
    write_compact_dependencies_list(Modules, "$(opts_subdir)", ".opt",
        Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".trans_opts = ", !IO),
    write_compact_dependencies_list(Modules, "$(trans_opts_subdir)",
        ".trans_opt", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".analysiss = ", !IO),
    write_compact_dependencies_list(Modules, "$(analysiss_subdir)",
        ".analysis", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".requests = ", !IO),
    write_compact_dependencies_list(Modules, "$(requests_subdir)",
        ".request", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".imdgs = ", !IO),
    write_compact_dependencies_list(Modules, "$(imdgs_subdir)",
        ".imdg", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".profs = ", !IO),
    write_compact_dependencies_list(Modules, "", ".prof", Basis, DepStream,
        !IO),
    io.write_string(DepStream, "\n\n", !IO).

:- pred generate_dep_file(file_name::in, module_name::in, deps_map::in,
    io.output_stream::in, io::di, io::uo) is det.

generate_dep_file(SourceFileName, ModuleName, DepsMap, DepStream, !IO) :-
    io.write_string(DepStream,
        "# Automatically generated dependencies for module `", !IO),
    ModuleNameString = sym_name_to_string(ModuleName),
    io.write_string(DepStream, ModuleNameString, !IO),
    io.write_string(DepStream, "'\n", !IO),
    io.write_string(DepStream,
        "# generated from source file `", !IO),
    io.write_string(DepStream, SourceFileName, !IO),
    io.write_string(DepStream, "'\n", !IO),

    library.version(Version),
    io.write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io.write_string(DepStream, Version, !IO),
    io.write_string(DepStream, ".\n\n", !IO),

    module_name_to_make_var_name(ModuleName, MakeVarName),

    module_name_to_file_name(ModuleName, ".init", yes, InitFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.s", no, InitAsmFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.$O", yes, InitObjFileName,
        !IO),
    module_name_to_file_name(ModuleName, "_init.pic_o", yes,
        InitPicObjFileName, !IO),

    % Note we have to do some ``interesting'' hacks to get
    % `$(ALL_MLLIBS_DEP)' to work in the dependency list
    % (and not complain about undefined variables).
    % These hacks rely on features of GNU Make, so should not be used
    % if we cannot assume we are using GNU Make.
    globals.io_lookup_bool_option(assume_gmake, Gmake, !IO),
    (
        Gmake = yes,
        append_list(["\\\n\t\t$(foreach @,", MakeVarName,
            ",$(ALL_MLLIBS_DEP))"],
            All_MLLibsDepString),
        append_list(["\\\n\t\t$(foreach @,", MakeVarName,
            ",$(ALL_MLOBJS))"],
            All_MLObjsString),
        append_list([
        "\\\n\t\t$(patsubst %.o,%.$(EXT_FOR_PIC_OBJECTS),$(foreach @,",
            MakeVarName, ",$(ALL_MLOBJS)))"],
            All_MLPicObjsString)
    ;
        Gmake = no,
        All_MLLibsDepString = "$(ALL_MLLIBS_DEP)",
        All_MLObjsString = "$(ALL_MLOBJS)",
        All_MLPicObjsString = "$(ALL_MLPICOBJS)"
    ),

    %
    % When compiling to C, we want to include $(foo.cs) first in
    % the dependency list, before $(foo.os).
    % This is not strictly necessary, since the .$O files themselves depend
    % on the .c files, but want to do it to ensure that Make will try to
    % create all the C files first, thus detecting errors early,
    % rather than first spending time compiling C files to .$O,
    % which could be a waste of time if the program contains errors.
    %
    % When compiling to assembler, we want to do the same kind of
    % thing, for the same reason, but with the `.s' files rather
    % than the `.c' files.
    %

    module_name_to_file_name(ModuleName, "", no, ExeFileName, !IO),

    IfIL = ["ifeq ($(findstring il,$(GRADE)),il)\n"],
    ILMainRule = [ExeFileName, " : ", ExeFileName, ".exe\n",
        ExeFileName, ".exe : ", "$(", MakeVarName, ".dlls) ",
        "$(", MakeVarName, ".foreign_dlls)\n"],
    Else = ["else\n"],
    IfJava = [" ifeq ($(findstring java,$(GRADE)),java)\n"],
    JavaMainRule = [ExeFileName, " : $(", MakeVarName, ".classes)\n"],
    Else2 = [" else\n"],
    EndIf2 = [" endif\n"],

    % XXX the output here is GNU Make-specific
    io.write_strings(DepStream, [
        "ifneq ($(EXT_FOR_EXE),)\n",
        ".PHONY : ", ExeFileName, "\n",
        ExeFileName, " : ", ExeFileName, "$(EXT_FOR_EXE)\n",
        "endif\n"
    ], !IO),

    MainRule =
        [ExeFileName, "$(EXT_FOR_EXE) : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".os) ", InitObjFileName, " ",
            All_MLObjsString, " ", All_MLLibsDepString, "\n",
        "\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) ",
            "-o ", ExeFileName, "$(EXT_FOR_EXE) ", InitObjFileName, " \\\n",
        "\t\t$(", MakeVarName, ".os) ", All_MLObjsString, " $(ALL_MLLIBS)\n"],
    EndIf = ["endif\n"],

    globals.io_get_target(Target, !IO),
    (
        Gmake = yes,
        Rules = IfIL ++ ILMainRule ++ Else ++
            IfJava ++ JavaMainRule ++ Else2 ++
            MainRule ++ EndIf2 ++ EndIf
    ;
        Gmake = no,
        (
            Target = target_il,
            Rules = ILMainRule
        ;
            Target = target_java,
            Rules = JavaMainRule
        ;
            ( Target = target_c
            ; Target = target_asm
            ),
            Rules = MainRule
        )
    ),
    io.write_strings(DepStream, Rules, !IO),

    globals.io_lookup_bool_option(intermodule_optimization, Intermod, !IO),
    (
        Intermod = yes,
        string.append_list(["$(", MakeVarName, ".opts) "], MaybeOptsVar)
    ;
        Intermod = no,
        MaybeOptsVar = ""
    ),
    globals.io_lookup_bool_option(transitive_optimization, TransOpt, !IO),
    (
        TransOpt = yes,
        string.append_list(["$(", MakeVarName, ".trans_opts) "],
            MaybeTransOptsVar)
    ;
        TransOpt = no,
        MaybeTransOptsVar = ""
    ),
    globals.io_lookup_bool_option(generate_mmc_make_module_dependencies,
        MmcMakeDeps, !IO),
    (
        MmcMakeDeps = yes,
        string.append_list(["$(", MakeVarName, ".module_deps) "],
            MaybeModuleDepsVar)
    ;
        MmcMakeDeps = no,
        MaybeModuleDepsVar = ""
    ),

    module_name_to_lib_file_name("lib", ModuleName, "", no, LibTargetName,
        !IO),
    module_name_to_lib_file_name("lib", ModuleName, ".$A", yes, LibFileName,
        !IO),
    module_name_to_lib_file_name("lib", ModuleName,
        ".$(EXT_FOR_SHARED_LIB)", yes, SharedLibFileName, !IO),
    module_name_to_lib_file_name("lib", ModuleName,
        ".$(EXT_FOR_SHARED_LIB)", no, MaybeSharedLibFileName, !IO),
    module_name_to_file_name(ModuleName, ".jar", no, JarFileName, !IO),

    %
    % Set up the installed name for shared libraries.
    %
    globals.io_lookup_bool_option(shlib_linker_use_install_name,
        UseInstallName, !IO),
    (
        UseInstallName = yes,
        get_install_name_option(SharedLibFileName, InstallNameOpt, !IO)
    ;
        UseInstallName = no,
        InstallNameOpt = ""
    ),

    AllInts = [
        "$(", MakeVarName, ".ints) ",
        "$(", MakeVarName, ".int3s) ",
        MaybeOptsVar, MaybeTransOptsVar,
        InitFileName, "\n\n"
    ],
    ILLibRule = [
        LibTargetName, " : ", "$(", MakeVarName, ".dlls) ",
            "$(", MakeVarName, ".foreign_dlls) \\\n\t\t"
        | AllInts
    ],
    JavaLibRule = [
        LibTargetName, " : ", JarFileName, " \\\n\t\t"
        | AllInts
    ],
    LibRule = [
        LibTargetName, " : ", LibFileName, " ",
        MaybeSharedLibFileName, " \\\n\t\t"
        | AllInts
    ],
    (
        Gmake = yes,
        LibRules = IfIL ++ ILLibRule ++ Else ++
            IfJava ++ JavaLibRule ++ Else2 ++
            LibRule ++ EndIf2 ++ EndIf
    ;
        Gmake = no,
        (
            Target = target_il,
            LibRules = ILLibRule
        ;
            Target = target_java,
            LibRules = JavaLibRule
        ;
            ( Target = target_c
            ; Target = target_asm
            ),
            LibRules = LibRule
        )
    ),
    io.write_strings(DepStream, [
        ".PHONY : ", LibTargetName, "\n" |
        LibRules
    ], !IO),

    io.write_strings(DepStream, [
        "ifneq ($(EXT_FOR_SHARED_LIB),$A)\n",
        SharedLibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".pic_os) ",
            All_MLPicObjsString, " ", All_MLLibsDepString, "\n",
        "\t$(ML) --make-shared-lib $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) ",
            "-- ", InstallNameOpt, " $(ALL_LD_LIBFLAGS) -o ",
            SharedLibFileName, " \\\n",
        "\t\t$(", MakeVarName, ".pic_os) ", All_MLPicObjsString,
            " $(ALL_MLLIBS)\n",
        "endif\n\n"
    ], !IO),

    io.write_strings(DepStream, [
        LibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
        "\trm -f ", LibFileName, "\n",
        "\t$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)", LibFileName, " ",
            "$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
        "\t$(RANLIB) $(ALL_RANLIBFLAGS) ", LibFileName, "\n\n"
    ], !IO),

    ClassFiles = "$(" ++ MakeVarName ++ ".classes)",
    list_class_files_for_jar(ModuleName, ClassFiles, ListClassFiles, !IO),
    io.write_strings(DepStream, [
        JarFileName, " : ", "$(", MakeVarName, ".classes)\n",
        "\t$(JAR) $(JAR_CREATE_FLAGS) ", JarFileName, " ",
        ListClassFiles, "\n\n"
    ], !IO),

    module_name_to_file_name(ModuleName, ".dep", no, DepFileName, !IO),
    module_name_to_file_name(ModuleName, ".dv", no, DvFileName, !IO),

    io.write_strings(DepStream, [
        InitFileName, " : ", DepFileName, " $(", MakeVarName, ".cs)\n",
        "\techo > ", InitFileName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        "\t$(MKLIBINIT) ", "$(", MakeVarName, ".cs)", " >> ",
        InitFileName, "\n"
    ], !IO),

    % $(EXTRA_INIT_COMMAND) should expand to a command to
    % generate extra entries in the `.init' file for a library.
    % It may expand to the empty string.
    io.write_string(DepStream, "\t$(EXTRA_INIT_COMMAND) >> ", !IO),
    io.write_string(DepStream, InitFileName, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The `force-module_init' dependency forces the commands for
    % the `module_init.c' rule to be run every time the rule
    % is considered.
    ModuleFileName = sym_name_to_string(ModuleName),
    ForceC2InitTarget = "force-" ++ ModuleFileName ++ "_init",
    TmpInitCFileName = InitCFileName ++ ".tmp",
    io.write_strings(DepStream, [
        ForceC2InitTarget, " :\n\n",
        InitCFileName, " : ", ForceC2InitTarget, " $(", MakeVarName, ".cs)\n",
        "\t@$(C2INIT) $(ALL_GRADEFLAGS) $(ALL_C2INITFLAGS) ",
            "--init-c-file ", TmpInitCFileName,
            " $(", MakeVarName, ".init_cs) $(ALL_C2INITARGS)\n",
        "\t@mercury_update_interface ", InitCFileName, "\n\n"
    ], !IO),

    module_name_to_lib_file_name("lib", ModuleName, ".install_ints", no,
        LibInstallIntsTargetName, !IO),
    (
        Intermod = yes,
        OptStr = " opt"
    ;
        Intermod = no,
        OptStr = ""
    ),
    (
        Intermod = yes,
        map.member(DepsMap, _, deps(_, Imports)),
        Imports ^ children = [_ | _]
    ->
        % The `.int0' files only need to be installed with
        % `--intermodule-optimization'.
        Int0Str = " int0",
        MaybeInt0sVar = "$(" ++ MakeVarName ++ ".int0s) "
    ;
        Int0Str = "",
        MaybeInt0sVar = ""
    ),
    (
        TransOpt = yes,
        TransOptStr = " trans_opt"
    ;
        TransOpt = no,
        TransOptStr = ""
    ),
    (
        MmcMakeDeps = yes,
        DepStr = " module_dep"
    ;
        MmcMakeDeps = no,
        DepStr = ""
    ),

    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallIntsTargetName, "\n",
        LibInstallIntsTargetName, " : $(", MakeVarName, ".ints) $(",
            MakeVarName, ".int3s) ", MaybeInt0sVar, MaybeOptsVar,
            MaybeTransOptsVar, MaybeModuleDepsVar,
            " install_lib_dirs\n",
        "\tfiles=""$(", MakeVarName, ".ints) $(", MakeVarName,
            ".int3s) ", MaybeInt0sVar, MaybeOptsVar,
            MaybeTransOptsVar, MaybeModuleDepsVar, """; \\\n",
        "\tfor file in $$files; do \\\n",
        "\t\ttarget=""$(INSTALL_INT_DIR)/`basename $$file`""; \\\n",
        "\t\tif cmp -s ""$$file"" ""$$target""; then \\\n",
        "\t\t\techo \"$$target unchanged\"; \\\n",
        "\t\telse \\\n",
        "\t\t\techo \"installing $$target\"; \\\n",
        "\t\t\t$(INSTALL) ""$$file"" ""$$target""; \\\n",
        "\t\tfi; \\\n",
        "\tdone\n",
        "\t# The following is needed to support the `--use-subdirs' option\n",
        "\t# We try using `$(LN_S)', but if that fails, then we just use\n",
        "\t# `$(INSTALL)'.\n",
        "\tfor ext in int int2 int3",
        Int0Str, OptStr, TransOptStr, DepStr,
        "; do \\\n",
        "\t\tdir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\\n",
        "\t\trm -rf ""$$dir""; \\\n",
        "\t\t$(LN_S) .. ""$$dir"" || { \\\n",
        "\t\t\t{ [ -d ""$$dir"" ] || \\\n",
        "\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\\n",
        "\t\t\t$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext ""$$dir""; \\\n",
        "\t\t} || exit 1; \\\n",
        "\tdone\n\n"
    ], !IO),

    %
    % XXX  Note that we install the `.opt' and `.trans_opt' files
    % in two places: in the `lib/$(GRADE)/opts' directory, so
    % that mmc will find them, and also in the `ints' directory,
    % so that Mmake will find them.  That's not ideal, but it works.
    %
    module_name_to_lib_file_name("lib", ModuleName,
        ".install_opts", no, LibInstallOptsTargetName, !IO),
    io.write_strings(DepStream,
        [".PHONY : ", LibInstallOptsTargetName, "\n",
        LibInstallOptsTargetName, " : "], !IO),
    (
        Intermod = no,
        TransOpt = no
    ->
        io.write_string(DepStream, "\n\t@:\n\n", !IO)
    ;
        io.write_strings(DepStream, [
        MaybeOptsVar, MaybeTransOptsVar, "install_grade_dirs\n",
        "\tfiles=""", MaybeOptsVar, MaybeTransOptsVar, """; \\\n",
        "\tfor file in $$files; do \\\n",
        "\t\ttarget=""$(INSTALL_GRADE_INT_DIR)/`basename $$file`"";\\\n",
        "\t\tif cmp -s ""$$file"" ""$$target""; then \\\n",
        "\t\t\techo \"$$target unchanged\"; \\\n",
        "\t\telse \\\n",
        "\t\t\techo \"installing $$target\"; \\\n",
        "\t\t\t$(INSTALL) ""$$file"" ""$$target""; \\\n",
        "\t\tfi; \\\n",
        "\tdone\n",
        "\t# The following is needed to support the `--use-subdirs' option\n",
        "\t# We try using `$(LN_S)', but if that fails, then we just use\n",
        "\t# `$(INSTALL)'.\n",
        "\tfor ext in ",
        OptStr, TransOptStr,
        "; do \\\n",
        "\t\tdir=""$(INSTALL_GRADE_INT_DIR)/Mercury/$${ext}s""; \\\n",
        "\t\trm -rf ""$$dir""; \\\n",
        "\t\t$(LN_S) .. ""$$dir"" || { \\\n",
        "\t\t\t{ [ -d ""$$dir"" ] || \\\n",
        "\t\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\\n",
        "\t\t\t$(INSTALL) ""$(INSTALL_GRADE_INT_DIR)""/*.$$ext \\\n",
        "\t\t\t\t""$$dir""; \\\n",
        "\t\t} || exit 1; \\\n",
        "\tdone\n\n"
        ], !IO)
    ),

    %
    % XXX  Note that we install the header files in two places:
    % in the `lib/inc' or `lib/$(GRADE)/$(FULLARCH)/inc' directory,
    % so that the C compiler will find them, and also in the `ints'
    % directory, so that Mmake will find them.  That's not ideal,
    % but it works.
    %
    % (A better fix would be to change the VPATH setting
    % in scripts/Mmake.vars.in so that Mmake also searches
    % the `lib/$(GRADE)/$(FULLARCH)/inc' directory, but doing
    % that properly is non-trivial.)
    %
    module_name_to_lib_file_name("lib", ModuleName,
        ".install_hdrs", no, LibInstallHdrsTargetName, !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallHdrsTargetName, "\n",
        LibInstallHdrsTargetName, " : ",
            "$(", MakeVarName, ".mhs) ",
            "install_lib_dirs\n",
        "ifeq ($(", MakeVarName, ".mhs),)\n",
        "\t@:\n",
        "else\n",
        "\tfor hdr in $(", MakeVarName, ".mhs); do \\\n",
        "\t\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\\n",
        "\t\t$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\\n",
        "\tdone\n",
        "endif\n\n"], !IO),

    module_name_to_lib_file_name("lib", ModuleName,
        ".install_grade_hdrs", no, LibInstallGradeHdrsTargetName, !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallGradeHdrsTargetName, "\n",
        LibInstallGradeHdrsTargetName, " : ",
            "$(", MakeVarName, ".mihs) ",
            "install_grade_dirs\n",
    "ifeq ($(", MakeVarName, ".mihs),)\n",
    "\t@:\n",
    "else\n",
    "\tfor hdr in $(", MakeVarName, ".mihs); do \\\n",
    "\t\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\\n",
    "\t\t$(INSTALL) $$hdr $(INSTALL_GRADE_INC_DIR); \\\n",
    "\tdone\n",
    "\t# The following is needed to support the `--use-subdirs' option\n",
    "\t# We try using `$(LN_S)', but if that fails, then we just use\n",
    "\t# `$(INSTALL)'.\n",
    "\trm -rf $(INSTALL_GRADE_INC_SUBDIR)\n",
    "\t$(LN_S) .. $(INSTALL_GRADE_INC_SUBDIR) || { \\\n",
    "\t\t{ [ -d $(INSTALL_GRADE_INC_SUBDIR) ] || \\\n",
    "\t\t\t$(INSTALL_MKDIR) $(INSTALL_GRADE_INC_SUBDIR); \\\n",
    "\t\t} && \\\n",
    "\t\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\\n",
    "\t\t\t$(INSTALL_GRADE_INC_SUBDIR); \\\n",
    "\t} || exit 1\n",
    "\trm -rf $(INSTALL_INT_DIR)/Mercury/mihs\n",
    "\t$(LN_S) .. $(INSTALL_INT_DIR)/Mercury/mihs || { \\\n",
    "\t\t{ [ -d $(INSTALL_INT_DIR)/Mercury/mihs ] || \\\n",
    "\t\t\t$(INSTALL_MKDIR) \\\n",
    "\t\t\t$(INSTALL_INT_DIR)/Mercury/mihs; \\\n",
    "\t\t} && \\\n",
    "\t\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\\n",
    "\t\t\t$(INSTALL_INT_DIR); \\\n",
    "\t} || exit 1\n",
    "endif\n\n"], !IO),

    module_name_to_file_name(ModuleName, ".check", no, CheckTargetName, !IO),
    module_name_to_file_name(ModuleName, ".ints", no, IntsTargetName, !IO),
    module_name_to_file_name(ModuleName, ".int3s", no, Int3sTargetName, !IO),
    module_name_to_file_name(ModuleName, ".opts", no, OptsTargetName, !IO),
    module_name_to_file_name(ModuleName, ".trans_opts", no,
        TransOptsTargetName, !IO),
    module_name_to_file_name(ModuleName, ".ss", no, SsTargetName, !IO),
    module_name_to_file_name(ModuleName, ".pic_ss", no, PicSsTargetName, !IO),
    module_name_to_file_name(ModuleName, ".ils", no, ILsTargetName, !IO),
    module_name_to_file_name(ModuleName, ".javas", no, JavasTargetName, !IO),
    module_name_to_file_name(ModuleName, ".classes", no, ClassesTargetName,
        !IO),

    % We need to explicitly mention
    % $(foo.pic_ss) somewhere in the Mmakefile, otherwise it
    % won't build properly with --target asm: GNU Make's pattern rule
    % algorithm will try to use the .m -> .c_date -> .c -> .pic_o rule chain
    % rather than the .m -> .pic_s_date -> .pic_s -> .pic_o chain.
    % So don't remove the pic_ss target here.

    io.write_strings(DepStream, [
        ".PHONY : ", CheckTargetName, "\n",
        CheckTargetName, " : $(", MakeVarName, ".errs)\n\n",
        ".PHONY : ", IntsTargetName, "\n",
        IntsTargetName, " : $(", MakeVarName, ".dates)\n\n",
        ".PHONY : ", Int3sTargetName, "\n",
        Int3sTargetName, " : $(", MakeVarName, ".date3s)\n\n",
        ".PHONY : ", OptsTargetName, "\n",
        OptsTargetName, " : $(", MakeVarName, ".optdates)\n\n",
        ".PHONY : ", TransOptsTargetName, "\n",
        TransOptsTargetName, " : $(", MakeVarName,
            ".trans_opt_dates)\n\n",
        ".PHONY : ", SsTargetName, "\n",
        SsTargetName, " : $(", MakeVarName, ".ss)\n\n",
        ".PHONY : ", PicSsTargetName, "\n",
        PicSsTargetName, " : $(", MakeVarName, ".pic_ss)\n\n",
        ".PHONY : ", ILsTargetName, "\n",
        ILsTargetName, " : $(", MakeVarName, ".ils)\n\n",
        ".PHONY : ", JavasTargetName, "\n",
        JavasTargetName, " : $(", MakeVarName, ".javas)\n\n",
        ".PHONY : ", ClassesTargetName, "\n",
        ClassesTargetName, " : $(", MakeVarName, ".classes)\n\n"
    ], !IO),

    %
    % If you change the clean targets below, please also update the
    % documentation in doc/user_guide.texi.
    %
    % XXX The use of xargs in the clean targets doesn't handle
    % special characters in the file names correctly.  This is
    % currently not a problem in practice as we never generate
    % names containing special characters, any fix for this problem
    % will also require a fix in `mmake.in'.
    %

    module_name_to_file_name(ModuleName, ".clean", no, CleanTargetName, !IO),
    io.write_strings(DepStream, [
        "clean_local : ", CleanTargetName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", CleanTargetName, "\n",
        CleanTargetName, " :\n",
        "\t-echo $(", MakeVarName, ".dirs) | xargs rm -rf \n",
        "\t-echo $(", MakeVarName, ".cs) ", InitCFileName,
            " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".mihs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_ss) ", InitAsmFileName,
            " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_pic_ss) ",
            InitAsmFileName, " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_os) ", InitObjFileName,
            " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_pic_os) ",
            InitPicObjFileName, " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".c_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".il_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".java_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_s_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_pic_s_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".useds) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".ils) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".javas) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".profs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".errs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".foreign_cs) | xargs rm -f\n"
    ], !IO),

    io.write_string(DepStream, "\n", !IO),

    module_name_to_file_name(ModuleName, ".realclean", no,
        RealCleanTargetName, !IO),
    io.write_strings(DepStream, [
        "realclean_local : ", RealCleanTargetName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", RealCleanTargetName, "\n",
        RealCleanTargetName, " : ", CleanTargetName, "\n",
        "\t-echo $(", MakeVarName, ".dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".date0s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".date3s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".optdates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".trans_opt_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".ints) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".int0s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".int3s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".opts) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".trans_opts) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".analysiss) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".requests) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".imdgs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".ds) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".module_deps) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_mhs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_mihs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".dlls) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".foreign_dlls) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".classes) | xargs rm -f\n"
    ], !IO),
    io.write_strings(DepStream, [
        "\t-rm -f ",
            ExeFileName, "$(EXT_FOR_EXE) ",
            InitFileName, " ",
            LibFileName, " ",
            SharedLibFileName, " ",
            JarFileName, " ",
            DepFileName, " ",
            DvFileName, "\n\n"
    ], !IO).

:- pred get_source_file(deps_map::in, module_name::in, file_name::out) is det.

get_source_file(DepsMap, ModuleName, FileName) :-
    map.lookup(DepsMap, ModuleName, Deps),
    Deps = deps(_, ModuleImports),
    module_imports_get_source_file_name(ModuleImports, SourceFileName),
    ( string.remove_suffix(SourceFileName, ".m", SourceFileBase) ->
        FileName = SourceFileBase
    ;
        unexpected(this_file, "source file name doesn't end in `.m'")
    ).

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

    % Find out which modules will generate as external foreign
    % language files.
    % We return the module names and file extensions.
    %
:- func foreign_modules(list(module_name), deps_map)
    = assoc_list(module_name, string).

foreign_modules(Modules, DepsMap) = ForeignModules :-
    P = (pred(M::in, FMs::out) is semidet :-
        module_has_foreign(DepsMap, M, LangList),
        FMs = list.filter_map((func(L) = (NewM - Ext) is semidet :-
            NewM = foreign_language_module_name(M, L),
            Ext = foreign_language_file_extension(L)
        ), LangList)
    ),
    list.filter_map(P, Modules, ForeignModulesList),
    ForeignModules = list.condense(ForeignModulesList).

    % Succeed iff we need to generate a C header file for the specified
    % module, assuming we're compiling with `--target asm'.
    %
:- pred module_needs_header(deps_map::in, module_name::in) is semidet.

module_needs_header(DepsMap, Module) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    ModuleImports ^ has_foreign_code = contains_foreign_code(Langs),
    set.member(lang_c, Langs).

    % Succeed iff we need to generate a foreign language output file
    % for the specified module.
    %
:- pred module_has_foreign(deps_map::in, module_name::in,
    list(foreign_language)::out) is semidet.

module_has_foreign(DepsMap, Module, LangList) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    ModuleImports ^ has_foreign_code = contains_foreign_code(Langs),
    LangList = set.to_sorted_list(Langs).

    % get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs):
    %
    % Find any extra .$O files that should be linked into the executable.
    % These include fact table object files and object files for foreign
    % code that can't be generated inline for this target.
    %
:- pred get_extra_link_objects(list(module_name)::in, deps_map::in,
    compilation_target::in, assoc_list(file_name, module_name)::out)
    is det.

get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs) :-
    get_extra_link_objects_2(Modules, DepsMap, Target, [], ExtraLinkObjs0),
    list.reverse(ExtraLinkObjs0, ExtraLinkObjs).

:- pred get_extra_link_objects_2(list(module_name)::in, deps_map::in,
    compilation_target::in,
    assoc_list(file_name, module_name)::in,
    assoc_list(file_name, module_name)::out) is det.

get_extra_link_objects_2([], _DepsMap, _Target, !ExtraLinkObjs).
get_extra_link_objects_2([Module | Modules], DepsMap, Target,
        !ExtraLinkObjs) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    %
    % Handle object files for fact tables
    %
    FactDeps = ModuleImports ^ fact_table_deps,
    list.length(FactDeps, NumFactDeps),
    list.duplicate(NumFactDeps, Module, ModuleList),
    assoc_list.from_corresponding_lists(FactDeps, ModuleList, FactTableObjs),
    %
    % Handle object files for foreign code.
    % XXX currently we only support `C' foreign code.
    %
    (
        Target = target_asm,
        ModuleImports ^ has_foreign_code = contains_foreign_code(Langs),
        set.member(lang_c, Langs)
    ->
        FileName = sym_name_to_string(Module),
        NewLinkObjs = [(FileName ++ "__c_code") - Module | FactTableObjs]
    ;
        NewLinkObjs = FactTableObjs
    ),
    list.append(NewLinkObjs, !ExtraLinkObjs),
    get_extra_link_objects_2(Modules, DepsMap, Target, !ExtraLinkObjs).

:- type module_foreign_info --->
    module_foreign_info(
        used_foreign_languages          :: set(foreign_language),
        foreign_proc_languages          :: map(sym_name, foreign_language),
        all_foreign_import_module_info  :: foreign_import_module_info_list,
        module_contains_foreign_export  :: contains_foreign_export
    ).

:- pred get_item_list_foreign_code(globals::in, item_list::in,
    set(foreign_language)::out, foreign_import_module_info_list::out,
    contains_foreign_export::out) is det.

get_item_list_foreign_code(Globals, Items, LangSet, ForeignImports,
        ContainsPragmaExport) :-
    Info0 = module_foreign_info(set.init, map.init, [], no_foreign_export),
    list.foldl(get_item_foreign_code(Globals), Items, Info0, Info),
    Info = module_foreign_info(LangSet0, LangMap, ForeignImports,
        ContainsPragmaExport),
    ForeignProcLangs = map.values(LangMap),
    LangSet = set.insert_list(LangSet0, ForeignProcLangs).

:- pred get_item_foreign_code(globals::in, item_and_context::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_item_foreign_code(Globals, item_and_context(Item, Context), !Info) :-
    ( Item = item_pragma(_, Pragma) ->
        do_get_item_foreign_code(Globals, Pragma, Context, !Info)
    ; Item = item_mutable(_, _, _, _, _, _) ->
        % Mutables introduce foreign_procs, but mutable declarations
        % won't have been expanded by the time we get here so we need
        % to handle them separately.
        % XXX mutables are currently only implemented for the C backends
        % but we should handle the Java/IL backends here as well.
        % (See do_get_item_foreign_code for details/5).
        !:Info = !.Info ^ used_foreign_languages :=
            set.insert(!.Info ^ used_foreign_languages, lang_c)
    ; ( Item = item_initialise(_, _, _) ; Item = item_finalise(_, _, _) ) ->
        % Intialise/finalise declarations introduce export pragmas, but
        % again they won't have been expanded by the time we get here.
        % XXX we don't currently support these on non-C backends.
        !:Info = !.Info ^ used_foreign_languages :=
            set.insert(!.Info ^ used_foreign_languages, lang_c),
        !:Info = !.Info ^ module_contains_foreign_export :=
            contains_foreign_export
    ;
        true
    ).

:- pred do_get_item_foreign_code(globals::in, pragma_type::in,
    prog_context::in, module_foreign_info::in, module_foreign_info::out)
    is det.

do_get_item_foreign_code(Globals, Pragma, Context, Info0, Info) :-
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    globals.get_target(Globals, Target),

    % The code here should match the way that mlds_to_gcc.m decides whether
    % or not to call mlds_to_c.m.  XXX Note that we do NOT count foreign_decls
    % here. We only link in a foreign object file if mlds_to_gcc called
    % mlds_to_c.m to generate it, which it will only do if there is some
    % foreign_code, not just foreign_decls. Counting foreign_decls here
    % causes problems with intermodule optimization.
    (
        Pragma = pragma_foreign_code(Lang, _),
        list.member(Lang, BackendLangs)
    ->
        Info = Info0 ^ used_foreign_languages :=
            set.insert(Info0 ^ used_foreign_languages, Lang)
    ;
        Pragma = pragma_foreign_proc(Attrs, Name, _, _, _, _, _)
    ->
        NewLang = get_foreign_language(Attrs),
        ( OldLang = Info0 ^ foreign_proc_languages ^ elem(Name) ->
            % is it better than an existing one?
            (
                yes = prefer_foreign_language(Globals, Target,
                    OldLang, NewLang)
            ->
                Info = Info0 ^ foreign_proc_languages ^ elem(Name) := NewLang
            ;
                Info = Info0
            )
        ;
            % is it one of the languages we support?
            ( list.member(NewLang, BackendLangs) ->
                Info = Info0 ^ foreign_proc_languages ^ elem(Name) := NewLang
            ;
                Info = Info0
            )
        )
    ;
        % XXX `pragma export' should not be treated as foreign, but currently
        % mlds_to_gcc.m doesn't handle that declaration, and instead just
        % punts it on to mlds_to_c.m, thus generating C code for it,
        % rather than assembler code. So we need to treat `pragma export'
        % like the other pragmas for foreign code.
        Pragma = pragma_foreign_export(Lang, _, _, _, _),
        list.member(Lang, BackendLangs)
    ->
        Info1 = Info0 ^ used_foreign_languages :=
            set.insert(Info0 ^ used_foreign_languages, Lang),
        Info = Info1 ^ module_contains_foreign_export :=
            contains_foreign_export
    ;
        Pragma = pragma_foreign_import_module(Lang, Import),
        list.member(Lang, BackendLangs)
    ->
        Info = Info0 ^ all_foreign_import_module_info :=
            [foreign_import_module_info(Lang, Import, Context) |
                Info0 ^ all_foreign_import_module_info]
    ;
        % We generate some C code for fact tables,
        % so we need to treat modules containing
        % fact tables as if they contain foreign
        % code.
        ( Target = target_asm
        ; Target = target_c
        ),
        Pragma = pragma_fact_table(_, _, _)
    ->
        Info = Info0 ^ used_foreign_languages :=
            set.insert(Info0 ^ used_foreign_languages, lang_c)
    ;
        Info = Info0
    ).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(module_name)::in, deps_map::in,
    list(module_name)::out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    module_imports_get_error(ModuleImports, Error),
    ( Error = fatal_module_errors ->
        Modules = Modules1
    ;
        Modules = [Module | Modules1]
    ),
    select_ok_modules(Modules0, DepsMap, Modules1).

%-----------------------------------------------------------------------------%

:- pred write_dependencies_list(list(module_name)::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_dependencies_list([], _, _, !IO).
write_dependencies_list([Module | Modules], Suffix, DepStream, !IO) :-
    module_name_to_file_name(Module, Suffix, no, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_dependencies_list(Modules, Suffix, DepStream, !IO).

referenced_dlls(Module, DepModules0) = Modules :-
    DepModules = [Module | DepModules0],

        % If we are not compiling a module in the mercury
        % std library then replace all the std library dlls with
        % one reference to mercury.dll.
    (
        mercury_std_library_module_name(Module)
    ->
            % In the standard library we need to add the
            % runtime dlls.
        Modules = list.remove_dups(
            [unqualified("mercury_dotnet"), unqualified("mercury_il")
                | DepModules])
    ;
        F = (func(M) =
            (
                mercury_std_library_module_name(M)
            ->
                unqualified("mercury")
            ;
                    % A sub module is located in the top level assembly.
                unqualified(outermost_qualifier(M))
            )
        ),
        Modules = list.remove_dups(list.map(F, DepModules))
    ).

    % submodules(Module, Imports):
    %
    % Returns the list of submodules from Imports which are sub-modules of
    % Module, if Module is a top level module and not in the std library.
    % Otherwise it returns the empty list.
    %
:- func submodules(module_name, list(module_name)) = list(module_name).

submodules(Module, Modules0) = Modules :-
    (
        Module = unqualified(Str),
        \+ mercury_std_library_module_name(Module)
    ->
        P = (pred(M::in) is semidet :-
            Str = outermost_qualifier(M),
            M \= Module
        ),
        list.filter(P, Modules0, Modules)
    ;
        Modules = []
    ).

:- pred write_dll_dependencies_list(list(module_name)::in,
    string::in, io.output_stream::in, io::di, io::uo) is det.

write_dll_dependencies_list(Modules, Prefix, DepStream, !IO) :-
    list.foldl(write_dll_dependency(DepStream, Prefix), Modules, !IO).

:- pred write_dll_dependency(io.output_stream::in, string::in,
    module_name::in, io::di, io::uo) is det.

write_dll_dependency(DepStream, Prefix, Module, !IO) :-
    module_name_to_file_name(Module, ".dll", no, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, Prefix, !IO),
    io.write_string(DepStream, FileName, !IO).

:- pred write_fact_table_dependencies_list(module_name::in,
    list(file_name)::in, string::in, io.output_stream::in,
    io::di, io::uo) is det.

write_fact_table_dependencies_list(_, [], _, _, !IO).
write_fact_table_dependencies_list(Module, [FactTable | FactTables], Suffix,
        DepStream, !IO) :-
    fact_table_file_name(Module, FactTable, Suffix, no, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_fact_table_dependencies_list(Module, FactTables, Suffix,
        DepStream, !IO).

:- pred write_extra_link_dependencies_list(
    assoc_list(file_name, module_name)::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_extra_link_dependencies_list([], _, _, !IO).
write_extra_link_dependencies_list([ExtraLink - Module | ExtraLinks], Suffix,
        DepStream, !IO) :-
    extra_link_obj_file_name(Module, ExtraLink, Suffix, no, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_extra_link_dependencies_list(ExtraLinks, Suffix, DepStream, !IO).

:- pred write_file_dependencies_list(list(string)::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_file_dependencies_list([], _, _, !IO).
write_file_dependencies_list([FileName | FileNames], Suffix, DepStream, !IO) :-
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    io.write_string(DepStream, Suffix, !IO),
    write_file_dependencies_list(FileNames, Suffix, DepStream, !IO).

%-----------------------------------------------------------------------------%

:- pred write_compact_dependencies_list(list(module_name)::in, string::in,
    string::in, maybe(pair(string))::in, io.output_stream::in,
    io::di, io::uo) is det.

write_compact_dependencies_list(Modules, Prefix, Suffix, Basis, DepStream,
        !IO) :-
    (
        Basis = yes(VarName - OldSuffix),
        io.write_string(DepStream, "$(", !IO),
        io.write_string(DepStream, VarName, !IO),
        io.write_string(DepStream, ":%", !IO),
        io.write_string(DepStream, OldSuffix, !IO),
        io.write_string(DepStream, "=", !IO),
        io.write_string(DepStream, Prefix, !IO),
        io.write_string(DepStream, "%", !IO),
        io.write_string(DepStream, Suffix, !IO),
        io.write_string(DepStream, ")", !IO)
    ;
        Basis = no,
        write_dependencies_list(Modules, Suffix, DepStream, !IO)
    ).

:- pred write_compact_dependencies_separator(maybe(pair(string))::in,
    io.output_stream::in, io::di, io::uo) is det.

write_compact_dependencies_separator(no, _DepStream, !IO).
write_compact_dependencies_separator(yes(_), DepStream, !IO) :-
    io.write_string(DepStream, " ", !IO).

%-----------------------------------------------------------------------------%

    % Look up a module in the dependency map.
    % If we don't know its dependencies, read the module and
    % save the dependencies in the dependency map.
    %
:- pred lookup_dependencies(module_name::in, bool::in, bool::out,
    deps_map::in, deps_map::out, module_imports::out, io::di, io::uo) is det.

lookup_dependencies(Module, Search, Done, !DepsMap, ModuleImports, !IO) :-
    ( map.search(!.DepsMap, Module, deps(DonePrime, ModuleImportsPrime)) ->
        Done = DonePrime,
        ModuleImports = ModuleImportsPrime
    ;
        read_dependencies(Module, Search, ModuleImportsList, !IO),
        list.foldl(insert_into_deps_map, ModuleImportsList, !DepsMap),
        map.lookup(!.DepsMap, Module, deps(Done, ModuleImports))
    ).

    % insert_into_deps_map/3:
    %
    % Insert a new entry into the deps_map. If the module already occurred
    % in the deps_map, then we just replace the old entry (presumed to be
    % a dummy entry) with the new one.
    %
    % This can only occur for sub-modules which have been imported before
    % their parent module was imported: before reading a module and
    % inserting it into the deps map, we check if it was already there,
    % but when we read in the module, we try to insert not just that module
    % but also all the nested sub-modules inside that module. If a sub-module
    % was previously imported, then it may already have an entry in the
    % deps_map. However, unless the sub-module is defined both as a separate
    % sub-module and also as a nested sub-module, the previous entry will be
    % a dummy entry that we inserted after trying to read the source file
    % and failing.
    %
    % Note that the case where a module is defined as both a separate
    % sub-module and also as a nested sub-module is caught in
    % split_into_submodules.
    %
:- pred insert_into_deps_map(module_imports::in, deps_map::in, deps_map::out)
    is det.

insert_into_deps_map(ModuleImports, DepsMap0, DepsMap) :-
    module_imports_get_module_name(ModuleImports, ModuleName),
    map.set(DepsMap0, ModuleName, deps(no, ModuleImports), DepsMap).

    % Read a module to determine the (direct) dependencies of that module
    % and any nested sub-modules it contains.
    %
:- pred read_dependencies(module_name::in, bool::in, list(module_imports)::out,
    io::di, io::uo) is det.

read_dependencies(ModuleName, Search, ModuleImportsList, !IO) :-
    read_mod_ignore_errors(ModuleName, ".m",
        "Getting dependencies for module", Search, no, ItemAndContexts0, Error,
        FileName0, _, !IO),
    globals.io_get_globals(Globals, !IO),
    (
        ItemAndContexts0 = [],
        Error = fatal_module_errors
    ->
        read_mod_ignore_errors(ModuleName, ".int",
            "Getting dependencies for module interface", Search,
            no, ItemAndContexts, _Error, FileName, _, !IO),
        SubModuleList = [ModuleName - ItemAndContexts]
    ;
        FileName = FileName0,
        ItemAndContexts = ItemAndContexts0,
        split_into_submodules(ModuleName, ItemAndContexts, SubModuleList,
            [], Specs),
        sort_error_specs(Specs, SortedSpecs),
        write_error_specs(SortedSpecs, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO)
    ),
    assoc_list.keys(SubModuleList, SubModuleNames),
    list.map(init_dependencies(FileName, ModuleName, SubModuleNames,
        Error, Globals), SubModuleList, ModuleImportsList).

init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
        Error, Globals, ModuleName - ItemAndContexts, ModuleImports) :-
    ParentDeps = get_ancestors(ModuleName),

    get_dependencies(ItemAndContexts, ImplImportDeps0, ImplUseDeps0),
    add_implicit_imports(ItemAndContexts, Globals,
        ImplImportDeps0, ImplImportDeps,
        ImplUseDeps0, ImplUseDeps),
    list.append(ImplImportDeps, ImplUseDeps, ImplementationDeps),

    get_interface(ModuleName, no, ItemAndContexts, InterfaceItemAndContexts),
    get_dependencies(InterfaceItemAndContexts,
        InterfaceImportDeps0, InterfaceUseDeps0),
    add_implicit_imports(InterfaceItemAndContexts, Globals,
        InterfaceImportDeps0, InterfaceImportDeps,
        InterfaceUseDeps0, InterfaceUseDeps),
    list.append(InterfaceImportDeps, InterfaceUseDeps, InterfaceDeps),

    % We don't fill in the indirect dependencies yet.
    IndirectDeps = [],

    get_children(ItemAndContexts, IncludeDeps),
    get_children(InterfaceItemAndContexts, InterfaceIncludeDeps),

    ( ModuleName = SourceFileModuleName ->
        list.delete_all(NestedModuleNames, ModuleName, NestedDeps)
    ;
        NestedDeps = []
    ),

    get_fact_table_dependencies(ItemAndContexts, FactTableDeps),

    % Figure out whether the items contain foreign code.
    get_item_list_foreign_code(Globals, ItemAndContexts, LangSet,
        ForeignImports0, ContainsPragmaExport),
    ( set.empty(LangSet) ->
        ContainsForeignCode = no_foreign_code
    ;
        ContainsForeignCode = contains_foreign_code(LangSet)
    ),

    % If this module contains `:- pragma export' or
    % `:- pragma foreign_type' declarations, importing modules
    % may need to import its `.mh' file.
    get_foreign_self_imports(ItemAndContexts, SelfImportLangs),
    ForeignSelfImports = list.map(
        (func(Lang) = foreign_import_module_info(Lang, ModuleName,
            term.context_init)),
        SelfImportLangs),
    ForeignImports = ForeignSelfImports ++ ForeignImports0,

    % Work out whether the items contain main/2.
    (
        list.member(ItemAndContext, ItemAndContexts),
        ItemAndContext = item_and_context(Item, _),
        Item = item_pred_or_func(_, _, _, _, predicate, Name, [_, _],
            WithType, _, _, _, _, _),
        unqualify_name(Name) = "main",

        % XXX We should allow `main/2' to be declared using
        % `with_type`, but equivalences haven't been expanded
        % at this point. The `has_main' field is only used for
        % some special case handling of the module containing
        % main for the IL backend (we generate a `.exe' file
        % rather than a `.dll' file). This would arguably be
        % better done by generating a `.dll' file as normal,
        % and a separate `.exe' file containing initialization
        % code and a call to `main/2', as we do with the `_init.c'
        % file in the C backend.
        WithType = no
    ->
        HasMain = has_main
    ;
        HasMain = no_main
    ),

    ModuleImports = module_imports(FileName, SourceFileModuleName,
        ModuleName, ParentDeps, InterfaceDeps,
        ImplementationDeps, IndirectDeps, IncludeDeps,
        InterfaceIncludeDeps, NestedDeps, FactTableDeps,
        ContainsForeignCode, ForeignImports, ContainsPragmaExport,
        [], Error, no, HasMain, dir.this_directory).

%-----------------------------------------------------------------------------%

:- pred read_mod(read_modules::in, module_name::in, string::in, string::in,
    bool::in, bool::in, item_list::out, module_error::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

read_mod(ReadModules, ModuleName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, FileName, MaybeTimestamp, !IO) :-
    (
        find_read_module(ReadModules, ModuleName, Extension, ReturnTimestamp,
            Items0, MaybeTimestamp0, Error0, FileName0)
    ->
        Error = Error0,
        Items = Items0,
        MaybeTimestamp = MaybeTimestamp0,
        FileName = FileName0
    ;
        read_mod(ModuleName, Extension, Descr, Search, ReturnTimestamp,
            Items, Error, FileName, MaybeTimestamp, !IO)
    ).

read_mod(ModuleName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, FileName, MaybeTimestamp, !IO) :-
    read_mod_2(no, ModuleName, Extension, Descr, Search,
        no, ReturnTimestamp, Items, Error, FileName, MaybeTimestamp, !IO).

read_mod_if_changed(ModuleName, Extension, Descr, Search, OldTimestamp,
        Items, Error, FileName, MaybeTimestamp, !IO) :-
    read_mod_2(no, ModuleName, Extension, Descr, Search,
        yes(OldTimestamp), yes, Items, Error,
        FileName, MaybeTimestamp, !IO).

read_mod_ignore_errors(ModuleName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, FileName, MaybeTimestamp, !IO) :-
    read_mod_2(yes, ModuleName, Extension, Descr, Search,
        no, ReturnTimestamp, Items, Error, FileName, MaybeTimestamp, !IO).

:- pred read_mod_2(bool::in, module_name::in, string::in, string::in,
    bool::in, maybe(timestamp)::in, bool::in, item_list::out,
    module_error::out, file_name::out, maybe(timestamp)::out,
    io::di, io::uo) is det.

read_mod_2(IgnoreErrors, ModuleName, Extension, Descr, Search,
        MaybeOldTimestamp, ReturnTimestamp, Items, Error, FileName,
        MaybeTimestamp, !IO) :-
    (
        Search = yes,
        module_name_to_search_file_name(ModuleName, Extension, FileName0, !IO)
    ;
        Search = no,
        module_name_to_file_name(ModuleName, Extension, no, FileName0, !IO)
    ),
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(VeryVerbose, "% ", !IO),
    maybe_write_string(VeryVerbose, Descr, !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    maybe_write_string(VeryVerbose, FileName0, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),

    (
        Search = yes,
        globals.io_lookup_accumulating_option(search_directories, SearchDirs,
            !IO)
    ;
        Search = no,
        SearchDirs = [dir.this_directory]
    ),
    ( Extension = ".m" ->
        % For `.m' files we need to deal with the case where
        % the module name does not match the file name.
        OpenFile = search_for_module_source(SearchDirs, ModuleName)
    ;
        OpenFile = search_for_file(SearchDirs, FileName0)
    ),
    (
        MaybeOldTimestamp = yes(OldTimestamp),
        prog_io.read_module_if_changed(OpenFile, ModuleName,
            OldTimestamp, Error, MaybeFileName, ActualModuleName,
            Messages, Items, MaybeTimestamp0, !IO)
    ;
        MaybeOldTimestamp = no,
        prog_io.read_module(OpenFile, ModuleName,
            ReturnTimestamp, Error, MaybeFileName,
            ActualModuleName, Messages, Items, MaybeTimestamp0, !IO)
    ),

    (
        MaybeFileName = yes(FileName)
    ;
        MaybeFileName = no,
        FileName = FileName0
    ),
    check_module_has_expected_name(FileName, ModuleName, ActualModuleName,
        !IO),

    check_timestamp(FileName0, MaybeTimestamp0, MaybeTimestamp, !IO),
    (
        IgnoreErrors = yes,
        (
            Error = fatal_module_errors,
            Items = []
        ->
            maybe_write_string(VeryVerbose, "not found.\n", !IO)
        ;
            maybe_write_string(VeryVerbose, "done.\n", !IO)
        )
    ;
        IgnoreErrors = no,
        (
            Error = fatal_module_errors,
            maybe_write_string(VeryVerbose, "fatal error(s).\n", !IO),
            io.set_exit_status(1, !IO)
        ;
            Error = some_module_errors,
            maybe_write_string(VeryVerbose, "parse error(s).\n", !IO),
            io.set_exit_status(1, !IO)
        ;
            Error = no_module_errors,
            maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
        ),
        prog_out.write_messages(Messages, !IO)
    ).

read_mod_from_file(FileName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, ModuleName, MaybeTimestamp, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(VeryVerbose, "% ", !IO),
    maybe_write_string(VeryVerbose, Descr, !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    maybe_write_string(VeryVerbose, FileName, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),
    string.append(FileName, Extension, FullFileName),
    ( dir.basename(FileName, BaseFileNamePrime) ->
        BaseFileName = BaseFileNamePrime
    ;
        BaseFileName = ""
    ),
    file_name_to_module_name(BaseFileName, DefaultModuleName),
    (
        Search = yes,
        globals.io_lookup_accumulating_option(search_directories,
            SearchDirs, !IO)
    ;
        Search = no,
        SearchDirs = [dir.this_directory]
    ),
    OpenFile = search_for_file(SearchDirs, FullFileName),
    prog_io.read_module(OpenFile, DefaultModuleName, ReturnTimestamp, Error,
        _, ModuleName, Messages, Items, MaybeTimestamp0, !IO),
    check_timestamp(FullFileName, MaybeTimestamp0, MaybeTimestamp, !IO),
    (
        Error = fatal_module_errors,
        maybe_write_string(VeryVerbose, "fatal error(s).\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Error = some_module_errors,
        maybe_write_string(VeryVerbose, "parse error(s).\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Error = no_module_errors,
        maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
    ),
    prog_out.write_messages(Messages, !IO).

:- pred check_timestamp(file_name::in, maybe(io.res(timestamp))::in,
    maybe(timestamp)::out, io::di, io::uo) is det.

check_timestamp(FileName, MaybeTimestamp0, MaybeTimestamp, !IO) :-
    (
        MaybeTimestamp0 = yes(ok(Timestamp)),
        MaybeTimestamp = yes(Timestamp)
    ;
        MaybeTimestamp0 = yes(error(IOError)),
        MaybeTimestamp = no,
        globals.io_lookup_bool_option(smart_recompilation, SmartRecompilation,
            !IO),
        (
            SmartRecompilation = yes,
            report_modification_time_warning(FileName, IOError, !IO)
        ;
            SmartRecompilation = no
        )
    ;
        MaybeTimestamp0 = no,
        MaybeTimestamp = no
    ).

% :- pred combine_module_errors(module_error, module_error, module_error).
% :- mode combine_module_errors(in, in, out) is det.
%
% combine_module_errors(fatal, _, fatal).
% combine_module_errors(yes, fatal, fatal).
% combine_module_errors(yes, yes, yes).
% combine_module_errors(yes, no, yes).
% combine_module_errors(no, Error, Error).

%-----------------------------------------------------------------------------%

process_module_private_interfaces(_, [], _, _, !DirectImports,
        !DirectUses, !Module, !IO).
process_module_private_interfaces(ReadModules, [Ancestor | Ancestors],
        IntStatusItem, ImpStatusItem, !DirectImports,
        !DirectUses, !Module, !IO) :-
    ModuleName = !.Module ^ module_name,
    ModAncestors0 = !.Module ^ parent_deps,
    ( Ancestor = ModuleName ->
        unexpected(this_file, "process_module_private_interfaces: " ++
            "module is its own ancestor?")
    ; list.member(Ancestor, ModAncestors0) ->
        % we've already read it
        process_module_private_interfaces(ReadModules,
            Ancestors, IntStatusItem, ImpStatusItem,
            !DirectImports, !DirectUses, !Module, !IO)
    ;
        ModItems0 = !.Module ^ items,
        ModError0 = !.Module ^ error,
        maybe_to_bool(!.Module ^ maybe_timestamps, ReturnTimestamp),
        read_mod(ReadModules, Ancestor, ".int0",
            "Reading private interface for module", yes,
            ReturnTimestamp, PrivateIntItems, PrivateIntError,
            _AncestorFileName, MaybeTimestamp, !IO),

        maybe_record_timestamp(Ancestor, ".int0", may_be_unqualified,
            MaybeTimestamp, !Module),

        replace_section_decls(IntStatusItem, ImpStatusItem,
            PrivateIntItems, Items),
        maybe_add_int_error(PrivateIntError, ModError0, ModError),

        globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
        maybe_report_stats(Statistics, !IO),

        ( PrivateIntError = fatal_module_errors ->
            ModAncestors = ModAncestors0
        ;
            ModAncestors = [Ancestor | ModAncestors0]
        ),
        get_dependencies(Items, AncDirectImports, AncDirectUses),
        !:DirectImports = !.DirectImports ++ AncDirectImports,
        !:DirectUses = !.DirectUses ++ AncDirectUses,
        ModItems = ModItems0 ++ Items,
        !:Module = !.Module ^ items := ModItems,
        !:Module = !.Module ^ parent_deps := ModAncestors,
        !:Module = !.Module ^ error := ModError,
        process_module_private_interfaces(ReadModules, Ancestors,
            IntStatusItem, ImpStatusItem,
            !DirectImports, !DirectUses, !Module, !IO)
    ).

%-----------------------------------------------------------------------------%

process_module_long_interfaces(_, _, [], _Ext, _, _,
        !IndirectImports, !ImplIndirectImports, !Module, !IO).
process_module_long_interfaces(ReadModules, NeedQualifier, [Import | Imports],
        Ext, IntStatusItem, ImpStatusItem, !IndirectImports,
        !ImplIndirectImports, !Module, !IO) :-
    ModuleName = !.Module ^ module_name,
    ModImplementationImports0 = !.Module ^ impl_deps,
    (
        % have we already read it?
        ( Import = ModuleName
        ; list.member(Import, !.Module ^ parent_deps)
        ; list.member(Import, !.Module ^ int_deps)
        ; list.member(Import, ModImplementationImports0)
        )
    ->
        process_module_long_interfaces(ReadModules, NeedQualifier,
            Imports, Ext, IntStatusItem, ImpStatusItem,
            !IndirectImports, !ImplIndirectImports, !Module, !IO)
    ;
        ModItems0 = !.Module ^ items,
        ModError0 = !.Module ^ error,
        maybe_to_bool(!.Module ^ maybe_timestamps, ReturnTimestamp),
        read_mod(ReadModules, Import, Ext,
            "Reading interface for module", yes, ReturnTimestamp,
            LongIntItems, LongIntError, _LongIntFileName,
            MaybeTimestamp, !IO),

        get_dependencies(LongIntItems,
            IndirectImports1, IndirectUses1,
            ImplIndirectImports1, ImplIndirectUses1),
        replace_section_decls(IntStatusItem, ImpStatusItem,
            LongIntItems, Items),
        maybe_add_int_error(LongIntError, ModError0, ModError),

        globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
        maybe_report_stats(Statistics, !IO),

        ( LongIntError = fatal_module_errors ->
            ModImplementationImports = ModImplementationImports0
        ;
            maybe_record_timestamp(Import, Ext, NeedQualifier, MaybeTimestamp,
                !Module),
            ModImplementationImports = [Import | ModImplementationImports0]
        ),
        !:IndirectImports = !.IndirectImports ++ IndirectImports1
            ++ IndirectUses1,
        !:ImplIndirectImports = !.ImplIndirectImports
            ++ ImplIndirectImports1 ++ ImplIndirectUses1,
        ModItems = ModItems0 ++ Items,
        !:Module = !.Module ^ impl_deps := ModImplementationImports,
        !:Module = !.Module ^ items := ModItems,
        !:Module = !.Module ^ error := ModError,

        process_module_long_interfaces(ReadModules, NeedQualifier,
            Imports, Ext, IntStatusItem, ImpStatusItem,
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
    item_list::in, list(error_spec)::in, list(error_spec)::out) is det.

check_imports_accessibility(ModuleName, Imports, Items, !Specs) :-
    get_accessible_children(Items, AccessibleSubModules),
    list.foldl(check_module_accessibility(ModuleName,
        AccessibleSubModules, Items), Imports, !Specs).

:- pred check_module_accessibility(module_name::in, list(module_name)::in,
    item_list::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_module_accessibility(ModuleName, AccessibleSubModules, ItemAndContexts,
        ImportedModule, !Specs) :-
    ( ImportedModule = qualified(ParentModule, SubModule) ->
        ( list.member(ImportedModule, AccessibleSubModules) ->
            true
        ;
            % The user attempted to import an inaccessible
            % sub-module, so report an error.
            % Unfortunately we didn't get passed the
            % context of the `import_module' or `use_module'
            % declaration(s), so we need to search the item
            % list again to find them.
            FindImports = (pred(ItemAndContext::in) is semidet :-
                ItemAndContext = item_and_context(Item, _),
                Item = item_module_defn(_, ModuleDefn),
                ( ModuleDefn = md_import(list_module(Mods))
                ; ModuleDefn = md_use(list_module(Mods))
                ),
                list.member(ImportedModule, Mods)
            ),
            list.filter(FindImports, ItemAndContexts, ImportItemAndContexts),
            (
                ImportItemAndContexts = [],
                unexpected(this_file, "check_parent_module")
            ;
                ImportItemAndContexts = [_ | _]
            ),
            list.foldl(
                report_inaccessible_module_error(
                    ModuleName, ParentModule, SubModule),
                ImportItemAndContexts, !Specs)
        )
    ;
        true
    ).

:- pred report_inaccessible_module_error(module_name::in, module_name::in,
    string::in, item_and_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

% The error message should come out like this
% (the second sentence is included only with --verbose-errors):
% very_long_name.m:123: In module `very_long_name':
% very_long_name.m:123:   error in `import_module' declaration:
% very_long_name.m:123:   module `parent_module:sub_module' is inaccessible.
% very_long_name.m:123:   Either there was no prior `import_module' or
% very_long_name.m:123:   `use_module' declaration to import module
% very_long_name.m:123:   `parent_module', or the interface for module
% very_long_name.m:123:   `parent_module' does not contain an `include_module'
% very_long_name.m:123:   declaration for module `sub_module'.

report_inaccessible_module_error(ModuleName, ParentModule, SubModule,
        item_and_context(Item, Context), !Specs) :-
    ( Item = item_module_defn(_, md_import(list_module(_))) ->
        DeclName = "import_module"
    ; Item = item_module_defn(_, md_use(list_module(_))) ->
        DeclName = "use_module"
    ;
        unexpected(this_file,
            "report_inaccessible_parent_error: invalid item")
    ),
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

process_module_short_interfaces_and_impls_transitively(ReadModules,
        Imports, Ext, IntStatusItem, ImpStatusItem, !Module, !IO) :-
    process_module_short_interfaces_transitively(ReadModules, Imports, Ext,
        IntStatusItem, ImpStatusItem, [], ImpIndirectImports, !Module, !IO),
    (
        ImpIndirectImports = []
    ;
        ImpIndirectImports = [_ | _],
        process_module_short_interfaces_and_impls_transitively(
            ReadModules, ImpIndirectImports, Ext,
            IntStatusItem, ImpStatusItem, !Module, !IO)
    ).

process_module_short_interfaces_transitively(ReadModules, Imports, Ext,
        IntStatusItem, ImpStatusItem, !ImpIndirectImports, !Module, !IO) :-
    process_module_short_interfaces(ReadModules, Imports, Ext,
        IntStatusItem, ImpStatusItem, [], IndirectImports,
        !ImpIndirectImports, !Module, !IO),
    (
        IndirectImports = []
    ;
        IndirectImports = [_ | _],
        process_module_short_interfaces_transitively(ReadModules,
            IndirectImports, Ext, IntStatusItem, ImpStatusItem,
            !ImpIndirectImports, !Module, !IO)
    ).

process_module_short_interfaces(_, [], _, _, _, !IndirectImports,
        !ImpIndirectImports, !Module, !IO).
process_module_short_interfaces(ReadModules, [Import | Imports], Ext,
        IntStatusItem, ImpStatusItem, !IndirectImports,
        !ImpIndirectImports, !Module, !IO) :-
    ModIndirectImports0 = !.Module ^ indirect_deps,
    (
        % check if the imported module has already been imported
        ( Import = !.Module ^ module_name
        ; list.member(Import, !.Module ^ parent_deps)
        ; list.member(Import, !.Module ^ int_deps)
        ; list.member(Import, !.Module ^ impl_deps)
        ; list.member(Import, ModIndirectImports0)
        )
    ->
        process_module_short_interfaces(ReadModules, Imports, Ext,
            IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ;
        ModItems0 = !.Module ^ items,
        ModError0 = !.Module ^ error,
        maybe_to_bool(!.Module ^ maybe_timestamps, ReturnTimestamp),
        read_mod(ReadModules, Import, Ext,
            "Reading short interface for module", yes,
            ReturnTimestamp, ShortIntItems, ShortIntError,
            _ImportFileName, MaybeTimestamp, !IO),
        maybe_record_timestamp(Import, Ext, must_be_qualified,
            MaybeTimestamp, !Module),

        get_dependencies(ShortIntItems, IntImports1, IntUses1,
            ImpImports1, ImpUses1),
        replace_section_decls(IntStatusItem, ImpStatusItem,
            ShortIntItems, Items),
        maybe_add_int_error(ShortIntError, ModError0, ModError),

        globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
        maybe_report_stats(Statistics, !IO),

        ModIndirectImports = [Import | ModIndirectImports0],
        !:IndirectImports = !.IndirectImports ++ IntImports1 ++ IntUses1,
        !:ImpIndirectImports = !.ImpIndirectImports ++ ImpImports1 ++ ImpUses1,
        ModItems = ModItems0 ++ Items,
        !:Module = !.Module ^ indirect_deps := ModIndirectImports,
        !:Module = !.Module ^ items := ModItems,
        !:Module = !.Module ^ error := ModError,
        process_module_short_interfaces(ReadModules, Imports, Ext,
            IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ).

replace_section_decls(IntStatusItemAndContext, ImpStatusItemAndContext,
        !ItemAndContexts) :-
    list.map(
        replace_section_decl(IntStatusItemAndContext, ImpStatusItemAndContext),
        !ItemAndContexts).

:- pred replace_section_decl(item_and_context::in, item_and_context::in,
    item_and_context::in, item_and_context::out) is det.

replace_section_decl(IntStatusItemAndContext, ImpStatusItemAndContext,
        ItemAndContext0, ItemAndContext) :-
    (
        ItemAndContext0 = item_and_context(item_module_defn(_, Defn), _),
        (
            Defn = md_interface,
            ItemAndContextPrime = IntStatusItemAndContext
        ;
            Defn = md_implementation,
            ItemAndContextPrime = ImpStatusItemAndContext
        )
    ->
        ItemAndContext = ItemAndContextPrime
    ;
        ItemAndContext = ItemAndContext0
    ).

:- pred maybe_add_int_error(module_error::in, module_error::in,
    module_error::out) is det.

maybe_add_int_error(InterfaceError, ModError0, ModError) :-
    ( InterfaceError \= no_module_errors ->
        ModError = some_module_errors
    ;
        ModError = ModError0
    ).

%-----------------------------------------------------------------------------%

get_ancestors(ModuleName) = get_ancestors_2(ModuleName, []).

:- func get_ancestors_2(module_name, list(module_name)) = list(module_name).

get_ancestors_2(unqualified(_), Ancestors) = Ancestors.
get_ancestors_2(qualified(Parent, _), Ancestors0) =
    get_ancestors_2(Parent, [Parent | Ancestors0]).

%-----------------------------------------------------------------------------%

    % get_children(Items, IncludeDeps):
    %
    % IncludeDeps is the list of sub-modules declared with `:- include_module'
    % in Items.
    %
:- pred get_children(item_list::in, list(module_name)::out) is det.

get_children(Items, IncludeDeps) :-
    get_children_2(Items, [], IncludeDeps).

:- pred get_children_2(item_list::in, list(module_name)::in,
    list(module_name)::out) is det.

get_children_2([], IncludeDeps, IncludeDeps).
get_children_2([ItemAndContext | ItemAndContexts],
        IncludeDeps0, IncludeDeps) :-
    ItemAndContext = item_and_context(Item, _),
    ( Item = item_module_defn(_VarSet, md_include_module(Modules)) ->
        IncludeDeps1 = IncludeDeps0 ++ Modules
    ;
        IncludeDeps1 = IncludeDeps0
    ),
    get_children_2(ItemAndContexts, IncludeDeps1, IncludeDeps).

    % get_accessible_children(Items, IncludeDeps):
    %
    % IncludeDeps is the list of sub-modules declared with `:- include_module'
    % in Items which are visible in the current module.
    %
:- pred get_accessible_children(item_list::in, list(module_name)::out) is det.

get_accessible_children(Items, IncludeDeps) :-
    get_accessible_children_2(yes, Items, [], IncludeDeps).

:- pred get_accessible_children_2(bool::in, item_list::in,
    list(module_name)::in, list(module_name)::out) is det.

get_accessible_children_2(_, [], !IncludeDeps).
get_accessible_children_2(!.Visible, [ItemAndContext | ItemAndContexts],
        !IncludeDeps) :-
    ItemAndContext = item_and_context(Item, _),
    (
        Item = item_module_defn(_VarSet, Defn),
        ( Defn = md_abstract_imported
        ; Defn = md_opt_imported
        ; Defn = md_transitively_imported
        )
    ->
        !:Visible = no
    ;
        Item = item_module_defn(_VarSet, Defn),
        ( Defn = md_imported(_)
        ; Defn = md_used(_)
        ; Defn = md_interface
        ; Defn = md_implementation
        ; Defn = md_private_interface
        )
    ->
        !:Visible = yes
    ;
        Item = item_module_defn(_VarSet, md_include_module(Modules)),
        !.Visible = yes
    ->
        !:IncludeDeps = !.IncludeDeps ++ Modules
    ;
        true
    ),
    get_accessible_children_2(!.Visible, ItemAndContexts, !IncludeDeps).

%-----------------------------------------------------------------------------%

get_dependencies(Items, ImportDeps, UseDeps) :-
    get_dependencies_implementation(Items,
        [], IntImportDeps, [], IntUseDeps, [], ImpImportDeps, [], ImpUseDeps),
    ImportDeps = IntImportDeps ++ ImpImportDeps,
    UseDeps = IntUseDeps ++ ImpUseDeps.

    % get_dependencies(Items, IntImportDeps, IntUseDeps,
    %   ImpImportDeps, ImpUseDeps):
    %
    % Get the list of modules that a list of items (explicitly) depends on.
    %
    % IntImportDeps is the list of modules imported using `:- import_module'
    % in the interface, and ImpImportDeps those modules imported in the
    % implementation. IntUseDeps is the list of modules imported using
    % `:- use_module' in the interface, and ImpUseDeps those modules imported
    % in the implementation.
    %
    % N.B. Typically you also need to consider the module's implicit
    % dependencies (see get_implicit_dependencies/3), its parent modules
    % (see get_ancestors/1) and possibly also the module's child modules
    % (see get_children/2). You may also need to consider indirect
    % dependencies.
    %
    % N.B This predicate assumes that any declarations between the `:- module'
    % and the first `:- interface' or `:- implementation' are in the
    % implementation.
    %
:- pred get_dependencies(item_list::in,
    list(module_name)::out, list(module_name)::out,
    list(module_name)::out, list(module_name)::out) is det.

get_dependencies(Items, IntImportDeps, IntUseDeps,
        ImpImportDeps, ImpUseDeps) :-
    get_dependencies_implementation(Items,
        [], IntImportDeps, [], IntUseDeps, [], ImpImportDeps, [], ImpUseDeps).

:- pred get_dependencies_implementation(item_list::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

get_dependencies_implementation([],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps).
get_dependencies_implementation([ItemAndContext | ItemAndContexts],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps) :-
    ItemAndContext = item_and_context(Item, _Context),
    ( Item = item_module_defn(_VarSet, md_interface) ->
        get_dependencies_interface(ItemAndContexts,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ;
        ( Item = item_module_defn(_VarSet, md_import(list_module(Modules))) ->
            !:ImpImportDeps = !.ImpImportDeps ++ Modules
        ; Item = item_module_defn(_VarSet, md_use(list_module(Modules))) ->
            !:ImpUseDeps = !.ImpUseDeps ++ Modules
        ;
            true
        ),
        get_dependencies_implementation(ItemAndContexts,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ).

:- pred get_dependencies_interface(item_list::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

get_dependencies_interface([],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps).
get_dependencies_interface([ItemAndContext | ItemAndContexts],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps) :-
    ItemAndContext = item_and_context(Item, _Context),
    ( Item = item_module_defn(_VarSet, md_implementation) ->
        get_dependencies_implementation(ItemAndContexts,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ;
        ( Item = item_module_defn(_VarSet, md_import(list_module(Modules))) ->
            !:IntImportDeps = !.IntImportDeps ++ Modules
        ; Item = item_module_defn(_VarSet, md_use(list_module(Modules))) ->
            !:IntUseDeps = !.IntUseDeps ++ Modules
        ;
            true
        ),
        get_dependencies_interface(ItemAndContexts,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ).

%-----------------------------------------------------------------------------%

    % get the fact table dependencies for a module
:- pred get_fact_table_dependencies(item_list::in, list(string)::out) is det.

get_fact_table_dependencies(Items, Deps) :-
    get_fact_table_dependencies_2(Items, [], Deps).

:- pred get_fact_table_dependencies_2(item_list::in, list(string)::in,
    list(string)::out) is det.

get_fact_table_dependencies_2([], !Deps).
get_fact_table_dependencies_2([ItemAndContext | ItemAndContexts], !Deps) :-
    ItemAndContext = item_and_context(Item, _Context),
    ( Item = item_pragma(_, pragma_fact_table(_SymName, _Arity, FileName)) ->
        !:Deps = [FileName | !.Deps]
    ;
        true
    ),
    get_fact_table_dependencies_2(ItemAndContexts, !Deps).

%-----------------------------------------------------------------------------%

:- type submodule_map == map(module_name, item_list).

    % Given a module (well, a list of items), split it into
    % its constituent sub-modules, in top-down order.
    %
split_into_submodules(ModuleName, ItemAndContexts0, ModuleList, !Specs) :-
    InParentInterface = no,
    split_into_submodules_2(ModuleName, ItemAndContexts0, InParentInterface,
        ItemAndContexts, ModuleList, !Specs),

    % Check that there are no items after the end_module declaration.
    ( ItemAndContexts = [item_and_context(_Item, Context) | _] ->
        report_items_after_end_module(Context, !Specs)
    ;
        true
    ),

    % Check for modules declared as both nested and separate sub-modules.
    get_children(ItemAndContexts0, NestedSubmodules),
    assoc_list.keys(ModuleList, SeparateSubModules),
    Duplicates = set.intersect(
        set.list_to_set(NestedSubmodules),
        set.list_to_set(SeparateSubModules)),
    ( set.empty(Duplicates) ->
        true
    ;
        report_duplicate_modules(Duplicates, ItemAndContexts0, !Specs)
    ).

:- pred split_into_submodules_2(module_name::in, item_list::in, bool::in,
    item_list::out, module_list::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_into_submodules_2(ModuleName, ItemAndContexts0, InParentInterface,
        ItemAndContexts, ModuleList, !Specs) :-
    InInterface0 = no,
    split_into_submodules_3(ModuleName, ItemAndContexts0,
        InParentInterface, InInterface0,
        ThisModuleItemAndContexts, ItemAndContexts, SubModules, !Specs),
    map.to_assoc_list(SubModules, SubModuleList),
    ModuleList = [ModuleName - ThisModuleItemAndContexts | SubModuleList].

:- pred split_into_submodules_3(module_name::in, item_list::in, bool::in,
    bool::in, item_list::out, item_list::out,
    map(module_name, item_list)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_into_submodules_3(_ModuleName, [], _, _, [], [], SubModules, !Specs) :-
    map.init(SubModules).
split_into_submodules_3(ModuleName, [ItemAndContext | ItemAndContexts1],
        InParentInterface, InInterface0,
        ThisModuleItemAndContexts, OtherItemAndContexts, SubModules, !Specs) :-
    ItemAndContext = item_and_context(Item, Context),
    (
        % Check for a `module' declaration, which signals the start
        % of a nested module.
        Item = item_module_defn(VarSet, md_module(SubModuleName))
    ->
        % Parse in the items for the nested submodule.
        split_into_submodules_2(SubModuleName, ItemAndContexts1, InInterface0,
            ItemAndContexts2, SubModules0, !Specs),

        % Parse in the remaining items for this module.
        split_into_submodules_3(ModuleName, ItemAndContexts2,
            InParentInterface, InInterface0,
            ThisModuleItemAndContexts0, ItemAndContexts3,
            SubModules1, !Specs),

        % Combine the sub-module declarations from the previous two steps.
        list.foldl(add_submodule, SubModules0, SubModules1, SubModules),

        % Replace the nested submodule with an `include_module' declaration.
        IncludeSubMod = item_and_context(item_module_defn(VarSet,
            md_include_module([SubModuleName])), Context),
        ThisModuleItemAndContexts =
            [IncludeSubMod | ThisModuleItemAndContexts0],
        OtherItemAndContexts = ItemAndContexts3
    ;
        % Check for a matching `end_module' declaration.
        Item = item_module_defn(_VarSet, md_end_module(ModuleName))
    ->
        % If so, that's the end of this module.
        ThisModuleItemAndContexts = [],
        OtherItemAndContexts = ItemAndContexts1,
        map.init(SubModules)
    ;
        % Otherwise, process the next item in this module.

        % Update the flag which records whether we're currently in the
        % interface section, and report an error if there is an
        % `implementation' section inside an `interface' section.
        ( Item = item_module_defn(_, md_interface) ->
            InInterface1 = yes
        ; Item = item_module_defn(_, md_implementation) ->
            (
                InParentInterface = yes,
                report_error_implementation_in_interface(ModuleName, Context,
                    !Specs)
            ;
                InParentInterface = no
            ),
            InInterface1 = no
        ;
            InInterface1 = InInterface0
        ),

        % Check to make sure that a non-abstract instance declaration
        % does not occur in a module interface.
        (
            InInterface1 = yes,
            Item = item_instance(_, _, _, Body, _, _),
            Body \= instance_body_abstract
        ->
            report_non_abstract_instance_in_interface(Context, !Specs)
        ;
            true
        ),

        % Parse the remaining items for this module.
        split_into_submodules_3(ModuleName, ItemAndContexts1,
            InParentInterface, InInterface1,
            ThisModuleItemAndContexts0, ItemAndContexts2, SubModules, !Specs),

        % Put the current item back onto the front of the item list
        % for this module.
        ThisModuleItemAndContexts =
            [ItemAndContext | ThisModuleItemAndContexts0],
        OtherItemAndContexts = ItemAndContexts2
    ).

:- pred add_submodule(pair(module_name, item_list)::in,
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
        svmap.det_update(ModuleName, ItemList, !SubModules)
    ;
        svmap.det_insert(ModuleName, ModuleItemList, !SubModules)
    ).

:- pred report_error_implementation_in_interface(module_name::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_error_implementation_in_interface(ModuleName, Context, !Specs) :-
    ( ModuleName = qualified(ParentModule0, ChildModule0) ->
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        unexpected(this_file, "report_error_implementation_in_interface")
    ),
    Pieces = [words("In interface for module"), sym_name(ParentModule),
        suffix(":"), nl,
        words("in definition of sub-module `" ++ ChildModule ++ "':"), nl,
        words("error: `:- implementation.' declaration for sub-module\n"),
        words("occurs in interface section of parent module.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_duplicate_modules(set(module_name)::in, item_list::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_modules(Duplicates, ItemAndContexts, !Specs) :-
    solutions.solutions(is_duplicate_error(Duplicates, ItemAndContexts),
        DuplicateErrors),
    list.foldl(report_error_duplicate_module_decl, DuplicateErrors, !Specs).

:- pred is_duplicate_error(set(module_name)::in, item_list::in,
    pair(module_name, prog_context)::out) is nondet.

is_duplicate_error(Duplicates, ItemAndContexts, SubModuleName - Context) :-
    list.member(ItemAndContext, ItemAndContexts),
    ItemAndContext = item_and_context(Item, Context),
    Item = item_module_defn(_VarSet, ModuleDefn),
    (
        ModuleDefn = md_module(SubModuleName)
    ;
        ModuleDefn = md_include_module(SubModuleNames),
        list.member(SubModuleName, SubModuleNames)
    ),
    set.member(SubModuleName, Duplicates).

:- pred report_error_duplicate_module_decl(pair(module_name, prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_error_duplicate_module_decl(ModuleName - Context, !Specs) :-
    ( ModuleName = qualified(ParentModule0, ChildModule0) ->
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        unexpected(this_file, "report_error_duplicate_module_decl")
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

    % Given a module (well, a list of items), extract the interface
    % part of that module, i.e. all the items between `:- interface'
    % and `:- implementation'.
    % The bodies of instance definitions are removed because
    % the instance methods have not yet been module qualified.
    %
:- pred get_interface(module_name::in, bool::in, item_list::in, item_list::out)
    is det.

get_interface(ModuleName, IncludeImplTypes, Items0, Items) :-
    AddToImpl = (func(_, ImplItems) = ImplItems),
    get_interface_and_implementation_2(IncludeImplTypes, Items0, no,
        [], RevItems, AddToImpl, unit, _),
    list.reverse(RevItems, Items1),
    maybe_add_foreign_import_module(ModuleName, Items1, Items2),
    order_items(Items2, Items).

:- pred get_interface_and_implementation(module_name::in, bool::in,
    item_list::in, item_list::out, item_list::out) is det.

get_interface_and_implementation(ModuleName, IncludeImplTypes,
        Items0, InterfaceItems, ImplementationItems) :-
    AddToImpl = (func(ImplItem, ImplItems) = [ImplItem | ImplItems]),
    get_interface_and_implementation_2(IncludeImplTypes, Items0, no,
        [], RevIntItems, AddToImpl, [], RevImplItems),
    list.reverse(RevIntItems, InterfaceItems0),
    list.reverse(RevImplItems, ImplementationItems),
    maybe_add_foreign_import_module(ModuleName,
        InterfaceItems0, InterfaceItems).

:- pred maybe_add_foreign_import_module(module_name::in,
    item_list::in, item_list::out) is det.

maybe_add_foreign_import_module(ModuleName,
        ItemAndContexts0, ItemAndContexts) :-
    get_foreign_self_imports(ItemAndContexts0, Langs),
    ImportItemAndContexts = list.map(make_foreign_import(ModuleName), Langs),
    ItemAndContexts = ImportItemAndContexts ++ ItemAndContexts0.

:- func make_foreign_import(module_name, foreign_language) = item_and_context.

make_foreign_import(ModuleName, Lang) = ImportItemAndContext :-
    Origin = compiler(foreign_imports),
    Pragma = pragma_foreign_import_module(Lang, ModuleName),
    ImportItem = item_pragma(Origin, Pragma),
    ImportItemAndContext = item_and_context(ImportItem, term.context_init).

:- pred get_foreign_self_imports(item_list::in, list(foreign_language)::out)
    is det.

get_foreign_self_imports(Items, Langs) :-
    list.foldl(accumulate_item_foreign_import_langs, Items, set.init, LangSet),
    set.to_sorted_list(LangSet, Langs).

:- pred accumulate_item_foreign_import_langs(item_and_context::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

accumulate_item_foreign_import_langs(item_and_context(Item, _), !LangSet) :-
    solutions.solutions(item_needs_foreign_imports(Item), Langs),
    svset.insert_list(Langs, !LangSet).

:- pred get_interface_and_implementation_2(bool::in, item_list::in, bool::in,
    item_list::in, item_list::out,
    func(item_and_context, T) = T::in, T::in, T::out) is det.

get_interface_and_implementation_2(_, [], _, !IntItemAndContexts,
        _, !ImplItemAndContexts).
get_interface_and_implementation_2(IncludeImplTypes, [ItemAndContext | Rest],
        InInterface0, !IntItemAndContexts,
        AddImplItem, !ImplItemAndContexts) :-
    ItemAndContext = item_and_context(Item, Context),
    (
        Item = item_module_defn(_, md_interface)
    ->
        !:IntItemAndContexts = [ItemAndContext | !.IntItemAndContexts],
        InInterface1 = yes,
        Continue = yes
    ;
        Item = item_module_defn(_, Defn),
        ( Defn = md_imported(_)
        ; Defn = md_used(_)
        )
    ->
        % Items after here are not part of this module.
        InInterface1 = no,
        Continue = no
    ;
        Item = item_module_defn(_, md_implementation)
    ->
        !:IntItemAndContexts = [ItemAndContext | !.IntItemAndContexts],
        InInterface1 = no,
        Continue = yes
    ;
        (
            InInterface0 = yes,
            ( make_abstract_instance(Item, Item1) ->
                ItemToWrite = Item1,
                !:ImplItemAndContexts =
                    AddImplItem(ItemAndContext, !.ImplItemAndContexts)
            ;
                ItemToWrite = Item
            ),
            !:IntItemAndContexts = [item_and_context(ItemToWrite, Context)
                | !.IntItemAndContexts]
        ;
            InInterface0 = no,
            !:ImplItemAndContexts =
                AddImplItem(ItemAndContext, !.ImplItemAndContexts),
            (
                IncludeImplTypes = yes,
                include_in_int_file_implementation(Item)
            ->
                ( make_abstract_defn(Item, int2, ImpItem1) ->
                    ImpItem = ImpItem1
                ; make_abstract_unify_compare(Item, int2, ImpItem1) ->
                    ImpItem = ImpItem1
                ;
                    ImpItem = Item
                ),
                !:IntItemAndContexts = [item_and_context(ImpItem, Context)
                    | !.IntItemAndContexts]
            ;
                true
            )
        ),
        InInterface1 = InInterface0,
        Continue = yes
    ),
    (
        Continue = yes,
        get_interface_and_implementation_2(IncludeImplTypes, Rest,
            InInterface1, !IntItemAndContexts,
            AddImplItem, !ImplItemAndContexts)
    ;
        Continue = no
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
:- pred get_short_interface(item_list::in, short_interface_kind::in,
    item_list::out) is det.

get_short_interface(Items0, Kind, Items) :-
    get_short_interface_2(Items0, Kind, [], RevItems),
    list.reverse(RevItems, Items1),
    maybe_strip_import_decls(Items1, Items2),
    order_items(Items2, Items).

:- pred get_short_interface_2(item_list::in, short_interface_kind::in,
    item_list::in, item_list::out) is det.

get_short_interface_2([], _Kind, Items, Items).
get_short_interface_2([ItemAndContext | Rest], Kind, Items0, Items) :-
    ItemAndContext = item_and_context(Item0, Context),
    ( make_abstract_defn(Item0, Kind, Item1) ->
        Items1 = [item_and_context(Item1, Context) | Items0]
    ; make_abstract_unify_compare(Item0, Kind, Item1) ->
        Items1 = [item_and_context(Item1, Context) | Items0]
    ; include_in_short_interface(Item0) ->
        Items1 = [ItemAndContext | Items0]
    ;
        Items1 = Items0
    ),
    get_short_interface_2(Rest, Kind, Items1, Items).

:- pred include_in_short_interface(item::in) is semidet.

include_in_short_interface(item_type_defn(_, _, _, _, _)).
include_in_short_interface(item_inst_defn(_, _, _, _, _)).
include_in_short_interface(item_mode_defn(_, _, _, _, _)).
include_in_short_interface(item_module_defn(_, _)).
include_in_short_interface(item_instance(_, _, _, _, _, _)).
include_in_short_interface(item_pragma(_, pragma_foreign_import_module(_, _))).

    % Could this item use items from imported modules.
    %
:- func item_needs_imports(item) = bool.

item_needs_imports(item_clause(_, _, _, _, _, _)) = yes.
item_needs_imports(Item @ item_type_defn(_, _, _, _, _)) =
        ( Item ^ td_ctor_defn = parse_tree_abstract_type(_) -> no ; yes ).
item_needs_imports(item_inst_defn(_, _, _, _, _)) = yes.
item_needs_imports(item_mode_defn(_, _, _, _, _)) = yes.
item_needs_imports(item_module_defn(_, _)) = no.
item_needs_imports(item_pragma(_, _)) = yes.
item_needs_imports(item_pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _, _)) =
    yes.
item_needs_imports(item_pred_or_func_mode(_, _, _, _, _, _, _)) = yes.
item_needs_imports(item_typeclass(_, _, _, _, _, _)) = yes.
item_needs_imports(item_instance(_, _, _, _, _, _)) = yes.
item_needs_imports(item_promise(_, _, _, _)) = yes.
item_needs_imports(item_initialise(_, _, _)) = yes.
item_needs_imports(item_finalise(_, _, _)) = yes.
item_needs_imports(item_mutable(_, _, _, _, _, _)) = yes.
item_needs_imports(item_nothing(_)) = no.

:- pred item_needs_foreign_imports(item::in, foreign_language::out) is nondet.

item_needs_foreign_imports(item_pragma(_,
        pragma_foreign_export(Lang, _, _, _, _)), Lang).

    % `:- pragma import' is only supported for C.
item_needs_foreign_imports(item_pragma(_, pragma_import(_, _, _, _, _)),
        lang_c).
item_needs_foreign_imports(Item @ item_type_defn(_, _, _, _, _), Lang) :-
    Item ^ td_ctor_defn = parse_tree_foreign_type(ForeignType, _, _),
    Lang = foreign_type_language(ForeignType).
item_needs_foreign_imports(item_pragma(_, pragma_foreign_decl(Lang, _, _)),
        Lang).
item_needs_foreign_imports(item_pragma(_, pragma_foreign_code(Lang, _)), Lang).
item_needs_foreign_imports(item_pragma(_,
        pragma_foreign_proc(Attrs, _, _, _, _, _, _)),
        get_foreign_language(Attrs)).
item_needs_foreign_imports(item_mutable(_, _, _, _, _, _), Lang) :-
    foreign_language(Lang).

:- pred include_in_int_file_implementation(item::in) is semidet.

include_in_int_file_implementation(item_type_defn(_, _, _, _, _)).
include_in_int_file_implementation(item_module_defn(_, Defn)) :-
    Defn \= md_external(_, _).

    % `:- typeclass declarations' may be referred to
    % by the constructors in type declarations.
    % Since these constructors are abstractly exported,
    % we won't need the local instance declarations.
    %
include_in_int_file_implementation(item_typeclass(_, _, _, _, _, _)).
include_in_int_file_implementation(item_pragma(_,
    pragma_foreign_import_module(_, _))).

:- pred make_abstract_defn(item::in, short_interface_kind::in, item::out)
    is semidet.

make_abstract_defn(Item0 @ item_type_defn(_VarSet, _Name, _Args, TypeDefn,
        _Cond), ShortInterfaceKind,
        Item0 ^ td_ctor_defn := parse_tree_abstract_type(IsSolverType)) :-
    (
        TypeDefn = parse_tree_du_type(_, _),
        IsSolverType = non_solver_type,
        % For the `.int2' files, we need the full definitions of
        % discriminated union types.  Even if the functors for a type
        % are not used within a module, we may need to know them for
        % comparing insts, e.g. for comparing `ground' and `bound(...)'.
        ShortInterfaceKind = int3
    ;
        TypeDefn = parse_tree_abstract_type(IsSolverType)
    ;
        TypeDefn = parse_tree_solver_type(_, _),
        % rafe: XXX we need to also export the details of the
        % forwarding type for the representation and the forwarding
        % pred for initialization.
        IsSolverType = solver_type
    ;
        TypeDefn = parse_tree_eqv_type(_),
        % rafe: XXX what *should* IsSolverType be here?  We need
        % to know properly.
        IsSolverType = non_solver_type,
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
        IsSolverType = non_solver_type,
        semidet_fail
    ).
make_abstract_defn(item_instance(_, _, _, _, _, _) @ Item0, int2, Item) :-
    make_abstract_instance(Item0, Item).
make_abstract_defn(item_typeclass(_, _, _, _, _, _) @ Item, _,
        Item ^ tc_class_methods := class_interface_abstract).

:- pred make_abstract_unify_compare(item::in, short_interface_kind::in,
    item::out) is semidet.

make_abstract_unify_compare(
        item_type_defn(VarSet, Name, Args, TypeDefn0, Cond), int2,
        item_type_defn(VarSet, Name, Args, TypeDefn, Cond)) :-
    (
        TypeDefn0 = parse_tree_du_type(Constructors, yes(_UserEqComp)),
        TypeDefn  = parse_tree_du_type(Constructors, yes(
                abstract_noncanonical_type(non_solver_type)))
    ;
        TypeDefn0 = parse_tree_foreign_type(ForeignType,
            yes(_UserEqComp), Assertions),
        TypeDefn  = parse_tree_foreign_type(ForeignType,
            yes(abstract_noncanonical_type(non_solver_type)), Assertions)
    ;
        TypeDefn0 = parse_tree_solver_type(SolverTypeDetails,
            yes(_UserEqComp)),
        TypeDefn  = parse_tree_solver_type(SolverTypeDetails,
            yes(abstract_noncanonical_type(solver_type)))
    ).

    % All instance declarations must be written to `.int' files as
    % abstract instance declarations, because the method names have not yet
    % been module qualified. This could cause the wrong predicate to be
    % used if calls to the method are specialized.
    %
:- pred make_abstract_instance(item::in, item::out) is semidet.

make_abstract_instance(Item0, Item) :-
    Item0 = item_instance(_Constraints, _Class, _ClassTypes, Body0,
        _TVarSet, _ModName),
    Body0 = instance_body_concrete(_),
    Body = instance_body_abstract,
    Item = Item0 ^ ci_method_instances := Body.

:- pred maybe_strip_import_decls(item_list::in, item_list::out) is det.

maybe_strip_import_decls(!ItemAndContexts) :-
    (
        some [Item] (
            list.member(item_and_context(Item, _), !.ItemAndContexts),
            item_needs_imports(Item) = yes
        )
    ->
        true
    ;
        list.filter(
            (pred((item_and_context(ThisItem, _))::in) is semidet :-
                \+ (
                    ThisItem = item_module_defn(_, Defn),
                    ( Defn = md_import(_)
                    ; Defn = md_use(_)
                    )
                )
            ), !ItemAndContexts)
    ),
    (
        some [Item] (
            list.member(item_and_context(Item, _), !.ItemAndContexts),
            item_needs_foreign_imports(Item, _)
        )
    ->
        true
    ;
        list.filter(
            (pred((item_and_context(ThisItem, _))::in) is semidet :-
                ThisItem \= item_pragma(_, pragma_foreign_import_module(_, _))
            ), !ItemAndContexts)
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
:- pred order_items(item_list::in, item_list::out) is det.

order_items(ItemAndContexts0, ItemAndContexts) :-
    filter_unnecessary_flips(ItemAndContexts0, other, ItemAndContexts1),
    do_order_items(ItemAndContexts1, ItemAndContexts2),
    % Delete any redundant :- interface and :- implementation markers at the
    % end, to make Items as insensitive as we can to the number of interface
    % sections in the source file. If some of the implementation sections
    % are not empty, we won't be fully successful.
    list.reverse(ItemAndContexts2, RevItemAndContexts2),
    list.takewhile(interface_or_import_marker, RevItemAndContexts2, _,
        RevItemAndContexts),
    list.reverse(RevItemAndContexts, ItemAndContexts).

:- pred interface_or_import_marker(item_and_context::in) is semidet.

interface_or_import_marker(item_and_context(Item, _)) :-
    ( Item = item_module_defn(_, md_interface)
    ; Item = item_module_defn(_, md_implementation)
    ).

    % Which section of the module we are in. The "other" alternative
    % reflects my ignorance (based on the lack of documentation) of
    % the invariants that govern the items involved in the representation
    % of nested modules. -zs

:- type cur_pos
    --->    in_interface
    ;       in_implementation
    ;       other.

:- pred filter_unnecessary_flips(item_list::in, cur_pos::in, item_list::out)
    is det.

filter_unnecessary_flips([], _, []).
filter_unnecessary_flips([ItemAndContext], _, [ItemAndContext]).
filter_unnecessary_flips([ItemAndContext1, ItemAndContext2 | ItemAndContexts0],
        CurPos, ItemAndContexts) :-
    ItemAndContext1 = item_and_context(Item1, _Context1),
    ItemAndContext2 = item_and_context(Item2, _Context2),
    (
        CurPos = in_interface,
        Item1 = item_module_defn(_, md_implementation),
        Item2 = item_module_defn(_, md_interface)
    ->
        filter_unnecessary_flips(ItemAndContexts0, CurPos, ItemAndContexts)
    ;
        CurPos = in_implementation,
        Item1 = item_module_defn(_, md_interface),
        Item2 = item_module_defn(_, md_implementation)
    ->
        filter_unnecessary_flips(ItemAndContexts0, CurPos, ItemAndContexts)
    ;
        ( Item1 = item_module_defn(_, md_implementation) ->
            NextPos = in_implementation
        ; Item1 = item_module_defn(_, md_interface) ->
            NextPos = in_interface
        ; chunkable_item(Item1) = yes ->
            NextPos = CurPos
        ;
            NextPos = other
        ),
        filter_unnecessary_flips([ItemAndContext2 | ItemAndContexts0], NextPos,
            ItemAndContextsTail),
        ItemAndContexts = [ItemAndContext1 | ItemAndContextsTail]
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
:- pred do_order_items(item_list::in, item_list::out) is det.

do_order_items([], []).
do_order_items([ItemAndContext0 | ItemAndContexts0], OrderedItemAndContexts) :-
    ( chunkable_item_and_context(ItemAndContext0) = yes ->
        list.takewhile(is_chunkable, ItemAndContexts0, FrontItemAndContexts,
        RemainItemAndContexts),
        list.filter(is_reorderable, [ItemAndContext0 | FrontItemAndContexts],
            ReorderableItemAndContexts, NonReorderableItemAndContexts),
        list.filter(import_or_use, ReorderableItemAndContexts,
            ImportReorderableItemAndContexts,
            NonImportReorderableItemAndContexts),
        list.filter(symname_orderable, NonReorderableItemAndContexts,
            SymNameItemAndContexts, NonSymNameItemAndContexts),
        % We rely on the sort being stable to keep the items with the same
        % sym_names in their original order.
        list.sort(compare_by_symname, SymNameItemAndContexts,
            OrderedSymNameItemAndContexts),
        do_order_items(RemainItemAndContexts, OrderedRemainItemAndContexts),
        OrderedItemAndContexts = list.sort(ImportReorderableItemAndContexts) ++
            list.sort(NonImportReorderableItemAndContexts) ++
            OrderedSymNameItemAndContexts ++ NonSymNameItemAndContexts ++
            OrderedRemainItemAndContexts
    ;
        do_order_items(ItemAndContexts0, OrderedItemAndContextsTail),
        OrderedItemAndContexts = [ItemAndContext0 | OrderedItemAndContextsTail]
    ).

:- pred import_or_use(item_and_context::in) is semidet.

import_or_use(item_and_context(item_module_defn(_, md_import(_)), _)).
import_or_use(item_and_context(item_module_defn(_, md_use(_)), _)).

:- pred is_reorderable(item_and_context::in) is semidet.

is_reorderable(ItemAndContext) :-
    reorderable_item_and_context(ItemAndContext) = yes.

:- func reorderable_item_and_context(item_and_context) = bool.

reorderable_item_and_context(item_and_context(Item, _Context)) =
    reorderable_item(Item).

    % The kinds of items for which reorderable_item returns yes can be
    % arbitrarily reordered with respect to each other and with respect to
    % other chunkable items in all kinds of interface files (.int, .int2,
    % .int3, and .int0). This predicate is not relevant to .opt and
    % .trans_opt files, since those are generated from the HLDS, not
    % from item_lists.
    %
    % We should make this predicate call "unexpected" for items that should
    % never occur in interface files. However, I don't have a reliable list
    % of exactly which items those are.
    %
:- func reorderable_item(item) = bool.

reorderable_item(item_module_defn(_, ModuleDefn)) = Reorderable :-
    ( ModuleDefn = md_import(_), Reorderable = yes
    ; ModuleDefn = md_abstract_imported, Reorderable = no
    ; ModuleDefn = md_end_module(_), Reorderable = no
    ; ModuleDefn = md_export(_), Reorderable = yes
    ; ModuleDefn = md_external(_, _), Reorderable = yes
    ; ModuleDefn = md_implementation, Reorderable = no
    ; ModuleDefn = md_imported(_), Reorderable = no
    ; ModuleDefn = md_include_module(_), Reorderable = no
    ; ModuleDefn = md_interface, Reorderable = no
    ; ModuleDefn = md_module(_), Reorderable = no
    ; ModuleDefn = md_opt_imported, Reorderable = no
    ; ModuleDefn = md_private_interface, Reorderable = no
    ; ModuleDefn = md_transitively_imported, Reorderable = no
    ; ModuleDefn = md_use(_), Reorderable = yes
    ; ModuleDefn = md_used(_), Reorderable = no
    ; ModuleDefn = md_version_numbers(_, _), Reorderable = no
    ).
reorderable_item(item_pragma(_, Pragma)) = Reorderable :-
    ( Pragma = pragma_check_termination(_, _), Reorderable = yes
    ; Pragma = pragma_does_not_terminate(_, _), Reorderable = yes
    ; Pragma = pragma_exceptions(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_trailing_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_mm_tabling_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_foreign_export(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_fact_table(_, _, _), Reorderable = no
    ; Pragma = pragma_foreign_code(_, _), Reorderable = no
    ; Pragma = pragma_foreign_decl(_, _, _), Reorderable = no
    ; Pragma = pragma_foreign_import_module(_, _), Reorderable = no
    ; Pragma = pragma_foreign_proc(_, _, _, _, _, _, _), Reorderable = no
    ; Pragma = pragma_import(_, _, _, _, _), Reorderable = no
    ; Pragma = pragma_inline(_, _), Reorderable = yes
    ; Pragma = pragma_mode_check_clauses(_, _), Reorderable = yes
    ; Pragma = pragma_no_inline(_, _), Reorderable = yes
    ; Pragma = pragma_obsolete(_, _), Reorderable = yes
    ; Pragma = pragma_promise_pure(_, _), Reorderable = yes
    ; Pragma = pragma_promise_semipure(_, _), Reorderable = yes
    ; Pragma = pragma_promise_equivalent_clauses(_, _), Reorderable = yes
    ; Pragma = pragma_reserve_tag(_, _), Reorderable = yes
    ; Pragma = pragma_source_file(_), Reorderable = no
    ; Pragma = pragma_tabled(_, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_terminates(_, _), Reorderable = yes
    ; Pragma = pragma_termination2_info(_, _, _, _, _, _), Reorderable = no
    ; Pragma = pragma_termination_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_structure_sharing(_, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_structure_reuse(_, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_type_spec(_, _, _, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_unused_args(_, _, _, _, _), Reorderable = yes
    ).
reorderable_item(item_type_defn(_, _, _, _, _)) = yes.
reorderable_item(item_inst_defn(_, _, _, _, _)) = yes.
reorderable_item(item_mode_defn(_, _, _, _, _)) = yes.
reorderable_item(item_promise(_, _, _, _)) = yes.
reorderable_item(item_typeclass(_, _, _, _, _, _)) = yes.
reorderable_item(item_instance(_, _, _, _, _, _)) = yes.
reorderable_item(item_clause(_, _, _, _, _, _)) = no.
reorderable_item(item_nothing(_)) = no.
reorderable_item(item_pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _, _)) =
    no.
reorderable_item(item_pred_or_func_mode(_, _, _, _, _, _, _)) = no.
reorderable_item(item_initialise(_, _, _)) = no.
reorderable_item(item_finalise(_, _, _)) = no.
reorderable_item(item_mutable(_, _, _, _, _, _)) = no.

:- pred is_chunkable(item_and_context::in) is semidet.

is_chunkable(ItemAndContext) :-
    chunkable_item_and_context(ItemAndContext) = yes.

:- func chunkable_item_and_context(item_and_context) = bool.

chunkable_item_and_context(item_and_context(Item, _Context)) =
    chunkable_item(Item).

    % Given a list of items for which chunkable_item returns yes, we need
    % to keep the relative order of the non-reorderable items, but we can
    % move the reorderable items around arbitrarily.
    %
    % We should make this predicate call "unexpected" for items that should
    % never occur in interface files. However, I don't have a reliable list
    % of exactly which items those are.
    %
:- func chunkable_item(item) = bool.

chunkable_item(item_module_defn(_, ModuleDefn)) = Reorderable :-
    ( ModuleDefn = md_abstract_imported, Reorderable = no
    ; ModuleDefn = md_end_module(_), Reorderable = no
    ; ModuleDefn = md_export(_), Reorderable = yes
    ; ModuleDefn = md_external(_, _), Reorderable = yes
    ; ModuleDefn = md_implementation, Reorderable = no
    ; ModuleDefn = md_import(_), Reorderable = yes
    ; ModuleDefn = md_imported(_), Reorderable = no
    ; ModuleDefn = md_include_module(_), Reorderable = no
    ; ModuleDefn = md_interface, Reorderable = no
    ; ModuleDefn = md_module(_), Reorderable = no
    ; ModuleDefn = md_opt_imported, Reorderable = no
    ; ModuleDefn = md_private_interface, Reorderable = no
    ; ModuleDefn = md_transitively_imported, Reorderable = no
    ; ModuleDefn = md_use(_), Reorderable = yes
    ; ModuleDefn = md_used(_), Reorderable = no
    ; ModuleDefn = md_version_numbers(_, _), Reorderable = no
    ).
chunkable_item(item_pragma(_, Pragma)) = Reorderable :-
    ( Pragma = pragma_check_termination(_, _), Reorderable = yes
    ; Pragma = pragma_does_not_terminate(_, _), Reorderable = yes
    ; Pragma = pragma_exceptions(_, _, _, _, _), Reorderable = no
    ; Pragma = pragma_foreign_export(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_fact_table(_, _, _), Reorderable = no
    ; Pragma = pragma_foreign_code(_, _), Reorderable = no
    ; Pragma = pragma_foreign_decl(_, _, _), Reorderable = no
    ; Pragma = pragma_foreign_import_module(_, _), Reorderable = no
    ; Pragma = pragma_foreign_proc(_, _, _, _, _, _, _), Reorderable = no
    ; Pragma = pragma_import(_, _, _, _, _), Reorderable = no
    ; Pragma = pragma_inline(_, _), Reorderable = yes
    ; Pragma = pragma_mode_check_clauses(_, _), Reorderable = yes
    ; Pragma = pragma_no_inline(_, _), Reorderable = yes
    ; Pragma = pragma_obsolete(_, _), Reorderable = yes
    ; Pragma = pragma_promise_pure(_, _), Reorderable = yes
    ; Pragma = pragma_promise_semipure(_, _), Reorderable = yes
    ; Pragma = pragma_promise_equivalent_clauses(_, _), Reorderable = yes
    ; Pragma = pragma_reserve_tag(_, _), Reorderable = yes
    ; Pragma = pragma_source_file(_), Reorderable = no
    ; Pragma = pragma_tabled(_, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_terminates(_, _), Reorderable = yes
    ; Pragma = pragma_termination2_info( _, _, _, _, _, _), Reorderable = no
    ; Pragma = pragma_termination_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_structure_sharing(_, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_structure_reuse(_, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_trailing_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_mm_tabling_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_type_spec(_, _, _, _, _, _, _, _), Reorderable = yes
    ; Pragma = pragma_unused_args(_, _, _, _, _), Reorderable = yes
    ).
chunkable_item(item_type_defn(_, _, _, _, _)) = yes.
chunkable_item(item_inst_defn(_, _, _, _, _)) = yes.
chunkable_item(item_mode_defn(_, _, _, _, _)) = yes.
chunkable_item(item_pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _, _)) = yes.
chunkable_item(item_pred_or_func_mode(_, _, _, _, _, _, _)) = yes.
chunkable_item(item_promise(_, _, _, _)) = yes.
chunkable_item(item_typeclass(_, _, _, _, _, _)) = yes.
chunkable_item(item_instance(_, _, _, _, _, _)) = yes.
chunkable_item(item_clause(_, _, _, _, _, _)) = yes.
chunkable_item(item_initialise(_, _, _)) = yes.
chunkable_item(item_finalise(_, _, _)) = yes.
chunkable_item(item_mutable(_, _, _, _, _, _)) = no.
chunkable_item(item_nothing(_)) = yes.

    % Given a list of items for which symname_ordered succeeds, we need to keep
    % the relative order of the items with the same sym_name as returned by
    % symname_ordered, but the relative order of items with different sym_names
    % doesn't matter.
    %
:- pred symname_ordered(item_and_context::in, sym_name::out) is semidet.

symname_ordered(item_and_context(Item, _Context), Name) :-
    ( Item = item_pred_or_func(_, _, _, _, _, Name, _, _, _, _, _, _, _)
    ; Item = item_pred_or_func_mode(_, _, Name, _, _, _, _)
    ).

:- pred symname_orderable(item_and_context::in) is semidet.

symname_orderable(ItemAndContext) :-
    symname_ordered(ItemAndContext, _).

:- pred compare_by_symname(item_and_context::in, item_and_context::in,
    comparison_result::out) is det.

compare_by_symname(ItemAndContextA, ItemAndContextB, Result) :-
    (
        symname_ordered(ItemAndContextA, SymNameA),
        symname_ordered(ItemAndContextB, SymNameB)
    ->
        compare(Result, SymNameA, SymNameB)
    ;
        unexpected(this_file, "compare_by_symname: symname not found")
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_to_bool(maybe(T)::in, bool::out) is det.

maybe_to_bool(yes(_), yes).
maybe_to_bool(no, no).

:- pred maybe_record_timestamp(module_name::in, string::in, need_qualifier::in,
    maybe(timestamp)::in, module_imports::in, module_imports::out) is det.

maybe_record_timestamp(ModuleName, Suffix, NeedQualifier,
        MaybeTimestamp, !Module) :-
    (
        !.Module ^ maybe_timestamps = yes(Timestamps0),
        (
            MaybeTimestamp = yes(Timestamp),
            TimestampInfo = module_timestamp(Suffix, Timestamp, NeedQualifier),
            map.set(Timestamps0, ModuleName, TimestampInfo, Timestamps),
            !:Module = !.Module ^ maybe_timestamps := yes(Timestamps)
        ;
            MaybeTimestamp = no
        )
    ;
        !.Module ^ maybe_timestamps = no
    ).

:- pred report_modification_time_warning(file_name::in, io.error::in,
    io::di, io::uo) is det.

report_modification_time_warning(SourceFileName, Error, !IO) :-
    globals.io_set_option(smart_recompilation, bool(no), !IO),
    globals.io_set_option(generate_item_version_numbers, bool(no), !IO),
    globals.io_lookup_bool_option(warn_smart_recompilation, Warn, !IO),
    (
        Warn = yes,
        io.write_string("Warning: cannot find modification time for ", !IO),
        io.write_string(SourceFileName, !IO),
        io.write_string(":\n", !IO),
        io.error_message(Error, Msg),
        io.write_string("  ", !IO),
        io.write_string(Msg, !IO),
        io.write_string(".\n", !IO),
        io.write_string("  Smart recompilation will not work.\n", !IO),
        globals.io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
        (
            HaltAtWarn = yes,
            io.set_exit_status(1, !IO)
        ;
            HaltAtWarn = no
        )
    ;
        Warn = no
    ).

%-----------------------------------------------------------------------------%
%
% Java command-line utilities.
%

create_java_shell_script(MainModuleName, Succeeded, !IO) :-
    % XXX Extension should be ".bat" on Windows
    Extension = "",
    module_name_to_file_name(MainModuleName, Extension, no, FileName, !IO),

    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Generating shell script `" ++
        FileName ++ "'...\n", !IO),

    module_name_to_file_name(MainModuleName, ".class", no, ClassFileName, !IO),
    DirName = dir.dirname(ClassFileName),

    % XXX PathSeparator should be ";" on Windows
    PathSeparator = ":",
    globals.io_lookup_accumulating_option(java_classpath, Java_Incl_Dirs0,
        !IO),
    % We prepend the .class files' directory and the current CLASSPATH.
    Java_Incl_Dirs = [DirName, "$CLASSPATH" | Java_Incl_Dirs0],
    ClassPath = string.join_list(PathSeparator, Java_Incl_Dirs),

    globals.io_lookup_string_option(java_interpreter, Java, !IO),
    module_name_to_file_name(MainModuleName, "", no, Name_No_Extn, !IO),

    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(ShellScript),
        % XXX On Windows we should output a .bat file instead
        io.write_string(ShellScript, "#!/bin/sh\n", !IO),
        io.write_string(ShellScript, "CLASSPATH=" ++ ClassPath ++ " ", !IO),
        io.write_string(ShellScript, Java ++ " ", !IO),
        io.write_string(ShellScript, Name_No_Extn ++ "\n", !IO),
        io.close_output(ShellScript, !IO),
        io.call_system("chmod a+x " ++ FileName, ChmodResult, !IO),
        (
            ChmodResult = ok(Status),
            ( Status = 0 ->
                Succeeded = yes,
                maybe_write_string(Verbose, "% done.\n", !IO)
            ;
                unexpected(this_file, "chmod exit status != 0"),
                Succeeded = no
            )
        ;
            ChmodResult = error(Message),
            unexpected(this_file, io.error_message(Message)),
            Succeeded = no
        )
    ;
        OpenResult = error(Message),
        unexpected(this_file, io.error_message(Message)),
        Succeeded = no
    ).

list_class_files_for_jar(ModuleName, ClassFiles, ListClassFiles, !IO) :-
    globals.io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    AnySubdirs = UseSubdirs `or` UseGradeSubdirs,
    (
        AnySubdirs = yes,
        module_name_to_file_name(ModuleName, ".class", no, ClassFile, !IO),
        ClassSubdir = dir.dirname(ClassFile),
        % Here we use the `-C' option of jar to change directory during
        % execution, then use sed to strip away the Mercury/classs/
        % prefix to the class files.
        % Otherwise, the class files would be stored as
        %   Mercury/classs/*.class
        % within the jar file, which is not what we want.
        % XXX It would be nice to avoid this dependency on sed.
        ListClassFiles = "-C " ++ ClassSubdir ++ " \\\n" ++
            "\t\t`echo "" " ++ ClassFiles ++ """" ++
            " | sed 's| '" ++ ClassSubdir ++ "/| |'`"
    ;
        AnySubdirs = no,
        ListClassFiles = ClassFiles
    ).

get_env_classpath(Classpath, !IO) :-
    io.get_environment_var("CLASSPATH", MaybeCP, !IO),
    (
        MaybeCP = yes(Classpath)
    ;
        MaybeCP = no,
        io.get_environment_var("java.class.path", MaybeJCP, !IO),
        (
            MaybeJCP = yes(Classpath)
        ;
            MaybeJCP = no,
            Classpath = ""
        )
    ).

%-----------------------------------------------------------------------------%

% Changes to the following predicate may require similar changes to
% make.program_target.install_library_grade_files/9.

get_install_name_option(OutputFileName, InstallNameOpt, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_string_option(Globals, shlib_linker_install_name_flag,
        InstallNameFlag),
    globals.lookup_string_option(Globals, shlib_linker_install_name_path,
        InstallNamePath0),
    ( InstallNamePath0 = "" ->
        globals.lookup_string_option(Globals, install_prefix, InstallPrefix),
        grade_directory_component(Globals, GradeDir),
        InstallNamePath = InstallPrefix / "lib" / "mercury" / "lib" / GradeDir
    ;
        InstallNamePath = InstallNamePath0
    ),
    InstallNameOpt = InstallNameFlag ++ InstallNamePath / OutputFileName.

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "modules.m".

%-----------------------------------------------------------------------------%
:- end_module modules.
%-----------------------------------------------------------------------------%
