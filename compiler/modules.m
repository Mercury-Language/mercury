%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: modules.m
% main author: fjh

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

:- module parse_tree__modules.

:- interface.

:- import_module libs__globals.
:- import_module libs__timestamp.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_io.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%

    % Succeeds iff the module refered to by the module name is one
    % of the modules in the standard library.
    %
:- pred mercury_std_library_module_name(module_name::in) is semidet.

    % module_name_to_file_name(Module, Extension, Mkdir, FileName):
    %
    % Convert a module name and file extension to the corresponding file name.
    % If `MkDir' is yes, then create any directories needed.
    %
    % Currently we use the convention that the module `foo:bar:baz' should be
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

    % module_name_to_split_c_file_name(Module, Num, Extension, FileName):
    %
    % Like module_name_to_file_name, but also allows a sequence number.
    % The files produced by this predicate will all be in a subdirectory
    % DirName, which be obtained by calling
    % `module_name_to_file_name(Module, ".dir", DirName)'.
    % This predicate does not create that directory.
    %
    % This predicate is used for the names of .c and .$O files for
    % --split-c-files.
    %
:- pred module_name_to_split_c_file_name(module_name::in, int::in, string::in,
    file_name::out, io::di, io::uo) is det.

    % module_name_to_split_c_file_pattern(Module, Extension, FileName):
    % Like module_name_to_split_c_file_name, but generates a wildcard pattern
    % to match all such files with the given extension for the given module.
    %
:- pred module_name_to_split_c_file_pattern(module_name::in, string::in,
    file_name::out, io::di, io::uo) is det.

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
        source_file_name            :: file_name,
                                    % The source file

        source_file_module_name     :: module_name,
                                    % The name of the top-level module in
                                    % the source file containing the module
                                    % that we are compiling.

        module_name                 :: module_name,
                                    % The module (or sub-module)
                                    % that we are compiling.

        parent_deps                 :: list(module_name),
                                    % The list of ancestor modules it inherits

        int_deps                    :: list(module_name),
                                    % The list of modules it directly imports
                                    % in the interface
                                    % (imports via ancestors count as direct)

        impl_deps                   :: list(module_name),
                                    % The list of modules it directly imports
                                    % in the implementation.

        indirect_deps               :: list(module_name),
                                    % The list of modules it indirectly imports

        children                    :: list(module_name),

        public_children             :: list(module_name),
                                    % The list of its public children,
                                    % i.e. child modules that it includes
                                    % in the interface section.

        nested_children             :: list(module_name),
                                    % The modules included in the same source
                                    % file. This field is only set for the
                                    % top-level module in each file.

        fact_table_deps             :: list(string),
                                    % The list of filenames for fact tables
                                    % in this module.

        foreign_code                :: contains_foreign_code,
                                    % Whether or not the module contains
                                    % foreign code (and which languages
                                    % if it does)

        foreign_import_module_info  :: foreign_import_module_info,
                                    % The `:- pragma foreign_import_module'
                                    % declarations.

        contains_foreign_export     :: contains_foreign_export,
                                    % Does the module contain any
                                    % `:- pragma export' declarations.

        items                       :: item_list,
                                    % The contents of the module and
                                    % its imports

        error                       :: module_error,
                                    % Whether an error has been encountered
                                    % when reading in this module.

        maybe_timestamps            :: maybe(module_timestamps),
                                    % If we are doing smart recompilation,
                                    % we need to keep the timestamps of the
                                    % modules read in.

        has_main                    :: has_main,
                                    % Does this module contain main/2.

        module_dir                  :: dir_name
                                    % The directory containing the module
                                    % source.
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

    % Make an item_and_context for a module declaration
    % or pseudo-declaration such as `:- imported'
    % (which is inserted by the compiler, but can't be used
    % in user code).
    %
:- func make_pseudo_decl(module_defn) = item_and_context.

    % append_pseudo_decl(PseudoDecl, Module0, Module):
    % Append the specified module declaration to the list
    % of items in Module0 to give Module.
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
:- pred strip_imported_items(item_list::in, item_list::out) is det.

%-----------------------------------------------------------------------------%

    % Given a module (well, a list of items), split it into
    % its constituent sub-modules, in top-down order.
    % Also do some error checking:
    % - report an error if the `implementation' section of a sub-module
    %   is contained inside the `interface' section of its parent module
    % - check for modules declared as both nested and separate sub-modules.
    % - check for non-abstract typeclass instance declarations in module
    %   interfaces.
    %
:- type module_list == list(pair(module_name, item_list)).

:- pred split_into_submodules(module_name::in, item_list::in, module_list::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % grab_imported_modules(SourceFileName, SourceFileModuleName,
    %   ModuleName, NestedSubModules, ReadModules,
    %   ModuleTimestamp, Items, Module, Error)
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

    % process_module_short_interfaces_and_impls_transitively(
    %   ReadModules, IndirectImports, Ext,
    %   IntStatusItem, ImpStatusItem, !Module):
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

    % process_module_short_interfaces(ReadModules, IntStatusItem,
    %   ImpStatusItem, Modules, Ext, !IndirectImports,
    %   !ImpIndirectImports, !Module):
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

    % update_interface(FileName, Succeeded):
    %
    % Call the shell script mercury_update_interface to update the
    % interface file FileName if it has changed.
    %
:- pred update_interface(file_name::in, bool::out, io::di, io::uo) is det.

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
:- pred copy_file(file_name::in, file_name::in, io__res::out,
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

:- import_module libs__handle_options.
:- import_module libs__options.
:- import_module make.              % XXX undesirable dependency
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_mutable.
:- import_module parse_tree__prog_type.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__source_file_map.
:- import_module recompilation__version.

:- import_module assoc_list.
:- import_module char.
:- import_module dir.
:- import_module getopt_io.
:- import_module library.
:- import_module multi_map.
:- import_module relation.
:- import_module require.
:- import_module sparse_bitset.
:- import_module string.
:- import_module svmap.
:- import_module svrelation.
:- import_module svset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

mercury_std_library_module_name(unqualified(Name)) :-
    mercury_std_library_module(Name).
mercury_std_library_module_name(qualified(unqualified("mercury"), Name)) :-
    mercury_std_library_module(Name).

module_name_to_search_file_name(ModuleName, Ext, FileName, !IO) :-
    module_name_to_file_name(ModuleName, Ext, yes, no, FileName, !IO).

module_name_to_file_name(ModuleName, Ext, MkDir, FileName, !IO) :-
    module_name_to_file_name(ModuleName, Ext, no, MkDir, FileName, !IO).

:- pred module_name_to_file_name(module_name::in, string::in, bool::in,
    bool::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name(ModuleName, Ext, Search, MkDir, FileName, !IO) :-
    ( Ext = ".m" ->
        % Look up the module in the module->file mapping.
        source_file_map__lookup_module_source_file(ModuleName, FileName, !IO)
    ;
        sym_name_to_string(ModuleName, ".", BaseFileName),
        string__append(BaseFileName, Ext, BaseName),
        choose_file_name(ModuleName, BaseName, Ext, Search, MkDir, FileName,
            !IO)
    ).

module_name_to_lib_file_name(Prefix, ModuleName, Ext, MkDir, FileName, !IO) :-
    sym_name_to_string(ModuleName, ".", BaseFileName),
    string__append_list([Prefix, BaseFileName, Ext], BaseName),
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
    string__append(ExtraLinkObjName, Ext, BaseName),
    choose_file_name(ModuleName, BaseName, Ext, no, MkDir, FileName, !IO).

:- pred choose_file_name(module_name::in, string::in, string::in, bool::in,
    bool::in, file_name::out, io::di, io::uo) is det.

choose_file_name(_ModuleName, BaseName, Ext, Search, MkDir, FileName, !IO) :-
    globals__io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
    globals__io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    globals__io_get_globals(Globals, !IO),
    (
        (
            UseSubdirs = no
        ;
            %
            % If we're searching for (rather than writing)
            % a `.mih' file, use the plain file name.
            % This is so that searches for files in installed
            % libraries will work.  `--c-include-directory' is
            % set so that searches for files in the current
            % directory will work.
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
        % the source files, the final executables,
        % library files (including .init files)
        % output files intended for use by the user,
        % and phony Mmake targets names go in the current directory
        %
        \+ (
            UseGradeSubdirs = yes,
            file_is_arch_or_grade_dependent(Globals, Ext)
        ),
        (
            % executable files
            ( Ext = ""
            ; Ext = ".exe"
            ; Ext = ".split"
            ; Ext = ".split.exe"
            ; Ext = ".dll"
            % library files
            ; Ext = ".a"
            ; Ext = ".$A"
            ; Ext = ".so"
            ; Ext = ".dylib"
            ; Ext = ".$(EXT_FOR_SHARED_LIB)"
            ; Ext = ".jar"
            ; Ext = ".split.a"
            ; Ext = ".split.$A"
            ; Ext = ".split.so"
            ; Ext = ".split.dylib"
            ; Ext = ".split.$(EXT_FOR_SHARED_LIB)"
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
            ; Ext = ".h"    % `.h' files are being replaced with
                    % `.mh' files (for the
                    % `:- pragma export'  declarations),
                    % and `.mih' files (for the high-level
                    % C backend's function declarations).
                    % We still generate the `.h' files
                    % for bootstrapping (the trace
                    % directory refers to std_util.h
                    % and io.h).
            ; Ext = ".h.tmp"
            ; Ext = ".err"
            ; Ext = ".ugly"
            ; Ext = ".hlds_dump"
            ; Ext = ".mlds_dump"
            ; Ext = ".dependency_graph"
            ; Ext = ".order"
            ; Ext = ".rla"
            ; Ext = ".rl_dump"
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
            ; Ext = ".rlos"
            ; Ext = ".ss"
            ; Ext = ".pic_ss"
            ; Ext = ".ils"
            ; Ext = ".javas"
            ; Ext = ".classes"
            ; Ext = ".opts"
            ; Ext = ".trans_opts"
            % The following files are only used by the Aditi
            % query shell which doesn't know about --use-subdirs.
            ; Ext = ".base_schema"
            ; Ext = ".derived_schema"
            ; Ext = ".rlo"
            )
        ;
            % output files intended for use by the user
            ( string__prefix(Ext, ".c_dump")
            ; string__prefix(Ext, ".mih_dump")
            )
        )
    ->
        FileName = BaseName
    ;
        %
        % we need to handle a few cases specially
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
            string__append("_init.", ExtName, Ext)
        ->
            string__append(ExtName, "s", SubDirName)
        ;
            % .int.tmp, .opt.tmp, etc. files
            % need to go in the ints, opts, etc. subdirectories
            string__append(".", ExtName0, Ext),
            string__remove_suffix(ExtName0, ".tmp", ExtName)
        ->
            string__append(ExtName, "s", SubDirName)
        ;
            % `.dv' files go in the `deps' subdirectory,
            % along with the `.dep' files
            Ext = ".dv"
        ->
            SubDirName = "deps"
        ;
            % the usual case: `*.foo' files go in the `foos'
            % subdirectory
            string__append(".", ExtName, Ext)
        ->
            string__append(ExtName, "s", SubDirName)
        ;
            Ext = ""
        ->
            SubDirName = "bin"
        ;
            string__append_list(["unknown extension `", Ext, "'"], ErrorMsg),
            error(ErrorMsg)
        ),
        make_file_name(SubDirName, Search, MkDir, BaseName, Ext, FileName, !IO)
    ).

module_name_to_split_c_file_name(ModuleName, Num, Ext, FileName, !IO) :-
    module_name_to_file_name(ModuleName, ".dir", no, DirName, !IO),
    unqualify_name(ModuleName, BaseFileName),
    dir__directory_separator(Slash),
    string__format("%s%c%s_%03d%s",
        [s(DirName), c(Slash), s(BaseFileName), i(Num), s(Ext)],
        FileName).

module_name_to_split_c_file_pattern(ModuleName, Ext, Pattern, !IO) :-
    module_name_to_file_name(ModuleName, ".dir", no, DirName, !IO),
    dir__directory_separator(Slash),
    string__format("%s%c*%s", [s(DirName), c(Slash), s(Ext)], Pattern).

file_name_to_module_name(FileName, ModuleName) :-
    string_to_sym_name(FileName, ".", ModuleName).

module_name_to_file_name(ModuleName, FileName) :-
    sym_name_to_string(ModuleName, ".", FileName).

module_name_to_make_var_name(ModuleName, MakeVarName) :-
    sym_name_to_string(ModuleName, ".", MakeVarName).

maybe_make_symlink(LinkTarget, LinkName, Result, !IO) :-
    globals__io_lookup_bool_option(use_symlinks, UseSymLinks, !IO),
    (
        UseSymLinks = yes,
        io__remove_file(LinkName, _, !IO),
        io__make_symlink(LinkTarget, LinkName, LinkResult, !IO),
        Result = ( if LinkResult = ok then yes else no )
    ;
        UseSymLinks = no,
        Result = no
    ).

copy_file(Source, Destination, Res, !IO) :-
    io__open_binary_input(Source, SourceRes, !IO),
    (
        SourceRes = ok(InputStream),
        io__open_binary_output(Destination, DestRes, !IO),
        (
            DestRes = ok(OutputStream),
            % XXX Depending on file size it may be
            % faster to call the system's cp command.
            WriteByte = io__write_byte(OutputStream),
            io__binary_input_stream_foldl_io(InputStream, WriteByte, Res, !IO),
            io__close_binary_input(InputStream, !IO),
            io__close_binary_output(OutputStream, !IO)
        ;
            DestRes = error(Error),
            Res = error(Error)
        )
    ;
        SourceRes = error(Error),
        Res = error(Error)
    ).

make_symlink_or_copy_file(SourceFileName, DestinationFileName, Succeeded,
        !IO) :-
    globals__io_lookup_bool_option(use_symlinks, UseSymLinks, !IO),
    (
        UseSymLinks = yes,
        LinkOrCopy = "linking",
        io__make_symlink(SourceFileName, DestinationFileName, Result, !IO)
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
        io__progname_base("mercury_compile", ProgName, !IO),
        io__write_string(ProgName, !IO),
        io__write_string(": error ", !IO),
        io__write_string(LinkOrCopy, !IO),
        io__write_string(" `", !IO),
        io__write_string(SourceFileName, !IO),
        io__write_string("' to `", !IO),
        io__write_string(DestinationFileName, !IO),
        io__write_string("': ", !IO),
        io__write_string(io__error_message(Error), !IO)
    ).

:- pred make_file_name(dir_name::in, bool::in, bool::in, file_name::in,
    string::in, file_name::out, io::di, io::uo) is det.

make_file_name(SubDirName, Search, MkDir, BaseName, Ext, FileName, !IO) :-
    globals__io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    globals__io_lookup_string_option(fullarch, FullArch, !IO),
    globals__io_get_globals(Globals, !IO),
    (
        UseGradeSubdirs = yes,
        file_is_arch_or_grade_dependent(Globals, Ext),

        %
        % If we're searching for (rather than writing) the file,
        % just search in Mercury/<ext>s. This is so that searches
        % for files in installed libraries work.
        % `--intermod-directories' is set so this will work.
        %
        \+ (
            Search = yes,
            ( Ext = ".opt"
            ; Ext = ".trans_opt"
            )
        )
    ->
        grade_directory_component(Globals, Grade),

        % The extra "Mercury" is needed so we can use
        % `--intermod-directory Mercury/<grade>/<fullarch>' and
        % `--c-include Mercury/<grade>/<fullarch>' to find
        % the local `.opt' and `.mih' files without messing
        % up the search for the files for installed libraries.
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
    string__append(Ext, ".tmp", Ext0), % for mercury_update_interface.
    file_is_arch_or_grade_dependent(Globals, Ext).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    globals__lookup_string_option(Globals, executable_file_extension, Ext).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    (
        globals__lookup_string_option(Globals,
            object_file_extension, ObjExt)
    ;
        globals__lookup_string_option(Globals,
            pic_object_file_extension, ObjExt)
    ;
        globals__lookup_string_option(Globals,
            link_with_pic_object_file_extension, ObjExt)
    ),
    ( Ext = ObjExt
    ; Ext = "_init" ++ ObjExt
    ).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    globals__lookup_string_option(Globals, library_extension, LibExt),
    ( Ext = LibExt
    ; Ext = ".split" ++ LibExt
    ).
file_is_arch_or_grade_dependent(Globals, Ext) :-
    globals__lookup_string_option(Globals, shared_library_extension, Ext).

:- pred file_is_arch_or_grade_dependent_2(string::in) is semidet.

    % The `.used' file isn't grade dependent itself, but it contains
    % information collected while compiling a grade-dependent
    % `.c', `il', etc file.
file_is_arch_or_grade_dependent_2(".used").
file_is_arch_or_grade_dependent_2(".opt").
file_is_arch_or_grade_dependent_2(".optdate").
file_is_arch_or_grade_dependent_2(".trans_opt").
file_is_arch_or_grade_dependent_2(".trans_opt_date").
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
file_is_arch_or_grade_dependent_2(".num_split").
file_is_arch_or_grade_dependent_2(".dll").
file_is_arch_or_grade_dependent_2(".$A").
file_is_arch_or_grade_dependent_2(".a").
file_is_arch_or_grade_dependent_2(".split.$A").
file_is_arch_or_grade_dependent_2(".split.a").
file_is_arch_or_grade_dependent_2(".split").
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
        %
        % Check whether we succeeded
        %
    % XXX zs: why does this code not check for fatal_module_errors?
    ( Error = some_module_errors ->
        module_name_to_file_name(ModuleName, ".int0", no, FileName, !IO),
        io__write_strings(["Error reading interface files.\n",
            "`", FileName, "' not written.\n"], !IO)
    ;
            %
            % Module-qualify all items.
            %
        module_imports_get_items(Module, Items1),
        module_qual__module_qualify_items(Items1, Items2, ModuleName, yes,
            _, _, _, _, !IO),
        io__get_exit_status(Status, !IO),
        ( Status \= 0 ->
            module_name_to_file_name(ModuleName, ".int0", no, FileName, !IO),
            io__write_strings(["`", FileName, "' not written.\n"], !IO)
        ;
            %
            % Write out the `.int0' file.
            %
            % XXX The following sequence of operations relies
            % on the fact that any reversals done while processing
            % it are undone by subsequent operations.  Also, we
            % should sort the contents of the .int0 file as we
            % do for the other types of interface file.  We don't
            % do that at the moment because the code for doing
            % that cannot handle the structure of lists of items
            % that represent private interfaces.
            %
            strip_imported_items(Items2, [], Items3),
            strip_clauses_from_interface(Items3, Items4),
            handle_mutables_in_private_interface(ModuleName,
                Items4, Items5),
            MakeAbs = (pred(Item0::in, Item::out) is det :-
                Item0 = Item1 - Context,
                ( make_abstract_instance(Item1, Item2) ->
                    Item = Item2 - Context
                ;
                    Item = Item0
                )
            ),
            list.map(MakeAbs, Items5, Items6),
            list.reverse(Items6, Items),
            write_interface_file(SourceFileName, ModuleName,
                ".int0", MaybeTimestamp,
                [make_pseudo_decl(interface) | Items], !IO),
            touch_interface_datestamp(ModuleName, ".date0", !IO)
        )
    ).

    % Expand any mutable declarations in the item list into the type and mode
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

handle_mutable_in_private_interface(ModuleName, Item - Context, !Items) :-
    ( Item = mutable(MutableName, Type, _Value, Inst, _Attrs) ->
        GetPredDecl = prog_mutable.get_pred_decl(ModuleName, MutableName,
            Type, Inst),
        list.cons(GetPredDecl - Context, !Items),
        SetPredDecl = prog_mutable.set_pred_decl(ModuleName, MutableName,
            Type, Inst),
        list.cons(SetPredDecl - Context, !Items)
    ;
        list.cons(Item - Context, !Items) 
    ).

%-----------------------------------------------------------------------------%

    % Read in the .int3 files that the current module depends on,
    % and use these to qualify all items in the interface as much as
    % possible. Then write out the .int and .int2 files.
    %
make_interface(SourceFileName, SourceFileModuleName, ModuleName,
        MaybeTimestamp, Items0, !IO) :-
    some [!InterfaceItems] (
        get_interface(ModuleName, yes, Items0, !:InterfaceItems),
            %
            % Get the .int3 files for imported modules
            %
        grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
            ModuleName, !.InterfaceItems, Module0, Error, !IO),

            %
            % Check whether we succeeded
            %
        module_imports_get_items(Module0, !:InterfaceItems),
        % XXX zs: why does this code not check for fatal_module_errors?
        ( Error = some_module_errors ->
            module_name_to_file_name(ModuleName, ".int", no, IntFileName, !IO),
            module_name_to_file_name(ModuleName, ".int2", no, Int2FileName,
                !IO),
            io__write_strings(["Error reading short interface files.\n",
                "`", IntFileName, "' and ",
                "`", Int2FileName, "' not written.\n"], !IO)
        ;
                %
                % Module-qualify all items.
                %
            module_qual__module_qualify_items(!InterfaceItems, ModuleName, yes,
                _, _, _, _, !IO),
            io__get_exit_status(Status, !IO),
            ( Status \= 0 ->
                module_name_to_file_name(ModuleName, ".int", no, IntFileName,
                    !IO),
                io__write_strings(["`", IntFileName, "' ", "not written.\n"],
                    !IO)
            ;
                %
                % Strip out the imported interfaces,
                % assertions are also stripped since they should
                % only be written to .opt files,
                % check for some warnings, and then
                % write out the `.int' and `int2' files
                % and touch the `.date' file.
                %
                strip_imported_items(!.InterfaceItems, [], !:InterfaceItems),
                strip_assertions(!InterfaceItems),
                strip_unnecessary_impl_defns(!InterfaceItems),
                check_for_clauses_in_interface(!InterfaceItems, !IO),
                check_int_for_no_exports(!.InterfaceItems, ModuleName, !IO),
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

    % This qualifies everything as much as it can given the
    % information in the current module and writes out the .int3 file.
make_short_interface(SourceFileName, ModuleName, Items0, !IO) :-
    get_interface(ModuleName, no, Items0, InterfaceItems0),
        % assertions are also stripped since they should
        % only be written to .opt files,
    strip_assertions(InterfaceItems0, InterfaceItems1),
    check_for_clauses_in_interface(InterfaceItems1, InterfaceItems, !IO),
    get_short_interface(InterfaceItems, int3, ShortInterfaceItems0),
    module_qual__module_qualify_items(ShortInterfaceItems0,
        ShortInterfaceItems, ModuleName, no, _, _, _, _, !IO),
    write_interface_file(SourceFileName, ModuleName, ".int3",
        no, ShortInterfaceItems, !IO),
    touch_interface_datestamp(ModuleName, ".date3", !IO).

%-----------------------------------------------------------------------------%

strip_imported_items(Items0, Items) :-
    strip_imported_items(Items0, [], Items).

:- pred strip_imported_items(item_list::in, item_list::in, item_list::out)
    is det.

strip_imported_items([], !Items) :-
    list__reverse(!Items).
strip_imported_items([Item - Context | Rest], !Items) :-
    ( Item = module_defn(_, imported(_)) ->
        list__reverse(!Items)
    ; Item = module_defn(_, used(_)) ->
        list__reverse(!Items)
    ; Item = module_defn(_, abstract_imported) ->
        list__reverse(!Items)
    ;
        strip_imported_items(Rest, [Item - Context | !.Items], !:Items)
    ).

:- pred strip_assertions(item_list::in, item_list::out) is det.

strip_assertions([], []).
strip_assertions([Item - Context | Rest], Items) :-
    ( Item = promise(true, _, _, _) ->
        strip_assertions(Rest, Items)
    ;
        strip_assertions(Rest, Items0),
        Items = [Item - Context | Items0]
    ).

%-----------------------------------------------------------------------------%

:- pred strip_unnecessary_impl_defns(item_list::in, item_list::out) is det.

strip_unnecessary_impl_defns(Items0, Items) :-
    % strip_unnecessary_impl_defns_2 is cc_multi because of the call
    % to std_util.unsorted_aggregate. The order in which items are deleted
    % from a multi_map does not matter.
    promise_equivalent_solutions [Items] (
        strip_unnecessary_impl_defns_2(Items0, Items)
    ).

:- pred strip_unnecessary_impl_defns_2(item_list::in, item_list::out)
    is cc_multi.

strip_unnecessary_impl_defns_2(Items0, Items) :-
    some [!IntTypesMap, !ImplTypesMap, !ImplItems] (
        gather_type_defns(no, Items0, [], IntItems0, [], !:ImplItems,
            map__init, !:IntTypesMap, map__init, !:ImplTypesMap),

        % Work out which module imports in the implementation section of
        % the interface are required by the definitions of equivalence
        % types in the implementation.
        get_requirements_of_eqv_types(!.IntTypesMap, !.ImplTypesMap,
            NecessaryTypeCtors, NecessaryImplImports),

        % If a type in the implementation section doesn't have
        % foreign type alternatives, make it abstract.
        map__map_values(make_impl_type_abstract, !ImplTypesMap),

        % If there is an exported type declaration for a type with an abstract
        % declaration in the implementation (usually it will originally
        % have been a d.u. type), remove the declaration in the implementation.
        unsorted_aggregate(
            (pred(TypeCtor::out) is nondet :-
                map__member(!.ImplTypesMap, TypeCtor, Defns),
                \+ (
                    list__member(Defn, Defns),
                    Defn \= abstract_type(_) - _
                ),
                multi_map__contains(!.IntTypesMap, TypeCtor)
            ),
            (pred(TypeCtor::in, !.ImplTypesMap::in, !:ImplTypesMap::out)
                    is det :-
                multi_map__delete(!.ImplTypesMap, TypeCtor, !:ImplTypesMap)
            ),
            !ImplTypesMap),

        map__foldl(
            (pred(_::in, Defns::in, !.ImplItems::in, !:ImplItems::out)
                    is det :-
                list__foldl(
                    (pred((_ - Item)::in, !.ImplItems::in, !:ImplItems::out)
                            is det :-
                        !:ImplItems = [Item | !.ImplItems]
                    ), Defns, !ImplItems)
            ), !.ImplTypesMap, !ImplItems),

        IntItems = [make_pseudo_decl(interface) | IntItems0],

        maybe_strip_import_decls(!ImplItems),
        strip_unnecessary_impl_imports(NecessaryImplImports, !ImplItems),
        strip_unnecessary_impl_types(NecessaryTypeCtors, !ImplItems),
        (
            !.ImplItems = [],
            Items = IntItems
        ;
            !.ImplItems = [_ | _],
            standardize_impl_items(!.ImplItems, no, Unexpected,
                [], RevRemainderItems, [], ImportItems, [], UseItems,
                [], TypeDefnItems),
            (
                Unexpected = yes,
                error("strip_unnecessary_impl_defns_2: " ++
                    "unexpected items in implementation section")
                % XXX If the above exception is thrown and you need a
                % workaround you, can replace the exception with this code:
                % Items = IntItems ++ [make_pseudo_decl(implementation)]
                %    ++ !.ImplItems
            ;
                Unexpected = no,
                list.reverse(RevRemainderItems, RemainderItems),
                list.condense([IntItems, [make_pseudo_decl(implementation)],
                    ImportItems, UseItems, TypeDefnItems, RemainderItems],
                    Items)
            )
        )
    ).

:- inst item_context(I) == bound(I - ground).

:- inst one_module_spec ==  bound(module(bound([ground | bound([])]))).
:- inst import_item     ==  bound(module_defn(ground,
                                bound(import(one_module_spec)))).
:- inst use_item        ==  bound(module_defn(ground,
                                bound(use(one_module_spec)))).
:- inst type_defn_item  ==  bound(type_defn(ground, ground, ground, ground,
                                ground)).

:- pred standardize_impl_items(item_list::in, bool::in, bool::out,
    item_list::in, item_list::out,
    item_list::in(list_skel(item_context(import_item))),
    item_list::out(list_skel(item_context(import_item))),
    item_list::in(list_skel(item_context(use_item))),
    item_list::out(list_skel(item_context(use_item))),
    item_list::in(list_skel(item_context(type_defn_item))),
    item_list::out(list_skel(item_context(type_defn_item))))
    is det.

standardize_impl_items([], !Unexpected, !RevRemainderItems,
        !ImportItems, !UseItems, !TypeDefnItems).
standardize_impl_items([ItemAndContext | ItemAndContexts], !Unexpected,
        !RevRemainderItems, !ImportItems, !UseItems, !TypeDefnItems) :-
    ItemAndContext = Item - Context,
    ( Item = module_defn(_VarSet, ModuleDefn) ->
        (
            ModuleDefn = import(ImportModules),
            ( ImportModules = module([_ImportModule]) ->
                insert_import_module(Context, Item, !ImportItems)
            ;
                unexpected(this_file, "standardize_impl_items: " ++
                    "non-singleton-module import")
            )
        ;
            ModuleDefn = use(UseModules),
            ( UseModules = module([_UseModule]) ->
                insert_use_module(Context, Item, !UseItems)
            ;
                unexpected(this_file, "standardize_impl_items: " ++
                    "non-singleton-module use")
            )
        ;
            ModuleDefn = module(_),
            !:Unexpected = yes
        ;
            ModuleDefn = end_module(_),
            !:Unexpected = yes
        ;
            ModuleDefn = imported(_),
            !:Unexpected = yes
        ;
            ModuleDefn = used(_),
            !:Unexpected = yes
        ;
            ModuleDefn = abstract_imported,
            !:Unexpected = yes
        ;
            ModuleDefn = opt_imported,
            !:Unexpected = yes
        ;
            ModuleDefn = transitively_imported,
            !:Unexpected = yes
        ;
            ModuleDefn = external(_, _),
            !:Unexpected = yes
        ;
            ModuleDefn = export(_),
            !:Unexpected = yes
        ;
            ModuleDefn = include_module(_),
            !:RevRemainderItems = [ItemAndContext | !.RevRemainderItems]
        ;
            ModuleDefn = interface,
            !:Unexpected = yes
        ;
            ModuleDefn = implementation,
            !:Unexpected = yes
        ;
            ModuleDefn = private_interface,
            !:Unexpected = yes
        ;
            ModuleDefn = version_numbers(_, _),
            !:Unexpected = yes
        )
    ; Item = type_defn(_, _, _, _, _) ->
        insert_type_defn(Context, Item, !TypeDefnItems)
    ;
        !:RevRemainderItems = [ItemAndContext | !.RevRemainderItems]
    ),
    standardize_impl_items(ItemAndContexts, !Unexpected,
        !RevRemainderItems, !ImportItems, !UseItems, !TypeDefnItems).

:- pred insert_import_module(prog_context::in, item::in,
    item_list::in(list_skel(item_context(import_item))),
    item_list::out(list_skel(item_context(import_item)))) is det.

insert_import_module(Context, Item, [], [Item - Context]).
insert_import_module(Context, Item, [Head | Tail], Result) :-
    Head = HeadItem - _HeadContext,
    % The lack of alias tracking prevents the compiler from figuring out
    % that this predicate is only called with values of Item for which
    % this test succeeds.
    ( Item = module_defn(_, import(module([ModulePrime]))) ->
        Module = ModulePrime
    ;
        unexpected(this_file, "insert_import_module: bad item")
    ),
    HeadItem = module_defn(_, import(module([HeadModule]))),
    compare(CompareSymName, Module, HeadModule),
    (
        CompareSymName = (<)
    ->
        Result = [Item - Context, Head | Tail]
    ;
        insert_import_module(Context, Item, Tail, TailResult),
        Result = [Head | TailResult]
    ).

:- pred insert_use_module(prog_context::in, item::in,
    item_list::in(list_skel(item_context(use_item))),
    item_list::out(list_skel(item_context(use_item)))) is det.

insert_use_module(Context, Item, [], [Item - Context]).
insert_use_module(Context, Item, [Head | Tail], Result) :-
    Head = HeadItem - _HeadContext,
    % The lack of alias tracking prevents the compiler from figuring out
    % that this predicate is only called with values of Item for which
    % this test succeeds.
    ( Item = module_defn(_, use(module([ModulePrime]))) ->
        Module = ModulePrime
    ;
        unexpected(this_file, "insert_import_module: bad item")
    ),
    HeadItem = module_defn(_, use(module([HeadModule]))),
    compare(CompareSymName, Module, HeadModule),
    (
        CompareSymName = (<)
    ->
        Result = [Item - Context, Head | Tail]
    ;
        insert_use_module(Context, Item, Tail, TailResult),
        Result = [Head | TailResult]
    ).

:- pred insert_type_defn(prog_context::in, item::in(type_defn_item),
    item_list::in(list_skel(item_context(type_defn_item))),
    item_list::out(list_skel(item_context(type_defn_item)))) is det.

insert_type_defn(Context, Item, [], [Item - Context]).
insert_type_defn(Context, Item, [Head | Tail], Result) :-
    Head = HeadItem - _HeadContext,
    Item = type_defn(_, SymName, Params, _, _),
    HeadItem = type_defn(_, HeadSymName, HeadParams, _, _),
    compare(CompareSymName, SymName, HeadSymName),
    (
        (
            CompareSymName = (<)
        ;
            CompareSymName = (=),
            list__length(Params, ParamsLength),
            list__length(HeadParams, HeadParamsLength),
            compare(Compare, ParamsLength, HeadParamsLength),
            Compare = (<)
        )
    ->
        Result = [Item - Context, Head | Tail]
    ;
        insert_type_defn(Context, Item, Tail, TailResult),
        Result = [Head | TailResult]
    ).

:- pred make_impl_type_abstract(type_ctor::in,
    assoc_list(type_defn, item_and_context)::in,
    assoc_list(type_defn, item_and_context)::out) is det.

make_impl_type_abstract(_TypeCtor, !TypeDefnPairs) :-
    (
        !.TypeDefnPairs = [du_type(_, _) - (Item0 - Context)]
    ->
        Defn = abstract_type(non_solver_type),
        ( Item = Item0 ^ td_ctor_defn := Defn ->
            !:TypeDefnPairs = [Defn - (Item - Context)]
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
    ItemAndContext = Item - _,
    ( Item = module_defn(_, Defn) ->
        (
            ( Defn = use(Module)
            ; Defn = import(Module)
            )
        ->
            ( Module = module([ModuleName]) ->
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
    ItemAndContext = Item - _,
    ( Item = type_defn(_, SymName, Params, _, _) ->
        TypeCtor = SymName - list.length(Params),
        set.member(TypeCtor, NecessaryTypeCtors)
    ;
        true
    ).

    % get_requirements_of_eqv_types(InterfaceTypeMap, ImplTypeMap,
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
:- pred get_requirements_of_eqv_types(type_defn_map::in, type_defn_map::in,
    set(type_ctor)::out, set(module_name)::out) is det.

get_requirements_of_eqv_types(InterfaceTypeMap, ImplTypeMap,
        NecessaryTypeCtors, Modules) :-
    multi_map.to_flat_assoc_list(ImplTypeMap, ImplTypes),
    list.foldl(accumulate_abs_eqv_type_lhs(InterfaceTypeMap), ImplTypes,
        set.init, AbsEqvLhsTypeCtors),
    set.fold2(accumulate_abs_eqv_type_rhs(ImplTypeMap), AbsEqvLhsTypeCtors,
        set.init, AbsEqvRhsTypeCtors, set.init, Modules),
    set.union(AbsEqvLhsTypeCtors, AbsEqvRhsTypeCtors, NecessaryTypeCtors).

:- pred accumulate_abs_eqv_type_lhs(type_defn_map::in,
    pair(type_ctor, pair(type_defn, item_and_context))::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_eqv_type_lhs(InterfaceTypeMap,
        TypeCtor - (TypeDefn - _ItemAndContext), !AbsEqvLhsTypeCtors) :-
    %
    % A type may have multiple definitions because it may be defined both
    % as a foreign type and as a Mercury type. We grab any equivalence types
    % that are in there.
    %
    (
        TypeDefn = eqv_type(_RhsType),
        map__search(InterfaceTypeMap, TypeCtor, _)
    ->
        svset.insert(TypeCtor, !AbsEqvLhsTypeCtors)
    ;
        TypeDefn = foreign_type(_, _, _),
        map__search(InterfaceTypeMap, TypeCtor, _)
    ->
        svset.insert(TypeCtor, !AbsEqvLhsTypeCtors)
    ;
        true
    ).

:- pred accumulate_abs_eqv_type_rhs(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs(ImplTypeMap, TypeCtor, !AbsEqvRhsTypeCtors,
        !Modules) :-
    (
        map.search(ImplTypeMap, TypeCtor, TypeDefns)
    ->
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
    (
        TypeDefn = eqv_type(RhsType)
    ->
        type_to_type_ctor_set(RhsType, set.init, RhsTypeCtors),
        set.difference(RhsTypeCtors, !.AbsEqvRhsTypeCtors, NewRhsTypeCtors),
        set.fold(accumulate_modules, NewRhsTypeCtors, !Modules),
        set.union(NewRhsTypeCtors, !AbsEqvRhsTypeCtors),
        set.fold2(accumulate_abs_eqv_type_rhs(ImplTypeMap), NewRhsTypeCtors,
            !AbsEqvRhsTypeCtors, !Modules)
    ;
        true
    ).

:- pred accumulate_modules(type_ctor::in, set(module_name)::in,
    set(module_name)::out) is det.

accumulate_modules(TypeCtor, !Modules) :-
    % NOTE: This assumes that everything has been module qualified.
    TypeCtor = SymName - _Arity,
    (
        sym_name_get_module_name(SymName, ModuleName)
    ->
        svset.insert(ModuleName, !Modules)
    ;
        unexpected(this_file, "accumulate_modules/3: unknown type encountered")
    ).

    % Given a type, return the set of user-defined type constructors
    % occurring in it.
    %
:- pred type_to_type_ctor_set((type)::in, set(type_ctor)::in,
    set(type_ctor)::out) is det.

type_to_type_ctor_set(Type, !TypeCtors) :-
    ( type_to_ctor_and_args(Type, TypeCtor, Args) ->
        TypeCtor = SymName - _Arity,
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
gather_type_defns(InInterface0, [Item - Context | Items0],
        !IntItems, !ImplItems, !IntTypes, !ImplTypes) :-
    ( Item = module_defn(_, interface) ->
        InInterface = yes
    ; Item = module_defn(_, implementation) ->
        InInterface = no
    ; Item = type_defn(_, Name, Args, Body, _) ->
        TypeCtor = Name - length(Args),
        InInterface = InInterface0,
        (
            InInterface = yes,
            !:IntItems = [Item - Context | !.IntItems],
            gather_type_defn(TypeCtor, Body, Item - Context, !IntTypes)
        ;
            InInterface = no,
            % We don't add this to !ImplItems yet --
            % we may be removing this item.
            gather_type_defn(TypeCtor, Body, Item - Context, !ImplTypes)
        )
    ;
        InInterface = InInterface0,
        (
            InInterface = yes,
            !:IntItems = [Item - Context | !.IntItems]
        ;
            InInterface = no,
            !:ImplItems = [Item - Context | !.ImplItems]
        )
    ),
    gather_type_defns(InInterface, Items0, !IntItems, !ImplItems,
        !IntTypes, !ImplTypes).

:- pred gather_type_defn(type_ctor::in, type_defn::in, item_and_context::in,
    type_defn_map::in, type_defn_map::out) is det.

gather_type_defn(TypeCtor, Body, Item, DefnMap0, DefnMap) :-
    multi_map__set(DefnMap0, TypeCtor, Body - Item, DefnMap).

%-----------------------------------------------------------------------------%

:- pred check_for_clauses_in_interface(item_list::in, item_list::out,
    io::di, io::uo) is det.

check_for_clauses_in_interface([], [], !IO).
check_for_clauses_in_interface([ItemAndContext0 | Items0], Items, !IO) :-
    ItemAndContext0 = Item0 - Context,
    (
        Item0 = clause(_, _, _, _, _, _)
    ->
        prog_out__write_context(Context, !IO),
        report_warning("Warning: clause in module interface.\n", !IO),
        check_for_clauses_in_interface(Items0, Items, !IO)
    ;
        Item0 = pragma(_, Pragma),
        pragma_allowed_in_interface(Pragma, no)
    ->
        prog_out__write_context(Context, !IO),
        report_warning("Warning: pragma in module interface.\n", !IO),
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
    ItemAndContext0 = Item0 - _Context,
    (
        ( Item0 = module_defn(_, interface)
        ; Item0 = module_defn(_, implementation)
        )
    ->
        split_clauses_and_decls(Items0, ClauseItems, InterfaceItems)
    ;
        (
            Item0 = clause(_,_,_,_,_,_)
        ;
            Item0 = pragma(_, Pragma),
            pragma_allowed_in_interface(Pragma, no)
        ;
            Item0 = initialise(_, _, _)
        ;
            Item0 = finalise(_, _, _)
        )
     ->
         split_clauses_and_decls(Items0, ClauseItems1, InterfaceItems),
         ClauseItems = [ItemAndContext0 | ClauseItems1]
    ;
        split_clauses_and_decls(Items0, ClauseItems, InterfaceItems1),
        InterfaceItems = [ItemAndContext0 | InterfaceItems1]
    ).

% pragma `obsolete', `terminates', `does_not_terminate'
% `termination_info', `check_termination', `aditi', `base_relation',
% `owner', and `reserve_tag' pragma declarations are supposed to go
% in the interface, but all other pragma declarations are implementation
% details only, and should go in the implementation.

% XXX we should allow c_header_code;
% but if we do allow it, we should put it in the generated
% header file, which currently we don't.

pragma_allowed_in_interface(foreign_decl(_, _, _), no).
pragma_allowed_in_interface(foreign_import_module(_, _), yes).
pragma_allowed_in_interface(foreign_code(_, _), no).
pragma_allowed_in_interface(foreign_proc(_, _, _, _, _, _), no).
pragma_allowed_in_interface(inline(_, _), no).
pragma_allowed_in_interface(no_inline(_, _), no).
pragma_allowed_in_interface(obsolete(_, _), yes).
pragma_allowed_in_interface(export(_, _, _, _), no).
pragma_allowed_in_interface(import(_, _, _, _, _), no).
pragma_allowed_in_interface(source_file(_), yes).
    % yes, but the parser will strip out `source_file' pragmas anyway...
pragma_allowed_in_interface(fact_table(_, _, _), no).
pragma_allowed_in_interface(tabled(_, _, _, _, _), no).
    % `reserve_tag' must be in the interface iff the corresponding
    % type definition is in the interface. This is checked in make_hlds.m.
pragma_allowed_in_interface(reserve_tag(_, _), yes).
pragma_allowed_in_interface(promise_pure(_, _), no).
pragma_allowed_in_interface(promise_semipure(_, _), no).
pragma_allowed_in_interface(unused_args(_, _, _, _, _), no).
pragma_allowed_in_interface(exceptions(_, _, _, _, _), no).
pragma_allowed_in_interface(type_spec(_, _, _, _, _, _, _, _), yes).
pragma_allowed_in_interface(termination_info(_, _, _, _, _), yes).
pragma_allowed_in_interface(termination2_info(_,_, _, _, _, _), yes).
pragma_allowed_in_interface(terminates(_, _), yes).
pragma_allowed_in_interface(does_not_terminate(_, _), yes).
pragma_allowed_in_interface(check_termination(_, _), yes).
    % `aditi', `base_relation', `index' and `owner' pragmas must be in the
    % interface for exported preds. This is checked in make_hlds.m.
pragma_allowed_in_interface(aditi(_, _), yes).
pragma_allowed_in_interface(base_relation(_, _), yes).
pragma_allowed_in_interface(aditi_index(_, _, _), yes).
pragma_allowed_in_interface(supp_magic(_, _), no).
pragma_allowed_in_interface(context(_, _), no).
pragma_allowed_in_interface(aditi_memo(_, _), no).
pragma_allowed_in_interface(aditi_no_memo(_, _), no).
pragma_allowed_in_interface(naive(_, _), no).
pragma_allowed_in_interface(psn(_, _), no).
pragma_allowed_in_interface(owner(_, _, _), yes).
pragma_allowed_in_interface(mode_check_clauses(_, _), yes).

check_for_no_exports(Items, ModuleName, !IO) :-
    globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning, !IO),
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
check_int_for_no_exports([Item - _Context | Items], ModuleName, !IO) :-
    (
        (
            Item = nothing(_)
        ;
            Item = module_defn(_, ModuleDefn),
            ModuleDefn \= include_module(_)
        )
    ->
        % nothing useful - keep searching
        check_int_for_no_exports(Items, ModuleName, !IO)
    ;
        % we found something useful - don't issue the warning
        true
    ).

:- pred warn_no_exports(module_name::in, io::di, io::uo) is det.

warn_no_exports(ModuleName, !IO) :-
    globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning, !IO),
    (
        ExportWarning = yes,
        module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
        report_warning(context_init(FileName, 1), 0,
            [words("Warning: interface for module"),
            sym_name(ModuleName),
            words("does not export anything.")], !IO),
        globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            report_warning(context_init(FileName, 1), 0,
                [words("To be useful, a module should export"),
                words("something. A file should contain"),
                words("at least one declaration other than"),
                fixed("`:- import_module'"),
                words("in its interface section(s)."),
                words("This would normally be a"),
                fixed("`:- pred',"), fixed("`:- func',"),
                fixed("`:- type',"), fixed("`:- inst'"),
                fixed("or `:- mode'"),
                fixed("declaration.")], !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ;
        ExportWarning = no
    ).

%-----------------------------------------------------------------------------%

:- pred write_interface_file(file_name::in, module_name::in, string::in,
    maybe(timestamp)::in, item_list::in, io::di, io::uo) is det.

write_interface_file(_SourceFileName, ModuleName, Suffix, MaybeTimestamp,
        InterfaceItems0, !IO) :-

        % Create (e.g.) `foo.int.tmp'.
    string__append(Suffix, ".tmp", TmpSuffix),
    module_name_to_file_name(ModuleName, Suffix, yes, OutputFileName, !IO),
    module_name_to_file_name(ModuleName, TmpSuffix, no, TmpOutputFileName,
        !IO),

    globals__io_lookup_bool_option(line_numbers, LineNumbers, !IO),
    globals__io_set_option(line_numbers, bool(no), !IO),

    globals__io_lookup_bool_option(generate_item_version_numbers,
        GenerateVersionNumbers, !IO),

    (
        GenerateVersionNumbers = yes,
        % Find the timestamp of the current module.
        (
            MaybeTimestamp = yes(Timestamp),

            % Read in the previous version of the file.
            read_mod_ignore_errors(ModuleName, Suffix,
                "Reading old interface for module", yes, no,
                OldItems, OldError, _OldIntFileName, _OldTimestamp, !IO),
            ( OldError = no_module_errors ->
                MaybeOldItems = yes(OldItems)
            ;
                % If we can't read in the old file, the
                % timestamps will all be set to the
                % modification time of the source file.
                MaybeOldItems = no
            ),
            recompilation__version__compute_version_numbers(Timestamp,
                InterfaceItems0, MaybeOldItems, VersionNumbers),
            VersionNumberItem = module_defn(varset__init,
                version_numbers(ModuleName, VersionNumbers))
                - term__context_init,
            (
                InterfaceItems0 = [FirstItem | InterfaceItems1],
                FirstItem = module_defn(_, interface) - _
            ->
                InterfaceItems = [FirstItem, VersionNumberItem
                    | InterfaceItems1]
            ;
                InterfaceItems = [make_pseudo_decl(interface),
                    VersionNumberItem | InterfaceItems0]
            )
        ;
            MaybeTimestamp = no,
            error("write_interface_file with " ++
                "`--smart-recompilation', timestamp not read")
        )
    ;
        GenerateVersionNumbers = no,
        InterfaceItems = InterfaceItems0
    ),
    convert_to_mercury(ModuleName, TmpOutputFileName, InterfaceItems, !IO),
    globals__io_set_option(line_numbers, bool(LineNumbers), !IO),
    update_interface(OutputFileName, !IO).

        % update <Module>.int from <Module>.int.tmp if necessary

update_interface(OutputFileName, !IO) :-
    update_interface(OutputFileName, Succeeded, !IO),
    (
        Succeeded = no,
        report_error("problem updating interface files.", !IO)
    ;
        Succeeded = yes
    ).

update_interface(OutputFileName, Succeeded, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Updating interface:\n", !IO),
    TmpOutputFileName = OutputFileName ++ ".tmp",
    io__open_binary_input(OutputFileName, OutputFileRes, !IO),
    (
        OutputFileRes = ok(OutputFileStream),
        io__open_binary_input(TmpOutputFileName, TmpOutputFileRes, !IO),
        (
            TmpOutputFileRes = ok(TmpOutputFileStream),
            binary_input_stream_cmp(OutputFileStream, TmpOutputFileStream,
                FilesDiffer, !IO),
            io__close_binary_input(OutputFileStream, !IO),
            io__close_binary_input(TmpOutputFileStream, !IO),
            (
                FilesDiffer = ok(ok(no)),
                Succeeded = yes,
                maybe_write_string(Verbose, "% ", !IO),
                maybe_write_string(Verbose, OutputFileName, !IO),
                maybe_write_string(Verbose, "' has not changed.\n", !IO),
                io__remove_file(TmpOutputFileName, _, !IO)
            ;
                FilesDiffer = ok(ok(yes)),
                update_interface_create_file("CHANGED", OutputFileName,
                    TmpOutputFileName, Succeeded, !IO)
            ;
                FilesDiffer = ok(error(TmpFileError)),
                Succeeded = no,
                io__write_string("Error reading `", !IO),
                io__write_string(TmpOutputFileName, !IO),
                io__write_string("': ", !IO),
                io__write_string(io__error_message(TmpFileError), !IO),
                io__nl(!IO)
            ;
                FilesDiffer = error(_, _),
                update_interface_create_file("been CREATED", OutputFileName,
                    TmpOutputFileName, Succeeded, !IO)
            )
        ;

            TmpOutputFileRes = error(TmpOutputFileError),
            Succeeded = no,
            io__close_binary_input(OutputFileStream, !IO),
            io__write_string("Error creating `", !IO),
            io__write_string(OutputFileName, !IO),
            io__write_string("': ", !IO),
            io__write_string(io__error_message(TmpOutputFileError), !IO),
            io__nl(!IO)
        )
    ;
        OutputFileRes = error(_),
        update_interface_create_file("been CREATED", OutputFileName,
            TmpOutputFileName, Succeeded, !IO)
    ).

:- pred binary_input_stream_cmp_2(io__binary_input_stream::in, int::in,
    bool::out, io__res(bool)::in, io__res(bool)::out,
    io::di, io::uo) is det.

binary_input_stream_cmp_2(TmpOutputFileStream, Byte, Continue, _, Differ,
        !IO) :-
    io__read_byte(TmpOutputFileStream, TmpByteResult, !IO),
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
        Differ = error(TmpByteError) `with_type` io__res(bool),
        Continue = no
    ).

:- pred binary_input_stream_cmp(io__binary_input_stream::in,
    io__binary_input_stream::in, io__maybe_partial_res(io__res(bool))::out,
    io::di, io::uo) is det.

binary_input_stream_cmp(OutputFileStream, TmpOutputFileStream, FilesDiffer,
        !IO) :-
    io__binary_input_stream_foldl2_io_maybe_stop(OutputFileStream,
        binary_input_stream_cmp_2(TmpOutputFileStream),
        ok(no), FilesDiffer0, !IO),

    % Check whether there is anything left in TmpOutputFileStream
    ( FilesDiffer0 = ok(ok(no)) ->
        io__read_byte(TmpOutputFileStream, TmpByteResult2, !IO),
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
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose,
        "% `" ++ OutputFileName ++ "' has " ++ Msg ++ ".\n", !IO),
    copy_file(TmpOutputFileName, OutputFileName, MoveRes, !IO),
    (
        MoveRes = ok,
        Succeeded = yes
    ;
        MoveRes = error(MoveError),
        Succeeded = no,
        io__write_string("Error creating `" ++ OutputFileName ++ "': " ++
            io__error_message(MoveError), !IO),
        io__nl(!IO)
    ),
    io__remove_file(TmpOutputFileName, _, !IO).

%-----------------------------------------------------------------------------%

touch_interface_datestamp(ModuleName, Ext, !IO) :-
    module_name_to_file_name(ModuleName, Ext, yes, OutputFileName, !IO),
    touch_datestamp(OutputFileName, !IO).

touch_datestamp(OutputFileName, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose,
        "% Touching `" ++ OutputFileName ++ "'... ", !IO),
    maybe_flush_output(Verbose, !IO),
    io__open_output(OutputFileName, Result, !IO),
    (
        Result = ok(OutputStream),
        io__write_string(OutputStream, "\n", !IO),
        io__close_output(OutputStream, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        Result = error(IOError),
        io__error_message(IOError, IOErrorMessage),
        io__write_string("\nError opening `" ++ OutputFileName
            ++ "' for output: " ++ IOErrorMessage ++ ".\n", !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

grab_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
        NestedChildren, ReadModules, MaybeTimestamp,
        Items0, !:Module, Error, !IO) :-
        %
        % Find out which modules this one depends on
        %
    AncestorModules = get_ancestors(ModuleName),
    get_dependencies(Items0, IntImportedModules0, IntUsedModules0,
        ImpImportedModules0, ImpUsedModules0),

    list__append(IntImportedModules0, ImpImportedModules0, ImportedModules0),
    list__append(IntUsedModules0, ImpUsedModules0, UsedModules0),

    warn_if_import_self_or_ancestor(ModuleName, AncestorModules,
        ImportedModules0, UsedModules0, !IO),

    warn_if_duplicate_use_import_decls(ModuleName,
        IntImportedModules0, IntImportedModules1,
        IntUsedModules0, IntUsedModules1,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules0, ImpUsedModules, !IO),

    get_fact_table_dependencies(Items0, FactDeps),
    get_interface_and_implementation(ModuleName, no, Items0,
        InterfaceItems, ImplItems),
    get_children(InterfaceItems, PublicChildren),
    (
        MaybeTimestamp = yes(Timestamp),
        MaybeTimestamps = yes(map__det_insert(map__init, ModuleName,
            module_timestamp(".m", Timestamp, may_be_unqualified)))
    ;
        MaybeTimestamp = no,
        MaybeTimestamps = no

    ),
    init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, PublicChildren, NestedChildren, FactDeps,
        MaybeTimestamps, !:Module),
    
        % If this module has any separately-compiled sub-modules,
        % then we need to make everything in the implementation
        % of this module exported_to_submodules.  We do that by
        % splitting out the implementation declarations and putting
        % them in a special `:- private_interface' section.
        %
    get_children(Items0, Children),
    (
        Children = [],
        Items1 = Items0
    ;
        Children = [_ | _],
        split_clauses_and_decls(ImplItems, Clauses, ImplDecls),
        list__condense(
            [[make_pseudo_decl(interface) | InterfaceItems],
            [make_pseudo_decl(private_interface) | ImplDecls],
            [make_pseudo_decl(implementation) | Clauses]], Items1),
        module_imports_set_items(Items1, !Module)
    ),

        % Add `builtin' and `private_builtin' to the
        % list of imported modules
    globals__io_get_globals(Globals, !IO),
    add_implicit_imports(Items1, Globals,
        IntImportedModules1, IntImportedModules2,
        IntUsedModules1, IntUsedModules2),

        % Process the ancestor modules
        % Uses of the items declared in ancestor modules
        % do not need module qualifiers. Modules imported
        % by ancestors are considered to be visible
        % in the current module.
    process_module_private_interfaces(ReadModules, AncestorModules,
        make_pseudo_decl(imported(ancestor_private_interface)),
        make_pseudo_decl(abstract_imported),
        IntImportedModules2, IntImportedModules,
        IntUsedModules2, IntUsedModules, !Module, !IO),

        % Process the modules imported using `import_module'.
        % Uses of these items do not need module qualifiers.
    IntIndirectImports0 = [],
    IntImpIndirectImports0 = [],
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        IntImportedModules, ".int",
        make_pseudo_decl(imported(interface)),
        make_pseudo_decl(abstract_imported),
        IntIndirectImports0, IntIndirectImports1,
        IntImpIndirectImports0, IntImpIndirectImports1,
        !Module, !IO),

    ImpIndirectImports0 = [],
    ImpImpIndirectImports0 = [],
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ImpImportedModules, ".int",
        make_pseudo_decl(imported(implementation)),
        make_pseudo_decl(abstract_imported),
        ImpIndirectImports0, ImpIndirectImports1,
        ImpImpIndirectImports0, ImpImpIndirectImports1,
        !Module, !IO),

        % Process the modules imported using `use_module' .
    process_module_long_interfaces(ReadModules, must_be_qualified,
        IntUsedModules, ".int",
        make_pseudo_decl(used(interface)),
        make_pseudo_decl(abstract_imported),
        IntIndirectImports1, IntIndirectImports,
        IntImpIndirectImports1, IntImpIndirectImports2,
        !Module, !IO),
    process_module_long_interfaces(ReadModules, must_be_qualified,
        ImpUsedModules, ".int",
        make_pseudo_decl(used(implementation)),
        make_pseudo_decl(abstract_imported),
        ImpIndirectImports1, ImpIndirectImports,
        ImpImpIndirectImports1, ImpImpIndirectImports2,
        !Module, !IO),

        % Process the short interfaces for indirectly imported modules.
        % The short interfaces are treated as if
        % they are imported using `use_module'.
    append_pseudo_decl(transitively_imported, !Module),
    process_module_short_interfaces_transitively(ReadModules,
        IntIndirectImports, ".int2",
        make_pseudo_decl(used(interface)),
        make_pseudo_decl(abstract_imported),
        IntImpIndirectImports2, IntImpIndirectImports, !Module, !IO),
    process_module_short_interfaces_transitively(ReadModules,
        ImpIndirectImports, ".int2",
        make_pseudo_decl(used(implementation)),
        make_pseudo_decl(abstract_imported),
        ImpImpIndirectImports2, ImpImpIndirectImports, !Module, !IO),

        % Process the short interfaces for modules imported in
        % the implementation of indirectly imported modules.
        % The items in these modules shouldn't be visible
        % to typechecking -- they are used for fully expanding
        % equivalence types after the semantic checking passes.
    process_module_short_interfaces_and_impls_transitively(
        ReadModules, IntImpIndirectImports, ".int2",
        make_pseudo_decl(abstract_imported),
        make_pseudo_decl(abstract_imported),
        !Module, !IO),
    process_module_short_interfaces_and_impls_transitively(
        ReadModules, ImpImpIndirectImports, ".int2",
        make_pseudo_decl(abstract_imported),
        make_pseudo_decl(abstract_imported),
        !Module, !IO),

    module_imports_get_items(!.Module, Items),
    check_imports_accessibility(ModuleName,
        IntImportedModules ++ IntUsedModules ++
        ImpImportedModules ++ ImpUsedModules, Items, !IO),

    module_imports_get_error(!.Module, Error).

    % grab_unqual_imported_modules:
    %
    % Like grab_imported_modules, but gets the `.int3' files
    % instead of the `.int' and `.int2' files.
    %
grab_unqual_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, !:Module, Error, !IO) :-
        %
        % Find out which modules this one depends on
        %
    ParentDeps = get_ancestors(ModuleName),
    get_dependencies(Items0, IntImportDeps0, IntUseDeps0,
        ImpImportDeps, ImpUseDeps),

        %
        % Construct the initial module import structure.
        %
    init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
        Items0, [], [], [], no, !:Module),

        % Add `builtin' and `private_builtin' to the imported modules.
    globals__io_get_globals(Globals, !IO),
    add_implicit_imports(Items0, Globals,
        IntImportDeps0, IntImportDeps, IntUseDeps0, IntUseDeps),

        %
        % Get the .int3s and .int0s that the current module depends on.
        %
    map__init(ReadModules),

        % first the .int0s for parent modules
    process_module_private_interfaces(ReadModules, ParentDeps,
        make_pseudo_decl(imported(ancestor_private_interface)),
        make_pseudo_decl(abstract_imported),
        [], ParentImportDeps, [], ParentUseDeps, !Module, !IO),

        % then the .int3s for `:- import'-ed modules
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ParentImportDeps, ".int3",
        make_pseudo_decl(imported(ancestor)),
        make_pseudo_decl(abstract_imported),
        [], IntIndirectImportDeps0, [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        IntImportDeps, ".int3",
        make_pseudo_decl(imported(interface)),
        make_pseudo_decl(abstract_imported),
        IntIndirectImportDeps0, IntIndirectImportDeps1,
        [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ImpImportDeps, ".int3",
        make_pseudo_decl(imported(implementation)),
        make_pseudo_decl(abstract_imported),
        [], ImpIndirectImportDeps0,
        [], _, !Module, !IO),

        % then (after appropriate `:- used' decls)
        % the .int3s for `:- use'-ed modules
    process_module_long_interfaces(ReadModules, may_be_unqualified,
        ParentUseDeps, ".int3",
        make_pseudo_decl(imported(ancestor)),
        make_pseudo_decl(abstract_imported),
        IntIndirectImportDeps1, IntIndirectImportDeps2,
        [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, must_be_qualified,
        IntUseDeps, ".int3",
        make_pseudo_decl(used(interface)),
        make_pseudo_decl(abstract_imported),
        IntIndirectImportDeps2, IntIndirectImportDeps,
        [], _, !Module, !IO),
    process_module_long_interfaces(ReadModules, must_be_qualified,
        ImpUseDeps, ".int3",
        make_pseudo_decl(used(implementation)),
        make_pseudo_decl(abstract_imported),
        ImpIndirectImportDeps0, ImpIndirectImportDeps,
        [], _, !Module, !IO),

        % then (after appropriate `:- used' decl)
        % the .int3s for indirectly imported modules
    process_module_short_interfaces_transitively(ReadModules,
        IntIndirectImportDeps, ".int3",
        make_pseudo_decl(used(interface)),
        make_pseudo_decl(abstract_imported),
        [], _, !Module, !IO),

    process_module_short_interfaces_transitively(ReadModules,
        ImpIndirectImportDeps, ".int3",
        make_pseudo_decl(used(implementation)),
        make_pseudo_decl(abstract_imported),
        [], _, !Module, !IO),

    module_imports_get_items(!.Module, Items),
    check_imports_accessibility(ModuleName,
        IntImportDeps ++ IntUseDeps ++ ImpImportDeps ++ ImpUseDeps,
        Items, !IO),

    module_imports_get_error(!.Module, Error).

%-----------------------------------------------------------------------------%

find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp,
        Items, MaybeTimestamp, Error, FileName) :-
    map__search(ReadModules, ModuleName - Suffix, ReadModule),
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
        MaybeTimestamps, no_main, dir__this_directory).

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
    list__append(Items0, [make_pseudo_decl(PseudoDecl)], Items),
    Module = Module0 ^ items := Items.

make_pseudo_decl(PseudoDecl) =
    module_defn(varset__init, PseudoDecl) - term__context_init.

%-----------------------------------------------------------------------------%

get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps) :-
    add_implicit_imports(Items, Globals, [], ImportDeps, [], UseDeps).

:- pred add_implicit_imports(item_list::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

add_implicit_imports(Items, Globals, !ImportDeps, !UseDeps) :-
    mercury_public_builtin_module(MercuryPublicBuiltin),
    mercury_private_builtin_module(MercuryPrivateBuiltin),
    mercury_table_builtin_module(MercuryTableBuiltin),
    mercury_profiling_builtin_module(MercuryProfilingBuiltin),
    mercury_term_size_prof_builtin_module(MercuryTermSizeProfBuiltin),
    aditi_private_builtin_module(AditiPrivateBuiltin),
    !:ImportDeps = [MercuryPublicBuiltin | !.ImportDeps],
    !:UseDeps = [MercuryPrivateBuiltin | !.UseDeps],
    (
        %
        % We should include MercuryTableBuiltin if the Items contain
        % a tabling pragma, or if one of --use-minimal-model and
        % --trace-table-io is specified.
        %
        ( contains_tabling_pragma(Items)
        ; globals__lookup_bool_option(Globals,
            use_minimal_model_stack_copy, yes)
        ; globals__lookup_bool_option(Globals,
            use_minimal_model_own_stacks, yes)
        ; globals__lookup_bool_option(Globals, trace_table_io, yes)
        )
    ->
        !:UseDeps = [MercuryTableBuiltin | !.UseDeps]
    ;
        true
    ),
    globals__lookup_bool_option(Globals, profile_deep, Deep),
    (
        Deep = yes,
        !:UseDeps = [MercuryProfilingBuiltin | !.UseDeps]
    ;
        Deep = no
    ),
    (
        (
            globals__lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals__lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        )
    ->
        !:UseDeps = [MercuryTermSizeProfBuiltin | !.UseDeps]
    ;
        true
    ),
    globals__lookup_bool_option(Globals, aditi, Aditi),
    (
        Aditi = yes,
        !:UseDeps = [AditiPrivateBuiltin | !.UseDeps]
    ;
        Aditi = no
    ).

:- pred contains_tabling_pragma(item_list::in) is semidet.

contains_tabling_pragma([Item | Items]) :-
    (
        Item = pragma(_, Pragma) - _Context,
        Pragma = tabled(_, _, _, _, _)
    ;
        contains_tabling_pragma(Items)
    ).

:- pred warn_if_import_self_or_ancestor(module_name::in, list(module_name)::in,
    list(module_name)::in, list(module_name)::in,
    io::di, io::uo) is det.

% Warn if a module imports itself, or an ancestor.

warn_if_import_self_or_ancestor(ModuleName, AncestorModules,
        ImportedModules, UsedModules, !IO) :-
    globals__io_lookup_bool_option(warn_simple_code, Warn, !IO),
    (
        Warn = yes,
        (
            ( list__member(ModuleName, ImportedModules)
            ; list__member(ModuleName, UsedModules)
            )
        ->
            module_name_to_file_name(ModuleName, ".m", no,
                FileName, !IO),
            term__context_init(FileName, 1, Context),
            prog_out__write_context(Context, !IO),
            report_warning("Warning: module `", !IO),
            prog_out__write_sym_name(ModuleName, !IO),
            io__write_string("' imports itself!\n", !IO)
        ;
            true
        ),
        IsImportedAncestor = (pred(Import::out) is nondet :-
            list__member(Import, AncestorModules),
            ( list__member(Import, ImportedModules)
            ; list__member(Import, UsedModules)
            )
        ),
        aggregate(IsImportedAncestor,
            warn_imported_ancestor(ModuleName), !IO)
    ;
        Warn = no
    ).

:- pred warn_imported_ancestor(module_name::in, module_name::in,
    io::di, io::uo) is det.

warn_imported_ancestor(ModuleName, AncestorName, !IO) :-
    module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
    term__context_init(FileName, 1, Context),
    record_warning(!IO),
    report_warning(Context, 0,
        [words("module"), sym_name(ModuleName),
        words("imports its own ancestor, module"),
        sym_name(AncestorName), words(".")], !IO),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        report_warning(Context, 0,
            [words("Every sub-module implicitly imports"),
            words("its ancestors."),
            words("There is no need to explicitly import them.")],
            !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO)
    ).

    % This predicate ensures that all every import_module declaration is
    % checked against every use_module declaration, except for the case
    % where the interface has `:- use_module foo.' and the implementation
    % `:- import_module foo.'.
    % warn_if_duplicate_use_import_decls/7 is called to generate the actual
    % warnings.

:- pred warn_if_duplicate_use_import_decls(module_name::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

warn_if_duplicate_use_import_decls(ModuleName,
        IntImportedModules0, IntImportedModules,
        IntUsedModules0, IntUsedModules,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules0, ImpUsedModules, !IO) :-

    warn_if_duplicate_use_import_decls(ModuleName,
        IntImportedModules0, IntImportedModules1,
        IntUsedModules0, IntUsedModules, !IO),
    warn_if_duplicate_use_import_decls(ModuleName,
        IntImportedModules1, IntImportedModules,
        ImpUsedModules0, ImpUsedModules1, !IO),

    warn_if_duplicate_use_import_decls(ModuleName,
        ImpImportedModules0, ImpImportedModules,
        ImpUsedModules1, ImpUsedModules, !IO).

    % Report warnings for modules imported using both `:- use_module'
    % and `:- import_module'.  Remove the unnecessary `:- use_module'
    % declarations.

:- pred warn_if_duplicate_use_import_decls(module_name::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

warn_if_duplicate_use_import_decls(ModuleName, !ImportedModules, !UsedModules,
        !IO) :-
    set__list_to_set(!.ImportedModules, ImportedSet),
    set__list_to_set(!.UsedModules, UsedSet),
    set__intersect(ImportedSet, UsedSet, BothSet),
    ( set__empty(BothSet) ->
        true
    ;
        set__to_sorted_list(BothSet, BothList),
        globals__io_lookup_bool_option(warn_simple_code, WarnSimple, !IO),
        (
            WarnSimple = yes,
            module_name_to_file_name(ModuleName, ".m", no, FileName, !IO),
            term__context_init(FileName, 1, Context),
            prog_out__write_context(Context, !IO),
            io__write_string("Warning:", !IO),
            ( BothList = [_] ->
                io__write_string(" module ", !IO),
                prog_out__write_module_list(BothList, !IO),
                io__write_string(" is ", !IO)
            ;
                io__write_string(" modules ", !IO),
                prog_out__write_module_list(BothList, !IO),
                io__write_string(" are ", !IO)
            ),
            io__write_string("imported using both\n", !IO),
            prog_out__write_context(Context, !IO),
            io__write_string("  `:- import_module' and ", !IO),
            io__write_string("`:- use_module' declarations.\n", !IO),

            globals__io_lookup_bool_option(halt_at_warn, Halt, !IO),
            (
                Halt = yes,
                io__set_exit_status(1, !IO)
            ;
                Halt = no
            )
        ;
            WarnSimple = no
        ),

        % Treat the modules with both types of import as if they
        % were imported using `:- import_module.'
        list__delete_elems(!.UsedModules, BothList, !:UsedModules)
    ).

%-----------------------------------------------------------------------------%

write_dependency_file(Module, AllDepsSet, MaybeTransOptDeps, !IO) :-
    Module = module_imports(SourceFileName, SourceFileModuleName,
        ModuleName, ParentDeps, IntDeps, ImplDeps, IndirectDeps,
        _Children, InclDeps, _NestDeps, FactDeps0,
        ContainsForeignCode, ForeignImports0, _ContainsForeignExport,
        Items, _Error, _Timestamps, _HasMain, _Dir),
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
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
    io__make_temp(dir__dirname(DependencyFileName), "tmp_d",
        TmpDependencyFileName, !IO),
    maybe_write_string(Verbose, "% Writing auto-dependency file `", !IO),
    maybe_write_string(Verbose, DependencyFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io__open_output(TmpDependencyFileName, Result, !IO),
    (
        Result = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io__error_message(IOError, IOErrorMessage),
        string__append_list(["error opening temporary file `",
            TmpDependencyFileName, "' for output: ",
            IOErrorMessage], Message),
        report_error(Message, !IO)
    ;
        Result = ok(DepStream),
        list__append(IntDeps, ImplDeps, LongDeps0),
        ShortDeps0 = IndirectDeps,
        set__list_to_set(LongDeps0, LongDepsSet0),
        set__delete(LongDepsSet0, ModuleName, LongDepsSet),
        set__list_to_set(ShortDeps0, ShortDepsSet0),
        set__difference(ShortDepsSet0, LongDepsSet, ShortDepsSet1),
        set__delete(ShortDepsSet1, ModuleName, ShortDepsSet),
        set__to_sorted_list(LongDepsSet, LongDeps),
        set__to_sorted_list(ShortDepsSet, ShortDeps),
        set__to_sorted_list(AllDepsSet, AllDeps),
        list__sort_and_remove_dups(FactDeps0, FactDeps),

        (
            MaybeTransOptDeps = yes(TransOptDeps0),
            set__list_to_set(TransOptDeps0, TransOptDepsSet0),
            set__intersect(TransOptDepsSet0, LongDepsSet, TransOptDepsSet),
            set__to_sorted_list(TransOptDepsSet, TransOptDateDeps),
            %
            % note that maybe_read_dependency_file searches for
            % this exact pattern
            %
            io__write_strings(DepStream, [TransOptDateFileName, " :"], !IO),
            write_dependencies_list(TransOptDateDeps, ".trans_opt", DepStream,
                !IO)
        ;
            MaybeTransOptDeps = no
        ),

        (
            FactDeps = [_ | _],
            io__write_strings(DepStream,
                ["\n\n", MakeVarName, ".fact_tables ="], !IO),
            write_file_dependencies_list(FactDeps, "", DepStream, !IO),
            io__nl(DepStream, !IO),
            globals__io_lookup_bool_option(assume_gmake, AssumeGmake, !IO),
            (
                AssumeGmake = yes,
                io__write_strings(DepStream, [
                    "\n\n", MakeVarName,
                    ".fact_tables.os = $(", MakeVarName,
                    ".fact_tables:%=$(os_subdir)%.$O)\n\n",
                    MakeVarName,
                    ".fact_tables.cs = $(", MakeVarName,
                    ".fact_tables:%=$(cs_subdir)%.c)\n\n"
                ], !IO)
            ;
                AssumeGmake = no,
                io__write_strings(DepStream,
                    [MakeVarName, ".fact_tables.cs ="], !IO),
                write_fact_table_dependencies_list(ModuleName,
                    FactDeps, ".c", DepStream, !IO),
                io__write_strings(DepStream, ["\n\n",
                    MakeVarName, ".fact_tables.os ="], !IO),
                write_fact_table_dependencies_list(ModuleName,
                    FactDeps, ".$O", DepStream, !IO),
                io__nl(DepStream, !IO)
            )
        ;
            FactDeps = []
        ),

        ( string__remove_suffix(SourceFileName, ".m", SourceFileBase) ->
            ErrFileName = SourceFileBase ++ ".err"
        ;
            error("modules.m: source file doesn't end in `.m'")
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
        module_name_to_file_name(ModuleName, ".rlo", no, RLOFileName, !IO),
        module_name_to_file_name(ModuleName, ".il_date", no, ILDateFileName,
            !IO),
        module_name_to_file_name(ModuleName, ".java_date", no,
            JavaDateFileName, !IO),
        module_name_to_file_name(ModuleName, ".pic_o", no, PicObjFileName, !IO),
        module_name_to_file_name(ModuleName, ".int0", no, Int0FileName, !IO),
        module_name_to_split_c_file_pattern(ModuleName, ".$O",
            SplitObjPattern, !IO),
        io__write_strings(DepStream, ["\n\n",
            OptDateFileName, " ",
            TransOptDateFileName, " ",
            ErrFileName, " ",
            CDateFileName, " ",
            AsmDateFileName, " ",
            PicAsmDateFileName, " ",
            SplitObjPattern, " ",
            RLOFileName, " ",
            ILDateFileName, " ",
            JavaDateFileName
        ] , !IO),
        write_dependencies_list(ParentDeps, ".optdate", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".trans_opt_date", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".c_date", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".s_date", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".pic_s_date", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".dir/*.$O", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".rlo", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".il_date", DepStream, !IO),
        write_dependencies_list(ParentDeps, ".java_date", DepStream, !IO),
        io__write_strings(DepStream, [" : ", SourceFileName], !IO),
        % If the module contains nested sub-modules then `.int0'
        % file must first be built.
        (
            InclDeps = [_ | _],
            io__write_strings(DepStream, [" ", Int0FileName], !IO)
        ;
            InclDeps = []
        ),
        write_dependencies_list(ParentDeps, ".int0", DepStream, !IO),
        write_dependencies_list(LongDeps, ".int", DepStream, !IO),
        write_dependencies_list(ShortDeps, ".int2", DepStream, !IO),

        (
            FactDeps = [_ | _],
            io__write_strings(DepStream, [
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

        globals__io_lookup_bool_option(use_opt_files, UseOptFiles, !IO),
        globals__io_lookup_bool_option(intermodule_optimization, Intermod, !IO),
        globals__io_lookup_accumulating_option(intermod_directories,
            IntermodDirs, !IO),

            % If intermodule_optimization is enabled then
            % all the .mh files must exist because it is
            % possible that the .c file imports them
            % directly or indirectly.
        (
            Intermod = yes,
            io__write_strings(DepStream, ["\n\n", ObjFileName, " : "], !IO),
            write_dependencies_list(AllDeps, ".mh", DepStream, !IO)
        ;
            Intermod = no
        ),
        ( ( Intermod = yes ; UseOptFiles = yes ) ->
            io__write_strings(DepStream, [
                "\n\n",
                TransOptDateFileName, " ",
                ErrFileName, " ",
                CDateFileName, " ",
                AsmDateFileName, " ",
                PicAsmDateFileName, " ",
                SplitObjPattern, " ",
                RLOFileName, " ",
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
            globals__io_lookup_bool_option(transitive_optimization, TransOpt,
                !IO),
            globals__io_lookup_bool_option(use_trans_opt_files, UseTransOpt,
                !IO),

            ( ( TransOpt = yes ; UseTransOpt = yes ) ->
                bool__not(UseTransOpt, BuildOptFiles),
                get_both_opt_deps(BuildOptFiles, [ModuleName | LongDeps],
                    IntermodDirs, OptDeps, TransOptDeps, !IO),
                OptInt0Deps = sort_and_remove_dups(
                    condense(list__map(get_ancestors, OptDeps))),
                write_dependencies_list(OptDeps, ".opt", DepStream, !IO),
                write_dependencies_list(OptInt0Deps, ".int0", DepStream, !IO),

                io__write_strings(DepStream, [
                    "\n\n",
                    ErrFileName, " ",
                    CDateFileName, " ",
                    AsmDateFileName, " ",
                    PicAsmDateFileName, " ",
                    SplitObjPattern, " ",
                    RLOFileName, " ",
                    ILDateFileName, " ",
                    JavaDateFileName, " : "
                ], !IO),
                write_dependencies_list(TransOptDeps, ".trans_opt", DepStream,
                    !IO)
            ;
                bool__not(UseOptFiles, BuildOptFiles),
                get_opt_deps(BuildOptFiles, [ModuleName | LongDeps],
                    IntermodDirs, ".opt", OptDeps, !IO),
                OptInt0Deps = sort_and_remove_dups(
                    condense(list__map(get_ancestors, OptDeps))),
                write_dependencies_list(OptDeps, ".opt", DepStream, !IO),
                write_dependencies_list(OptInt0Deps, ".int0", DepStream, !IO)
            )
        ;
            true
        ),

        globals__io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
        globals__io_get_target(CompilationTarget, !IO),
        (
            HighLevelCode = yes,
            CompilationTarget = c
        ->
            %
            % For --high-level-code with --target c,
            % we need to make sure that we
            % generate the header files for imported modules
            % before compiling the C files, since the generated C
            % files #include those header files.
            %
            io__write_strings(DepStream, [
                "\n\n",
                PicObjFileName, " ",
                ObjFileName, " ",
                SplitObjPattern, " :"
            ], !IO),
            write_dependencies_list(AllDeps, ".mih", DepStream, !IO)
        ;
            true
        ),

        %
        % We need to tell make how to make the header
        % files.  The header files are actually built by
        % the same command that creates the .c or .s file,
        % so we just make them depend on the .c or .s files.
        % This is needed for the --high-level-code rule above,
        % and for the rules introduced for
        % `:- pragma foreign_import_module' declarations.
        % In some grades the header file won't actually be built
        % (e.g. LLDS grades for modules not containing
        % `:- pragma export' declarations), but this
        % rule won't do any harm.
        %
        module_name_to_file_name(ModuleName, ".c", no, CFileName, !IO),
        module_name_to_file_name(ModuleName, ".s", no, AsmFileName, !IO),
        module_name_to_file_name(ModuleName, ".mh", no, HeaderFileName, !IO),
        module_name_to_file_name(ModuleName, ".mih", no, HeaderFileName2, !IO),
        io__write_strings(DepStream, [
            "\n\n",
            "ifeq ($(TARGET_ASM),yes)\n",
            HeaderFileName, " ", HeaderFileName2,
                " : ", AsmFileName, "\n",
            "else\n",
            HeaderFileName, " ",  HeaderFileName2,
                " : ", CFileName, "\n",
            "endif"
        ], !IO),

        %
        % The `.module_dep' file is made as a side effect of
        % creating the `.c', `.s', `.il', or `.java'.
        %
        module_name_to_file_name(ModuleName, ".il", no, ILFileName, !IO),
        module_name_to_file_name(ModuleName, ".java", no, JavaFileName, !IO),
        module_name_to_file_name(ModuleName, module_dep_file_extension,
            no, ModuleDepFileName, !IO),
        io__write_strings(DepStream, [
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

        % The .date and .date0 files depend on the .int0 files
        % for the parent modules, and the .int3 files for the
        % directly and indirectly imported modules.
        %
        % For nested sub-modules, the `.date' files for the
        % parent modules also depend on the same things as the
        % `.date' files for this module, since all the `.date'
        % files will get produced by a single mmc command.
        % Similarly for `.date0' files, except these don't
        % depend on the `.int0' files, because when doing the
        % `--make-private-interface' for nested modules, mmc
        % will process the modules in outermost to innermost
        % order so as to produce each `.int0' file before it is
        % needed.

        module_name_to_file_name(ModuleName, ".date", no, DateFileName, !IO),
        module_name_to_file_name(ModuleName, ".date0", no, Date0FileName, !IO),
        io__write_strings(DepStream, [
            "\n\n", DateFileName, " ",
            Date0FileName
        ], !IO),
        write_dependencies_list(ParentDeps, ".date", DepStream, !IO),
        io__write_strings(DepStream, [" : ", SourceFileName], !IO),
        write_dependencies_list(ParentDeps, ".int0", DepStream, !IO),
        write_dependencies_list(LongDeps, ".int3", DepStream, !IO),
        write_dependencies_list(ShortDeps, ".int3", DepStream, !IO),

        io__write_strings(DepStream, ["\n\n", Date0FileName], !IO),
        write_dependencies_list(ParentDeps, ".date0", DepStream, !IO),
        io__write_strings(DepStream, [" : ", SourceFileName], !IO),
        write_dependencies_list(LongDeps, ".int3", DepStream, !IO),
        write_dependencies_list(ShortDeps, ".int3", DepStream, !IO),

        %
        % If we can pass the module name rather than the
        % file name then do so. `--smart-recompilation'
        % doesn't work if the file name is passed and the
        % module name doesn't match the file name.
        %
        have_source_file_map(HaveMap, !IO),
        (
            HaveMap = yes,
            module_name_to_file_name(SourceFileModuleName, ModuleArg)
        ;
            HaveMap = no,
            ModuleArg = SourceFileName
        ),

        %
        % XXX The rule below will cause an undefined make
        % variable warning for $(MCFLAGS-module) when run,
        % but there's no easy way to avoid that without using
        % features only available in recent versions of make.
        % The special case handling of $(MCFLAGS-module) in
        % this rule is necessary because it's difficult to extract
        % the module name from $@ (e.g. module.dir/module_000.o) in
        % the code for TARGET_MCFLAGS in Mmake.vars.in using make's
        % text handling functions ($(patsubst ...) only works for a
        % single occurrence of the pattern).
        %
        % With recent versions of make (3.78 or later) it would be
        % possible to avoid the warning using
        %    $(if $(findstring undefined,$(origin MCFLAGS-module)),,\
        %   $(MCFLAGS-module))
        %
        module_name_to_file_name(ModuleName, ".dir", no, DirFileName, !IO),
        io__write_strings(DepStream, [
            "\n\n",
            SplitObjPattern, " : ",
                SourceFileName, "\n",
            "\trm -rf ", DirFileName, "\n",
            "\t$(MCS) $(ALL_GRADEFLAGS) $(ALL_MCSFLAGS) ",
                "$(MCFLAGS-", MakeVarName, ") ",
                ModuleArg, "\n\n"
        ], !IO),

        globals__io_get_target(Target, !IO),
        globals__io_lookup_bool_option(sign_assembly, SignAssembly, !IO),
        globals__io_get_globals(Globals, !IO),

        % If we are on the IL backend, add the dependency that the
        % top level dll of a nested module hierachy depends on all
        % of it sub-modules dlls, as they are referenced from
        % inside the top level dll.
        % XXX Do we need to do the same for Java?

        module_name_to_file_name(ModuleName, ".dll", no, DllFileName, !IO),
        module_name_to_file_name(ModuleName, ".class", no, ClassFileName, !IO),
        SubModules = submodules(ModuleName, AllDeps),
        (
            Target = il,
            SubModules = [_ | _]
        ->
            io__write_strings(DepStream, [DllFileName, " : "], !IO),
            write_dll_dependencies_list(SubModules, "", DepStream, !IO),
            io__nl(DepStream, !IO)
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
            % a conservative % approximation to the set of foreign imports
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
            set__init(LangSet),
            ForeignImports = ForeignImports0
        ),

        %
        % Handle dependencies introduced by
        % `:- pragma foreign_import_module' declarations.
        %
        list__filter_map(
            (pred(ForeignImportMod::in, Import::out) is semidet :-
                Import = foreign_import_module_name(ForeignImportMod,
                    SourceFileModuleName),

                % XXX We can't include mercury.dll as mmake
                % can't find it, but we know that it exists.
                Import \= unqualified("mercury")
            ), ForeignImports, ForeignImportedModules),
        (
            ForeignImportedModules = []
        ;
            ForeignImportedModules = [_ | _],
            (
                Target = il,
                ForeignImportTarget = DllFileName,
                ForeignImportExt = ".dll"
            ;
                Target = java,
                ForeignImportTarget = ClassFileName,
                ForeignImportExt = ".java"
            ;
                Target = c,
                ForeignImportTarget = ObjFileName,
                ForeignImportExt = ".mh"
            ;
                Target = asm,
                ForeignImportTarget = ObjFileName,
                ForeignImportExt = ".mh"
            ),
            io__write_string(DepStream, "\n\n", !IO),
            io__write_string(DepStream, ForeignImportTarget, !IO),
            io__write_string(DepStream, " : ", !IO),
            write_dependencies_list(ForeignImportedModules, ForeignImportExt,
                DepStream, !IO),
            io__write_string(DepStream, "\n\n", !IO)
        ),

        (
            Target = il,
            not set__empty(LangSet)
        ->
            Langs = set__to_sorted_list(LangSet),
            list__foldl(write_foreign_dependency_for_il(DepStream,
                ModuleName, AllDeps, ForeignImports), Langs, !IO)
        ;
            true
        ),

            % If we are signing the assembly, then we will
            % need the strong key to sign the il file with
            % so add a dependency that the il file requires
            % the strong name file `mercury.sn'.
            % Also add the variable ILASM_KEYFLAG-<module> which
            % is used to build the command line for ilasm.
        (
            Target = il,
            SignAssembly = yes
        ->
            module_name_to_make_var_name(ModuleName, ModuleNameString),
            module_name_to_file_name(ModuleName, ".il", no, IlFileName, !IO),

            io__write_strings(DepStream, [
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

        %
        % We add some extra dependencies to the generated `.d' files,
        % so that local `.int', `.opt', etc. files shadow the
        % installed versions properly (e.g. for when you're trying
        % to build a new version of an installed library).  This
        % saves the user from having to add these explicitly if
        % they have multiple libraries installed in the same
        % installation hierarchy which aren't independent (e.g.
        % one uses another). These extra dependencies are necessary
        % due to the way the combination of search paths and
        % pattern rules works in Make.
        %
        % Be very careful about changing the following rules.
        % The `@:' is a silent do-nothing command.
        % It is used to force GNU Make to recheck the timestamp
        % on the target file.  (It is a pity that GNU Make doesn't
        % have a way of handling these sorts of rules in a nicer manner.)
        %

        io__write_strings(DepStream, [
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

        globals__io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
        (
            UseSubdirs = yes,
            io__nl(DepStream, !IO),
            list__foldl(write_subdirs_shorthand_rule(DepStream, ModuleName),
                [".c", ".$O", ".pic_o", ".s", ".pic_s",
                ".java", ".class", ".il", ".dll"], !IO)
        ;
            UseSubdirs = no
        ),

        ( SourceFileName \= default_source_file(ModuleName) ->
            %
            % The pattern rules in Mmake.rules won't work,
            % since the source file name doesn't match the
            % expected source file name for this module name.
            % This can occur due to just the use of different
            % source file names, or it can be due to the use
            % of nested modules.  So we need to output
            % hard-coded rules in this case.
            %
            % The rules output below won't work in the case
            % of nested modules with parallel makes,
            % because it will end up invoking the same
            % command twice (since it produces two output files)
            % at the same time.
            %
            % Any changes here will require corresponding
            % changes to scripts/Mmake.rules.  See that
            % file for documentation on these rules.
            %

            io__write_strings(DepStream, [
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
                    " $(ERR_REDIRECT)\n",
                RLOFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--aditi-only ", ModuleArg,
                    " $(ERR_REDIRECT)\n"
            ], !IO)
        ;
            true
        ),

        io__close_output(DepStream, !IO),
        io__rename_file(TmpDependencyFileName, DependencyFileName, Result3,
            !IO),
        (
            Result3 = error(_),
            % On some systems, we need to remove the existing file
            % first, if any.  So try again that way.
            io__remove_file(DependencyFileName, Result4, !IO),
            (
                Result4 = error(Error4),
                maybe_write_string(Verbose, " failed.\n", !IO),
                maybe_flush_output(Verbose, !IO),
                io__error_message(Error4, ErrorMsg),
                string__append_list(["can't remove file `", DependencyFileName,
                    "': ", ErrorMsg], Message),
                report_error(Message, !IO)
            ;
                Result4 = ok,
                io__rename_file(TmpDependencyFileName, DependencyFileName,
                    Result5, !IO),
                (
                    Result5 = error(Error5),
                    maybe_write_string(Verbose, " failed.\n", !IO),
                    maybe_flush_output(Verbose, !IO),
                    io__error_message(Error5, ErrorMsg),
                    string__append_list(["can't rename file `",
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
:- pred write_foreign_dependency_for_il(io__output_stream::in,sym_name::in,
    list(module_name)::in, foreign_import_module_info::in,
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

        io__write_strings(DepStream, [
            ForeignDllFileName, " : ", DllFileName], !IO),
            % XXX This change doesn't work correctly because
            % mmake can't find the dlls which don't reside
            % in the current directory.
        % write_dll_dependencies_list(ModuleName, AllDeps, DepStream,
        %   !IO),
        io__nl(DepStream, !IO),

        io__write_strings(DepStream, [
            ForeignFileName, " : ", IlFileName, "\n\n"], !IO),

        ( ForeignLang = csharp ->
            % Store in the variable
            % CSHARP_ASSEMBLY_REFS-foreign_code_name
            % the command line argument to reference all the
            % dlls the foreign code module references.
            io__write_strings(DepStream,
                ["CSHARP_ASSEMBLY_REFS-", ForeignModuleNameString, "="], !IO),
            (
                ModuleName = unqualified(Str),
                mercury_std_library_module(Str)
            ->
                Prefix = "/addmodule:"
            ;
                Prefix = "/r:"
            ),
            ForeignDeps = list__map(
                (func(M) =
                    foreign_import_module_name(M, ModuleName)
                ), ForeignImports),
            Deps = AllDeps ++ ForeignDeps,
            write_dll_dependencies_list(referenced_dlls(ModuleName, Deps),
                Prefix, DepStream, !IO),
            io__nl(DepStream, !IO)
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
:- pred write_subdirs_shorthand_rule(io__output_stream::in,
    module_name::in, string::in, io::di, io::uo) is det.

write_subdirs_shorthand_rule(DepStream, ModuleName, Ext, !IO) :-
    module_name_to_file_name(ModuleName, ModuleStr),
    module_name_to_file_name(ModuleName, Ext, no, Target, !IO),
    ShorthandTarget = ModuleStr ++ Ext,
    io__write_string(DepStream, ".PHONY: ", !IO),
    io__write_string(DepStream, ShorthandTarget, !IO),
    io__nl(DepStream, !IO),
    io__write_string(DepStream, ShorthandTarget, !IO),
    io__write_string(DepStream, ": ", !IO),
    io__write_string(DepStream, Target, !IO),
    io__nl(DepStream, !IO).

maybe_read_dependency_file(ModuleName, MaybeTransOptDeps, !IO) :-
    globals__io_lookup_bool_option(transitive_optimization, TransOpt, !IO),
    (
        TransOpt = yes,
        globals__io_lookup_bool_option(verbose, Verbose, !IO),
        module_name_to_file_name(ModuleName, ".d", no, DependencyFileName, !IO),
        maybe_write_string(Verbose, "% Reading auto-dependency file `", !IO),
        maybe_write_string(Verbose, DependencyFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        io__open_input(DependencyFileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            io__set_input_stream(Stream, OldStream, !IO),
            module_name_to_file_name(ModuleName, ".trans_opt_date",
                no, TransOptDateFileName0, !IO),
            string__to_char_list(TransOptDateFileName0,
                TransOptDateFileName),
            list__append(TransOptDateFileName, [' ', ':'], SearchPattern),
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
            io__set_input_stream(OldStream, _, !IO),
            io__close_input(Stream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            OpenResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io__error_message(IOError, IOErrorMessage),
            string__append_list(["error opening file `",
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
:- pred read_dependency_file_find_start(list(char)::in, bool::out,
    io::di, io::uo) is det.

read_dependency_file_find_start(SearchPattern, Success, !IO) :-
    io__read_line(Result, !IO),
    ( Result = ok(CharList) ->
        ( list__append(SearchPattern, _, CharList) ->
            % Have found the start
            Success = yes
        ;
            read_dependency_file_find_start(SearchPattern, Success, !IO)
        )
    ;
        Success = no
    ).

    % Read lines until one is found which does not contain whitespace
    % followed by a word which ends in .trans_opt.  Remove the
    % .trans_opt ending from all the words which are read in and return
    % the resulting list of modules..
:- pred read_dependency_file_get_modules(list(module_name)::out,
    io::di, io::uo) is det.

read_dependency_file_get_modules(TransOptDeps, !IO) :-
    io__read_line(Result, !IO),
    (
        Result = ok(CharList0),
        % Remove any whitespace from the beginning of the line,
        % then take all characters until another whitespace occurs.
        list__takewhile(char__is_whitespace, CharList0, _, CharList1),
        NotIsWhitespace = (pred(Char::in) is semidet :-
            \+ char__is_whitespace(Char)
        ),
        list__takewhile(NotIsWhitespace, CharList1, CharList, _),
        string__from_char_list(CharList, FileName0),
        string__remove_suffix(FileName0, ".trans_opt", FileName)
    ->
        ( string__append("Mercury/trans_opts/", BaseFileName, FileName) ->
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
            io__seen(!IO),
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
            io__seen(!IO)
        ;
            Result2 = error(_)
        ),
        module_name_to_file_name(Dep, ".trans_opt", no, TransOptName, !IO),
        search_for_file(IntermodDirs, TransOptName, Result3, !IO),
        (
            Result3 = ok(_),
            !:TransOptDeps = [Dep | !.TransOptDeps],
            io__seen(!IO)
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
            io__seen(!IO)
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
            io__seen(!IO)
        ;
            Result2 = error(_)
        )
    ;
        Found = yes
    ).

%-----------------------------------------------------------------------------%

generate_module_dependencies(ModuleName, !IO) :-
    map__init(DepsMap0),
    generate_dependencies(ModuleName, DepsMap0, !IO).

generate_file_dependencies(FileName, !IO) :-
    % read in the top-level file (to figure out its module name)
    read_mod_from_file(FileName, ".m", "Reading file", no, no, Items, Error,
        ModuleName, _, !IO),
    string__append(FileName, ".m", SourceFileName),
    split_into_submodules(ModuleName, Items, SubModuleList, !IO),
    globals__io_get_globals(Globals, !IO),
    assoc_list__keys(SubModuleList, SubModuleNames),
    list__map(init_dependencies(SourceFileName, ModuleName, SubModuleNames,
        Error, Globals), SubModuleList, ModuleImportsList),
    map__init(DepsMap0),
    list__foldl(insert_into_deps_map, ModuleImportsList, DepsMap0, DepsMap1),
    generate_dependencies(ModuleName, DepsMap1, !IO).

:- pred generate_dependencies(module_name::in, deps_map::in,
    io::di, io::uo) is det.

generate_dependencies(ModuleName, DepsMap0, !IO) :-
    % first, build up a map of the dependencies.
    generate_deps_map([ModuleName], DepsMap0, DepsMap, !IO),
    %
    % check whether we could read the main `.m' file
    %
    map__lookup(DepsMap, ModuleName, deps(_, ModuleImports)),
    module_imports_get_error(ModuleImports, Error),
    ( Error = fatal_module_errors ->
        sym_name_to_string(ModuleName, ModuleString),
        string__append_list(["can't read source file for module `",
            ModuleString, "'."], Message),
        report_error(Message, !IO)
    ;
        module_imports_get_source_file_name(ModuleImports, SourceFileName),
        generate_dependencies_write_dv_file(SourceFileName, ModuleName,
            DepsMap, !IO),
        generate_dependencies_write_dep_file(SourceFileName, ModuleName,
            DepsMap, !IO),

        %
        % compute the interface deps relation and
        % the implementation deps relation from the deps map
        %
        relation__init(IntDepsRel0),
        relation__init(ImplDepsRel0),
        map__values(DepsMap, DepsList),
        deps_list_to_deps_rel(DepsList, DepsMap,
            IntDepsRel0, IntDepsRel, ImplDepsRel0, ImplDepsRel),

        %
        % compute the trans-opt deps ordering, by doing an
        % approximate topological sort of the implementation deps,
        % and then finding the subset of those for which of those
        % we have (or can make) trans-opt files.
        %
        relation__atsort(ImplDepsRel, ImplDepsOrdering0),
        maybe_output_module_order(ModuleName, ImplDepsOrdering0, !IO),
        list__map(set__to_sorted_list, ImplDepsOrdering0, ImplDepsOrdering),
        list__condense(ImplDepsOrdering, TransOptDepsOrdering0),
        globals__io_lookup_accumulating_option(intermod_directories,
            IntermodDirs, !IO),
        get_opt_deps(yes, TransOptDepsOrdering0, IntermodDirs, ".trans_opt",
            TransOptDepsOrdering, !IO),

        % relation__to_assoc_list(ImplDepsRel, ImplDepsAL),
        % print("ImplDepsAL:\n", !IO),
        % write_list(ImplDepsAL, "\n", print, !IO), nl(!IO),

        %
        % compute the indirect dependencies: they are equal to the
        % composition of the implementation dependencies
        % with the transitive closure of the implementation
        % dependencies.  (We used to take the transitive closure
        % of the interface dependencies, but we now include
        % implementation details in the interface files).
        %
        relation__tc(ImplDepsRel, TransImplDepsRel),
        relation__compose(ImplDepsRel, TransImplDepsRel, IndirectDepsRel),

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
        relation__tc(ImplDepsRel, IndirectOptDepsRel),

        % write_relations("Rel", IntDepsRel, TransIntDepsRel,
        %   ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel),

        generate_dependencies_write_d_files(DepsList, IntDepsRel, ImplDepsRel,
            IndirectDepsRel, IndirectOptDepsRel, TransOptDepsOrdering, DepsMap,
            !IO)
    ),
    %
    % For Java, the main target is actually a shell script which will
    % set CLASSPATH appropriately and invoke java on the appropriate
    % .class file.  Rather than generating an Mmake rule to build this
    % file when it is needed, we just generate this file "mmake depend"
    % time, since that is simpler and probably more efficient anyway.
    %
    globals__io_get_target(Target, !IO),
    ( Target = java ->
        create_java_shell_script(ModuleName, _Succeeded, !IO)
    ;
        true
    ).

%   % Output the various relations into a file which can be
%   % processed by the dot package to draw the relations.
% :- pred write_relations(string::in, relation(sym_name)::in,
%   relation(sym_name)::in, relation(sym_name)::in,
%   relation(sym_name)::in, relation(sym_name)::in, io::di, io::uo) is det.
%
% write_relations(FileName, IntDepsRel, TransIntDepsRel,
%       ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel) -->
%   io__open_output(FileName, Result),
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
% :- pred write_relation(io__output_stream::in, string::in,
%   relation(sym_name)::in, io::di, io::uo) is det.
%
% write_relation(Stream, Name, Relation) -->
%   io__write_string(Stream, "digraph " ++ Name ++ " {\n"),
%   io__write_string(Stream, "label=\"" ++ Name ++ "\";\n"),
%   io__write_string(Stream, "center=true;\n"),
%   relation__traverse(Relation, write_node(Stream), write_edge(Stream)),
%   io__write_string(Stream, "}\n").
%
% :- pred write_node(io__output_stream::in, sym_name::in, io::di, io::uo)
%   is det.
%
% write_node(Stream, Node) -->
%   { sym_name_to_string(Node, "__", NodeStr) },
%   io__write_string(Stream, NodeStr),
%   io__write_string(Stream, ";\n").
%
% :- pred write_edge(io__output_stream::in, sym_name::in, sym_name::in,
%   io::di, io::uo) is det.
%
% write_edge(Stream, A, B) -->
%   { sym_name_to_string(A, "__", AStr) },
%   { sym_name_to_string(B, "__", BStr) },
%   io__write_string(Stream, AStr),
%   io__write_string(Stream, " -> "),
%   io__write_string(Stream, BStr),
%   io__write_string(Stream, ";\n").

:- pred maybe_output_module_order(module_name::in, list(set(module_name))::in,
    io::di, io::uo) is det.

maybe_output_module_order(Module, DepsOrdering, !IO) :-
    globals__io_lookup_bool_option(generate_module_order, Order, !IO),
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    (
        Order = yes,
        module_name_to_file_name(Module, ".order", yes, OrdFileName, !IO),
        maybe_write_string(Verbose, "% Creating module order file `", !IO),
        maybe_write_string(Verbose, OrdFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        io__open_output(OrdFileName, OrdResult, !IO),
        (
            OrdResult = ok(OrdStream),
            io__write_list(OrdStream, DepsOrdering, "\n\n",
                write_module_scc(OrdStream), !IO),
            io__close_output(OrdStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            OrdResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io__error_message(IOError, IOErrorMessage),
            string__append_list(["error opening file `", OrdFileName,
                "' for output: ", IOErrorMessage], OrdMessage),
            report_error(OrdMessage, !IO)
        )
    ;
        Order = no
    ).

:- pred write_module_scc(io__output_stream::in, set(module_name)::in,
    io::di, io::uo) is det.

write_module_scc(Stream, SCC0, !IO) :-
    set__to_sorted_list(SCC0, SCC),
    io__write_list(Stream, SCC, "\n", prog_out__write_sym_name, !IO).

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
        globals__io_lookup_bool_option(intermodule_optimization, Intermod,
            !IO),
        (
            Intermod = yes,
            % Be conservative with inter-module optimization
            % -- assume a module depends on the `.int', `.int2'
            % and `.opt' files for all transitively imported
            % modules.
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

        globals__io_get_target(Target, !IO),
        ( Target = c, Lang = c
        ; Target = asm, Lang = c
        ; Target = java, Lang = java
        ; Target = il, Lang = il
        ),
        % Assume we need the `.mh' files for all imported
        % modules (we will if they define foreign types).
        ForeignImports = list__map(
            (func(ThisDep) = foreign_import_module(Lang, ThisDep,
                term__context_init)),
            IndirectOptDeps),
        !:Module = !.Module ^ foreign_import_module_info := ForeignImports,

        module_imports_set_int_deps(IntDeps, !Module),
        module_imports_set_impl_deps(ImplDeps, !Module),
        module_imports_set_indirect_deps(IndirectDeps, !Module),

        %
        % Compute the trans-opt dependencies for this module.
        % To avoid the possibility of cycles, each module is
        % only allowed to depend on modules that occur later
        % than it in the TransOptOrder.
        %
        FindModule = (pred(OtherModule::in) is semidet :-
            ModuleName \= OtherModule
        ),
        list__takewhile(FindModule, TransOptOrder, _, TransOptDeps0),
        ( TransOptDeps0 = [ _ | TransOptDeps1 ] ->
            % The module was found in the list
            TransOptDeps = TransOptDeps1
        ;
            TransOptDeps = []
        ),

        %
        % Note that even if a fatal error occured for one of the files
        % that the current Module depends on, a .d file is still
        % produced, even though it probably contains incorrect
        % information.
        module_imports_get_error(!.Module, Error),
        ( Error \= fatal_module_errors ->
            write_dependency_file(!.Module, set__list_to_set(IndirectOptDeps),
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
    svrelation__add_element(ModuleName, ModuleKey, DepsRel0, DepsRel),
    relation__lookup_key_set_from(DepsRel, ModuleKey, DepsKeysSet),
    foldl(
        (pred(Key::in, Deps0::in, [Dep | Deps0]::out) is det :-
            relation__lookup_key(DepsRel, Key, Dep)
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

:- pred generate_deps_map(list(module_name)::in, deps_map::in, deps_map::out,
    io::di, io::uo) is det.

generate_deps_map([], !DepsMap, !IO).
generate_deps_map([Module | Modules], !DepsMap, !IO) :-
        % Look up the module's dependencies, and determine whether
        % it has been processed yet.
    lookup_dependencies(Module, no, Done, !DepsMap, ModuleImports, !IO),
        % If the module hadn't been processed yet, then add its
        % imports, parents, and public children to the list of
        % dependencies we need to generate, and mark it as
        % having been processed.
    (
        Done = no,
        map__set(!.DepsMap, Module, deps(yes, ModuleImports), !:DepsMap),
        ForeignImportedModules =
            list__map(
                (func(foreign_import_module(_, ImportedModule, _))
                    = ImportedModule),
                ModuleImports ^ foreign_import_module_info),
        list__condense(
            [ModuleImports ^ parent_deps,
            ModuleImports ^ int_deps,
            ModuleImports ^ impl_deps,
            ModuleImports ^ public_children, % a.k.a. incl_deps
            ForeignImportedModules,
            Modules],
            Modules1)
    ;
        Done = yes,
        Modules1 = Modules
    ),
        % Recursively process the remaining modules
    generate_deps_map(Modules1, !DepsMap, !IO).

    % Construct a pair of dependency relations (the interface dependencies
    % and the implementation dependencies) for all the modules in the
    % program.
:- pred deps_list_to_deps_rel(list(deps)::in, deps_map::in,
    deps_rel::in, deps_rel::out, deps_rel::in, deps_rel::out) is det.

deps_list_to_deps_rel([], _, !IntRel, !ImplRel).
deps_list_to_deps_rel([Deps | DepsList], DepsMap, !IntRel, !ImplRel) :-
    Deps = deps(_, ModuleImports),
    ModuleError = ModuleImports ^ error,
    ( ModuleError \= fatal_module_errors ->
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
        svrelation__add_element(ModuleName, IntModuleKey, !IntRel),
        add_int_deps(IntModuleKey, ModuleImports, !IntRel),
        add_parent_impl_deps_list(DepsMap, IntModuleKey, ParentDeps, !IntRel),

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
        svrelation__add_element(ModuleName, ImplModuleKey, !ImplRel),
        add_impl_deps(ImplModuleKey, ModuleImports, !ImplRel),
        add_parent_impl_deps_list(DepsMap, ImplModuleKey, ParentDeps, !ImplRel)
    ;
        true
    ),
    deps_list_to_deps_rel(DepsList, DepsMap, !IntRel, !ImplRel).

    % add interface dependencies to the interface deps relation
    %
:- pred add_int_deps(relation_key::in, module_imports::in,
    deps_rel::in, deps_rel::out) is det.

add_int_deps(ModuleKey, ModuleImports, Rel0, Rel) :-
    AddDep = add_dep(ModuleKey),
    list__foldl(AddDep, ModuleImports ^ parent_deps, Rel0, Rel1),
    list__foldl(AddDep, ModuleImports ^ int_deps, Rel1, Rel).

    % add direct implementation dependencies for a module to the
    % impl. deps relation
    %
:- pred add_impl_deps(relation_key::in, module_imports::in,
    deps_rel::in, deps_rel::out) is det.

add_impl_deps(ModuleKey, ModuleImports, !Rel) :-
    % the implementation dependencies are a superset of the
    % interface dependencies, so first we add the interface deps
    add_int_deps(ModuleKey, ModuleImports, !Rel),
    % then we add the impl deps
    module_imports_get_impl_deps(ModuleImports, ImplDeps),
    list__foldl(add_dep(ModuleKey), ImplDeps, !Rel).

    % Add parent implementation dependencies for the given Parent module
    % to the impl. deps relation values for the given ModuleKey.
    %
:- pred add_parent_impl_deps(deps_map::in, relation_key::in, module_name::in,
    deps_rel::in, deps_rel::out) is det.

add_parent_impl_deps(DepsMap, ModuleKey, Parent, !Rel) :-
    map__lookup(DepsMap, Parent, deps(_, ParentModuleImports)),
    add_impl_deps(ModuleKey, ParentModuleImports, !Rel).

:- pred add_parent_impl_deps_list(deps_map::in, relation_key::in,
    list(module_name)::in, deps_rel::in, deps_rel::out) is det.

add_parent_impl_deps_list(DepsMap, ModuleKey, Parents, !Rel) :-
    list__foldl(add_parent_impl_deps(DepsMap, ModuleKey), Parents, !Rel).

    % Add a single dependency to a relation.
    %
:- pred add_dep(relation_key::in, T::in, relation(T)::in, relation(T)::out)
    is det.

add_dep(ModuleRelKey, Dep, !Rel) :-
    svrelation__add_element(Dep, DepRelKey, !Rel),
    svrelation__add(ModuleRelKey, DepRelKey, !Rel).

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
    ( list__last(Ancestors, Parent) ->
        map__lookup(DepsMap, ModuleName, deps(_, ModuleImports)),
        map__lookup(DepsMap, Parent, deps(_, ParentImports)),
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
:- pred generate_dependencies_write_dv_file(file_name::in, module_name::in,
    deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_dv_file(SourceFileName, ModuleName, DepsMap,
        !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    module_name_to_file_name(ModuleName, ".dv", yes, DvFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DvFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io__open_output(DvFileName, DvResult, !IO),
    (
        DvResult = ok(DvStream),
        generate_dv_file(SourceFileName, ModuleName, DepsMap, DvStream, !IO),
        io__close_output(DvStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        DvResult = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io__error_message(IOError, IOErrorMessage),
        string__append_list(["error opening file `", DvFileName,
            "' for output: ", IOErrorMessage], DvMessage),
        report_error(DvMessage, !IO)
    ).

    % Write out the `.dep' file, using the information collected in the
    % deps_map data structure.
:- pred generate_dependencies_write_dep_file(file_name::in, module_name::in,
    deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_dep_file(SourceFileName, ModuleName, DepsMap,
        !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    module_name_to_file_name(ModuleName, ".dep", yes, DepFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DepFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io__open_output(DepFileName, DepResult, !IO),
    (
        DepResult = ok(DepStream),
        generate_dep_file(SourceFileName, ModuleName, DepsMap,
            DepStream, !IO),
        io__close_output(DepStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        DepResult = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io__error_message(IOError, IOErrorMessage),
        string__append_list(["error opening file `", DepFileName,
            "' for output: ", IOErrorMessage], DepMessage),
        report_error(DepMessage, !IO)
    ).

:- pred generate_dv_file(file_name::in, module_name::in, deps_map::in,
    io__output_stream::in, io::di, io::uo) is det.

generate_dv_file(SourceFileName, ModuleName, DepsMap, DepStream, !IO) :-
    io__write_string(DepStream,
        "# Automatically generated dependency variables for module `", !IO),
    sym_name_to_string(ModuleName, ModuleNameString),
    io__write_string(DepStream, ModuleNameString, !IO),
    io__write_string(DepStream, "'\n", !IO),
    io__write_string(DepStream, "# generated from source file `", !IO),
    io__write_string(DepStream, SourceFileName, !IO),
    io__write_string(DepStream, "'\n", !IO),

    library__version(Version),
    io__write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io__write_string(DepStream, Version, !IO),
    io__write_string(DepStream, ".\n\n", !IO),

    map__keys(DepsMap, Modules0),
    select_ok_modules(Modules0, DepsMap, Modules),

    module_name_to_make_var_name(ModuleName, MakeVarName),
    list__map(get_source_file(DepsMap), Modules, SourceFiles0),
    list__sort_and_remove_dups(SourceFiles0, SourceFiles),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".ms =", !IO),
    write_file_dependencies_list(SourceFiles, ".m", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".errs =", !IO),
    write_file_dependencies_list(SourceFiles, ".err", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".mods =", !IO),
    write_dependencies_list(Modules, "", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    globals__io_get_target(Target, !IO),
    ( Target = il ->
        ForeignModulesAndExts = foreign_modules(Modules, DepsMap)
    ;
        ForeignModulesAndExts = []
    ),
    ForeignModules = assoc_list__keys(ForeignModulesAndExts),
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".foreign =", !IO),
    write_dependencies_list(ForeignModules, "", DepStream, !IO),
    io__write_string(DepStream, "\n\n", !IO),

    globals__io_lookup_bool_option(assume_gmake, Gmake, !IO),
    (
        Gmake = yes,
        string__append(MakeVarName, ".mods", ModsVarName),
        Basis = yes(ModsVarName - ""),

        string__append(MakeVarName, ".foreign", ForeignVarName),
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

    list__map_foldl(MakeFileName, ForeignModulesAndExts, ForeignFileNames,
        !IO),

        % .foreign_cs are the source files which have had
        % foreign code placed in them.
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".foreign_cs = ", !IO),
    write_file_dependencies_list(ForeignFileNames, "", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

        % The dlls which contain the foreign_code.
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".foreign_dlls = ", !IO),
    write_compact_dependencies_list(ForeignModules, "$(dlls_subdir)",
        ".dll", ForeignBasis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".init_cs = ", !IO),
    write_compact_dependencies_list(Modules, "$(cs_subdir)", ".c",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".cs = $(", !IO),
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".init_cs) ", !IO),
    write_extra_link_dependencies_list(ExtraLinkObjs, ".c", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".dlls = ", !IO),
    write_compact_dependencies_list(Modules, "$(dlls_subdir)", ".dll",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_ss = ", !IO),
    write_compact_dependencies_list(Modules, "$(ss_subdir)", ".s",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_pic_ss = ", !IO),
    write_compact_dependencies_list(Modules, "$(pic_ss_subdir)", ".pic_s",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_s_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(s_dates_subdir)",
        ".s_date", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_pic_s_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(pic_s_dates_subdir)",
        ".pic_s_date", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_os = ", !IO),
    write_compact_dependencies_list(Modules, "$(os_subdir)", ".$O",
        Basis, DepStream, !IO),
    write_extra_link_dependencies_list(ExtraLinkObjs, ".$O", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_pic_os = ", !IO),
    write_compact_dependencies_list(Modules, "$(os_subdir)",
        ".$(EXT_FOR_PIC_OBJECTS)", Basis, DepStream, !IO),
    write_extra_link_dependencies_list(ExtraLinkObjs,
        ".$(EXT_FOR_PIC_OBJECTS)", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    IsNested = (pred(Mod::in) is semidet :-
        get_submodule_kind(Mod, DepsMap) = nested_submodule
    ),
    (
        % For --target asm, we only generate separate object files
        % for top-level modules and separate sub-modules, not for
        % nested sub-modules.
        Target = asm,
        list__filter(IsNested, Modules, NestedModules, MainModules),
        NestedModules = [_ | _]
    ->
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".ss = ", !IO),
        write_dependencies_list(MainModules, ".s", DepStream, !IO),
        io__write_string(DepStream, "\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".pic_ss = ", !IO),
        write_dependencies_list(MainModules, ".pic_s", DepStream, !IO),
        io__write_string(DepStream, "\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".s_dates = ", !IO),
        write_dependencies_list(MainModules, ".s_date", DepStream, !IO),
        io__write_string(DepStream, "\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".pic_s_dates = ", !IO),
        write_dependencies_list(MainModules, ".pic_s_date", DepStream, !IO),
        io__write_string(DepStream, "\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".os = ", !IO),
        write_dependencies_list(MainModules, ".$O", DepStream, !IO),
        write_extra_link_dependencies_list(ExtraLinkObjs, ".$O", DepStream,
            !IO),
        io__write_string(DepStream, "\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".pic_os = ", !IO),
        write_dependencies_list(MainModules, ".$(EXT_FOR_PIC_OBJECTS)",
            DepStream, !IO),
        write_extra_link_dependencies_list(ExtraLinkObjs,
            ".$(EXT_FOR_PIC_OBJECTS)", DepStream, !IO),
        io__write_string(DepStream, "\n", !IO)
    ;
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".ss = $(", !IO),
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".all_ss)\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".pic_ss = $(", !IO),
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".all_pic_ss)\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".s_dates = $(", !IO),
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".all_s_dates)\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".pic_s_dates = $(", !IO),
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".all_pic_s_dates)\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".os = $(", !IO),
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".all_os)\n", !IO),

        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".pic_os = $(", !IO),
        io__write_string(DepStream, MakeVarName, !IO),
        io__write_string(DepStream, ".all_pic_os)\n", !IO)
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
    io__write_strings(DepStream, [
        "ifeq ($(TARGET_ASM),yes)\n",
        MakeVarName, ".cs_or_ss =$(", MakeVarName, ".ss)\n",
        "else\n",
        MakeVarName, ".cs_or_ss =$(", MakeVarName, ".cs)\n",
        "endif\n\n"
    ], !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".rlos = ", !IO),
    write_compact_dependencies_list(Modules, "$(rlos_subdir)", ".rlo",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".useds = ", !IO),
    write_compact_dependencies_list(Modules, "$(useds_subdir)", ".used",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".ils = ", !IO),
    write_compact_dependencies_list(Modules, "$(ils_subdir)", ".il",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".javas = ", !IO),
    write_compact_dependencies_list(Modules, "$(javas_subdir)", ".java",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".classes = ", !IO),
    write_compact_dependencies_list(Modules, "$(classes_subdir)", ".class",
        Basis, DepStream, !IO),
    io__write_string(DepStream, " ", !IO),
    % The Java compiler creates a .class file for each class
    % within the original .java file.  The filenames of all
    % these can be matched with `module\$*.class', hence the
    % "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    % XXX This relies on GNU Make.
    io__write_string(DepStream, "$(wildcard ", !IO),
    write_compact_dependencies_list(Modules, "$(classes_subdir)",
        "\\$$*.class", Basis, DepStream, !IO),
    io__write_string(DepStream, ")\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".dirs = ", !IO),
    write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".num_splits = ", !IO),
    write_compact_dependencies_list(Modules, "$(num_splits_subdir)",
        ".num_split", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".dir_os = ", !IO),
    write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir/*.$O",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(dates_subdir)", ".date",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".date0s = ", !IO),
    write_compact_dependencies_list(Modules, "$(date0s_subdir)", ".date0",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".date3s = ", !IO),
    write_compact_dependencies_list(Modules, "$(date3s_subdir)", ".date3",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".optdates = ", !IO),
    write_compact_dependencies_list(Modules, "$(optdates_subdir)",
        ".optdate", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".trans_opt_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(trans_opt_dates_subdir)",
        ".trans_opt_date", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".c_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(c_dates_subdir)",
        ".c_date", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".il_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(il_dates_subdir)",
        ".il_date", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".java_dates = ", !IO),
    write_compact_dependencies_list(Modules, "$(java_dates_subdir)",
        ".java_date", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".ds = ", !IO),
    write_compact_dependencies_list(Modules, "$(ds_subdir)", ".d",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".module_deps = ", !IO),
    write_compact_dependencies_list(Modules, "$(module_deps_subdir)",
        module_dep_file_extension, Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".mihs = ", !IO),
    globals__io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
    (
        HighLevelCode = yes,
        ( ( Target = c ; Target = asm ) ->
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
            % For the IL and Java targets, currently we don't
            % generate `.mih' files at all; although perhaps
            % we should...
            true
        )
    ;
        % For the LLDS back-end, we don't use `.mih' files at all
        HighLevelCode = no
    ),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".mhs = ", !IO),
    ( ( Target = c ; Target = asm ) ->
        write_compact_dependencies_list(Modules, "", ".mh",
            Basis, DepStream, !IO)
    ;
        true
    ),
    io__write_string(DepStream, "\n", !IO),

    % The `<module>.all_mihs' variable is like `<module>.mihs' except
    % that it contains header files for all the modules, regardless
    % of the grade or --target option.  It is used by the rule for
    % `mmake realclean', which should remove anything that could have
    % been automatically generated, even if the grade or --target option
    % has changed.
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_mihs = ", !IO),
    write_compact_dependencies_list(Modules, "$(mihs_subdir)", ".mih",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except
    % that it contains header files for all the modules, as for
    % `<module>.all_mihs' above.
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".all_mhs = ", !IO),
    write_compact_dependencies_list(Modules, "", ".mh", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".ints = ", !IO),
    write_compact_dependencies_list(Modules, "$(ints_subdir)", ".int",
        Basis, DepStream, !IO),
    write_compact_dependencies_separator(Basis, DepStream, !IO),
    write_compact_dependencies_list(Modules, "$(int2s_subdir)", ".int2",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    % `.int0' files are only generated for modules with sub-modules.
    %
    % XXX The dependencies for nested submodules are wrong - we
    % currently end up generating .int0 files for nested submodules that
    % don't have any children (the correct thing is done for separate
    % submodules).  The following commented out code generates the
    % correct rules for .int0 files; it and the line below can be
    % uncommented when the dependency problem is fixed.
    %
    % ModulesWithSubModules = list__filter(
    %   (pred(Module::in) is semidet :-
    %       map__lookup(DepsMap, Module, deps(_, ModuleImports)),
    %       ModuleImports ^ children = [_ | _]
    %   ), Modules),
    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".int0s = ", !IO),
    %
    % These next two lines are a workaround for the bug described above.
    %
    write_compact_dependencies_list(Modules, "$(ints_subdir)", ".int0",
        Basis, DepStream, !IO),
    write_compact_dependencies_separator(Basis, DepStream, !IO),
    %
    % End of workaround - it can be deleted when the bug described above
    % is fixed.  When that happens the following line needs to be
    % uncommented.
    %
    %write_dependencies_list(ModulesWithSubModules, ".int0", DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".int3s = ", !IO),
    write_compact_dependencies_list(Modules, "$(int3s_subdir)", ".int3",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".opts = ", !IO),
    write_compact_dependencies_list(Modules, "$(opts_subdir)", ".opt",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".trans_opts = ", !IO),
    write_compact_dependencies_list(Modules, "$(trans_opts_subdir)",
        ".trans_opt", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".analysiss = ", !IO),
    write_compact_dependencies_list(Modules, "$(analysiss_subdir)",
        ".analysis", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".requests = ", !IO),
    write_compact_dependencies_list(Modules, "$(requests_subdir)",
        ".request", Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".schemas = ", !IO),
    write_compact_dependencies_list(Modules, "", ".base_schema",
        Basis, DepStream, !IO),
    io__write_string(DepStream, " ", !IO),
    write_compact_dependencies_list(Modules, "", ".derived_schema",
        Basis, DepStream, !IO),
    io__write_string(DepStream, "\n", !IO),

    io__write_string(DepStream, MakeVarName, !IO),
    io__write_string(DepStream, ".profs = ", !IO),
    write_compact_dependencies_list(Modules, "", ".prof", Basis, DepStream,
        !IO),
    io__write_string(DepStream, "\n\n", !IO).

:- pred generate_dep_file(file_name::in, module_name::in, deps_map::in,
    io__output_stream::in, io::di, io::uo) is det.

generate_dep_file(SourceFileName, ModuleName, DepsMap, DepStream, !IO) :-
    io__write_string(DepStream,
        "# Automatically generated dependencies for module `", !IO),
    sym_name_to_string(ModuleName, ModuleNameString),
    io__write_string(DepStream, ModuleNameString, !IO),
    io__write_string(DepStream, "'\n", !IO),
    io__write_string(DepStream,
        "# generated from source file `", !IO),
    io__write_string(DepStream, SourceFileName, !IO),
    io__write_string(DepStream, "'\n", !IO),

    library__version(Version),
    io__write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io__write_string(DepStream, Version, !IO),
    io__write_string(DepStream, ".\n\n", !IO),

    map__keys(DepsMap, Modules0),
    select_ok_modules(Modules0, DepsMap, Modules),

    module_name_to_make_var_name(ModuleName, MakeVarName),

    module_name_to_file_name(ModuleName, ".init", yes, InitFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.s", no, InitAsmFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.$O", yes, InitObjFileName, !IO),
    module_name_to_file_name(ModuleName, "_init.pic_o", yes,
        InitPicObjFileName, !IO),

    % Note we have to do some ``interesting'' hacks to get
    % `$(ALL_MLLIBS_DEP)' to work in the dependency list
    % (and not complain about undefined variables).
    % These hacks rely on features of GNU Make, so should not be used
    % if we cannot assume we are using GNU Make.
    globals__io_lookup_bool_option(assume_gmake, Gmake, !IO),
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
    io__write_strings(DepStream, [
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

    globals__io_get_target(Target, !IO),
    (
        Gmake = yes,
        Rules = IfIL ++ ILMainRule ++ Else ++
            IfJava ++ JavaMainRule ++ Else2 ++
            MainRule ++ EndIf2 ++ EndIf
    ;
        Gmake = no,
        ( Target = il ->
            Rules = ILMainRule
        ; Target = java ->
            Rules = JavaMainRule
        ;
            Rules = MainRule
        )
    ),
    io__write_strings(DepStream, Rules, !IO),

    module_name_to_file_name(ModuleName, ".split", yes, SplitExeFileName, !IO),
    module_name_to_file_name(ModuleName, ".split.$A", yes, SplitLibFileName,
        !IO),
    io__write_strings(DepStream, [
        SplitExeFileName, " : ", SplitLibFileName, " ", InitObjFileName, " ",
            All_MLObjsString, " ", All_MLLibsDepString, "\n",
        "\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) ",
            "-o ", SplitExeFileName, " ", InitObjFileName, " \\\n",
        "\t\t", SplitLibFileName, " ", All_MLObjsString, " $(ALL_MLLIBS)\n\n"
    ], !IO),

    io__write_strings(DepStream, [
        SplitLibFileName, " : $(", MakeVarName, ".dir_os) ",
            All_MLObjsString, "\n",
        "\trm -f ", SplitLibFileName, "\n",
        "\t$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT) ",
        SplitLibFileName, " ", All_MLObjsString, "\n",
        "\tfind $(", MakeVarName, ".dirs) -name ""*.$O"" -print | \\\n",
        "\t\txargs $(AR) q ", SplitLibFileName, "\n",
        "\t$(RANLIB) $(ALL_RANLIBFLAGS) ", SplitLibFileName, "\n\n"
    ], !IO),

    globals__io_lookup_bool_option(intermodule_optimization, Intermod, !IO),
    (
        Intermod = yes,
        string__append_list(["$(", MakeVarName, ".opts) "], MaybeOptsVar)
    ;
        Intermod = no,
        MaybeOptsVar = ""
    ),
    globals__io_lookup_bool_option(transitive_optimization, TransOpt, !IO),
    (
        TransOpt = yes,
        string__append_list(["$(", MakeVarName, ".trans_opts) "],
            MaybeTransOptsVar)
    ;
        TransOpt = no,
        MaybeTransOptsVar = ""
    ),
    globals__io_lookup_bool_option(generate_mmc_make_module_dependencies,
        MmcMakeDeps, !IO),
    (
        MmcMakeDeps = yes,
        string__append_list(["$(", MakeVarName, ".module_deps) "],
            MaybeModuleDepsVar)
    ;
        MmcMakeDeps = no,
        MaybeModuleDepsVar = ""
    ),

    module_name_to_lib_file_name("lib", ModuleName, "", no, LibTargetName, !IO),
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
        ( Target = il ->
            LibRules = ILLibRule
        ; Target = java ->
            LibRules = JavaLibRule
        ;
            LibRules = LibRule
        )
    ),
    io__write_strings(DepStream, [
        ".PHONY : ", LibTargetName, "\n" |
        LibRules
    ], !IO),

    io__write_strings(DepStream, [
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

    io__write_strings(DepStream, [
        LibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
        "\trm -f ", LibFileName, "\n",
        "\t$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)", LibFileName, " ",
            "$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
        "\t$(RANLIB) $(ALL_RANLIBFLAGS) ", LibFileName, "\n\n"
    ], !IO),

    ClassFiles = "$(" ++ MakeVarName ++ ".classes)",
    list_class_files_for_jar(ModuleName, ClassFiles, ListClassFiles, !IO),
    io__write_strings(DepStream, [
        JarFileName, " : ", "$(", MakeVarName, ".classes)\n",
        "\t$(JAR) $(JAR_CREATE_FLAGS) ", JarFileName, " ",
        ListClassFiles, "\n\n"
    ], !IO),

    module_name_to_file_name(ModuleName, ".dep", no, DepFileName, !IO),
    module_name_to_file_name(ModuleName, ".dv", no, DvFileName, !IO),
    io__write_strings(DepStream, [
        InitFileName, " : ", DepFileName, "\n",
        "\techo > ", InitFileName, "\n"
    ], !IO),
    list__foldl(append_to_init_list(DepStream, InitFileName), Modules, !IO),

    % $(EXTRA_INIT_COMMAND) should expand to a command to
    % generate extra entries in the `.init' file for a library.
    % It may expand to the empty string.
    io__write_string(DepStream, "\t$(EXTRA_INIT_COMMAND) >> ", !IO),
    io__write_string(DepStream, InitFileName, !IO),
    io__write_string(DepStream, "\n", !IO),

    % The `force-module_init' dependency forces the commands for
    % the `module_init.c' rule to be run every time the rule
    % is considered.
    sym_name_to_string(ModuleName, ".", ModuleFileName),
    ForceC2InitTarget = "force-" ++ ModuleFileName ++ "_init",
    TmpInitCFileName = InitCFileName ++ ".tmp",
    io__write_strings(DepStream, [
        ForceC2InitTarget, " :\n\n",
        InitCFileName, " : ", ForceC2InitTarget, "\n",
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
        map__member(DepsMap, _, deps(_, Imports)),
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

    io__write_strings(DepStream, [
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
    io__write_strings(DepStream,
        [".PHONY : ", LibInstallOptsTargetName, "\n",
        LibInstallOptsTargetName, " : "], !IO),
    (
        Intermod = no,
        TransOpt = no
    ->
        io__write_string(DepStream, "\n\t@:\n\n", !IO)
    ;
        io__write_strings(DepStream, [
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
    io__write_strings(DepStream, [
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
    io__write_strings(DepStream, [
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
    module_name_to_file_name(ModuleName, ".rlos", no, RLOsTargetName, !IO),
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

    io__write_strings(DepStream, [
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
        ".PHONY : ", RLOsTargetName, "\n",
        RLOsTargetName, " : $(", MakeVarName, ".rlos)\n\n",
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
    io__write_strings(DepStream, [
        "clean_local : ", CleanTargetName, "\n"
    ], !IO),
    io__write_strings(DepStream, [
        ".PHONY : ", CleanTargetName, "\n",
        CleanTargetName, " :\n",
        "\t-echo $(", MakeVarName, ".dirs) | xargs rm -rf \n",
        "\t-echo $(", MakeVarName, ".num_splits) | xargs rm -rf \n",
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
        "\t-echo $(", MakeVarName, ".foreign_cs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".schemas) | xargs rm -f\n"
    ], !IO),

    io__write_string(DepStream, "\n", !IO),

    module_name_to_file_name(ModuleName, ".realclean", no,
        RealCleanTargetName, !IO),
    io__write_strings(DepStream, [
        "realclean_local : ", RealCleanTargetName, "\n"
    ], !IO),
    io__write_strings(DepStream, [
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
        "\t-echo $(", MakeVarName, ".ds) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".module_deps) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_mhs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_mihs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".dlls) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".foreign_dlls) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".classes) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".rlos) | xargs rm -f\n"
    ], !IO),
    io__write_strings(DepStream, [
        "\t-rm -f ",
            ExeFileName, "$(EXT_FOR_EXE) ",
            SplitExeFileName, " ",
            SplitLibFileName, " ",
            InitFileName, " ",
            LibFileName, " ",
            SharedLibFileName, " ",
            JarFileName, " ",
            DepFileName, " ",
            DvFileName, "\n\n"
    ], !IO).

:- pred get_source_file(deps_map::in, module_name::in, file_name::out) is det.

get_source_file(DepsMap, ModuleName, FileName) :-
    map__lookup(DepsMap, ModuleName, Deps),
    Deps = deps(_, ModuleImports),
    module_imports_get_source_file_name(ModuleImports, SourceFileName),
    ( string__remove_suffix(SourceFileName, ".m", SourceFileBase) ->
        FileName = SourceFileBase
    ;
        error("modules.m: source file name doesn't end in `.m'")
    ).

:- pred append_to_init_list(io__output_stream::in, file_name::in,
    module_name::in, io::di, io::uo) is det.

append_to_init_list(DepStream, InitFileName, Module, !IO) :-
    InitFuncName0 = make_init_name(Module),
    string__append(InitFuncName0, "init", InitFuncName),
    RLName = make_rl_data_name(Module),
    io__write_strings(DepStream, [
        "\techo ""INIT ", InitFuncName, """ >> ", InitFileName, "\n"
    ], !IO),
    globals__io_lookup_bool_option(aditi, Aditi, !IO),
    (
        Aditi = yes,
        io__write_strings(DepStream, [
            "\techo ""ADITI_DATA ", RLName, """ >> ", InitFileName, "\n"
        ], !IO)
    ;
        Aditi = no
    ).

%-----------------------------------------------------------------------------%

    % Find out which modules we need to generate C header files for,
    % assuming we're compiling with `--target asm'.
    %
:- func modules_that_need_headers(list(module_name), deps_map)
    = list(module_name).

modules_that_need_headers(Modules, DepsMap) =
    list__filter(module_needs_header(DepsMap), Modules).

    % Find out which modules will generate as external foreign
    % language files.
    % We return the module names and file extensions.
    %
:- func foreign_modules(list(module_name), deps_map)
    = assoc_list(module_name, string).

foreign_modules(Modules, DepsMap) = ForeignModules :-
    P = (pred(M::in, FMs::out) is semidet :-
        module_has_foreign(DepsMap, M, LangList),
        FMs = list__filter_map((func(L) = (NewM - Ext) is semidet :-
            NewM = foreign_language_module_name(M, L),
            Ext = foreign_language_file_extension(L)
        ), LangList)
    ),
    list__filter_map(P, Modules, ForeignModulesList),
    ForeignModules = list__condense(ForeignModulesList).

    % Succeed iff we need to generate a C header file for the specified
    % module, assuming we're compiling with `--target asm'.
    %
:- pred module_needs_header(deps_map::in, module_name::in) is semidet.

module_needs_header(DepsMap, Module) :-
    map__lookup(DepsMap, Module, deps(_, ModuleImports)),
    ModuleImports ^ foreign_code = contains_foreign_code(Langs),
    set__member(c, Langs).

    % Succeed iff we need to generate a foreign language output file
    % for the specified module.
    %
:- pred module_has_foreign(deps_map::in, module_name::in,
    list(foreign_language)::out) is semidet.

module_has_foreign(DepsMap, Module, LangList) :-
    map__lookup(DepsMap, Module, deps(_, ModuleImports)),
    ModuleImports ^ foreign_code = contains_foreign_code(Langs),
    LangList = set__to_sorted_list(Langs).

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
    list__reverse(ExtraLinkObjs0, ExtraLinkObjs).

:- pred get_extra_link_objects_2(list(module_name)::in, deps_map::in,
    compilation_target::in,
    assoc_list(file_name, module_name)::in,
    assoc_list(file_name, module_name)::out) is det.

get_extra_link_objects_2([], _DepsMap, _Target, !ExtraLinkObjs).
get_extra_link_objects_2([Module | Modules], DepsMap, Target,
        !ExtraLinkObjs) :-
    map__lookup(DepsMap, Module, deps(_, ModuleImports)),
    %
    % Handle object files for fact tables
    %
    FactDeps = ModuleImports ^ fact_table_deps,
    list__length(FactDeps, NumFactDeps),
    list__duplicate(NumFactDeps, Module, ModuleList),
    assoc_list__from_corresponding_lists(FactDeps, ModuleList, FactTableObjs),
    %
    % Handle object files for foreign code.
    % XXX currently we only support `C' foreign code.
    %
    (
        Target = asm,
        ModuleImports ^ foreign_code = contains_foreign_code(Langs),
        set__member(c, Langs)
    ->
        sym_name_to_string(Module, ".", FileName),
        NewLinkObjs = [(FileName ++ "__c_code") - Module | FactTableObjs]
    ;
        NewLinkObjs = FactTableObjs
    ),
    list__append(NewLinkObjs, !ExtraLinkObjs),
    get_extra_link_objects_2(Modules, DepsMap, Target, !ExtraLinkObjs).

:- type module_foreign_info --->
    module_foreign_info(
        used_foreign_languages          :: set(foreign_language),
        foreign_proc_languages          :: map(sym_name, foreign_language),
        all_foreign_import_module_info  :: foreign_import_module_info,
        module_contains_foreign_export  :: contains_foreign_export
    ).

:- pred get_item_list_foreign_code(globals::in, item_list::in,
    set(foreign_language)::out, foreign_import_module_info::out,
    contains_foreign_export::out) is det.

get_item_list_foreign_code(Globals, Items, LangSet, ForeignImports,
        ContainsPragmaExport) :-
    Info0 = module_foreign_info(set__init, map__init, [], no_foreign_export),
    list__foldl(get_item_foreign_code(Globals), Items, Info0, Info),
    Info = module_foreign_info(LangSet0, LangMap, ForeignImports,
        ContainsPragmaExport),
    ForeignProcLangs = map__values(LangMap),
    LangSet1 = set__insert_list(LangSet0, ForeignProcLangs),
    globals__lookup_bool_option(Globals, aditi, Aditi),
    % We generate a C constant containing the Aditi-RL code.
    LangSet = ( Aditi = yes -> set__insert(LangSet1, c) ; LangSet1 ).

:- pred get_item_foreign_code(globals::in, item_and_context::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_item_foreign_code(Globals, Item - Context, !Info) :-
    ( Item = pragma(_, Pragma) ->
        do_get_item_foreign_code(Globals, Pragma, Context, !Info)
    ; Item = mutable(_, _, _, _, _) ->
        % Mutables introduce foreign_procs, but mutable declarations
        % won't have been expanded by the time we get here so we need
        % to handle them separately.
        % XXX mutables are currently only implemented for the C backends
        % but we should handle the Java/IL backends here as well.
        % (See do_get_item_foreign_code for details/5).
        !:Info = !.Info ^ used_foreign_languages :=
            set__insert(!.Info ^ used_foreign_languages, c)
    ; ( Item = initialise(_, _, _) ; Item = finalise(_, _, _) ) ->
        % Intialise/finalise declarations introduce export pragmas, but
        % again they won't have been expanded by the time we get here.
        % XXX we don't currently support these on non-C backends. 
        Lang = c,
        !:Info = !.Info ^ used_foreign_languages :=
            set__insert(!.Info ^ used_foreign_languages, Lang),
        !:Info = !.Info ^ module_contains_foreign_export :=
            contains_foreign_export
    ;      
        true
    ).

:- pred do_get_item_foreign_code(globals::in, pragma_type::in,
    prog_context::in, module_foreign_info::in, module_foreign_info::out)
    is det.

do_get_item_foreign_code(Globals, Pragma, Context, Info0, Info) :-
    globals__get_backend_foreign_languages(Globals, BackendLangs),
    globals__get_target(Globals, Target),

    % The code here should match the way that mlds_to_gcc.m
    % decides whether or not to call mlds_to_c.m.  XXX Note
    % that we do NOT count foreign_decls here.  We only
    % link in a foreign object file if mlds_to_gcc called
    % mlds_to_c.m to generate it, which it will only do if
    % there is some foreign_code, not just foreign_decls.
    % Counting foreign_decls here causes problems with
    % intermodule optimization.
    (
        Pragma = foreign_code(Lang, _),
        list__member(Lang, BackendLangs)
    ->
        Info = Info0 ^ used_foreign_languages :=
            set__insert(Info0 ^ used_foreign_languages, Lang)
    ;
        Pragma = foreign_proc(Attrs, Name, _, _, _, _)
    ->
        NewLang = foreign_language(Attrs),
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
            ( list__member(NewLang, BackendLangs) ->
                Info = Info0 ^ foreign_proc_languages
                    ^ elem(Name) := NewLang
            ;
                Info = Info0
            )
        )
    ;
        % XXX `pragma export' should not be treated as
        % foreign, but currently mlds_to_gcc.m doesn't
        % handle that declaration, and instead just
        % punts it on to mlds_to_c.m, thus generating C
        % code for it, rather than assembler code.  So
        % we need to treat `pragma export' like the
        % other pragmas for foreign code.
        Pragma = export(_, _, _, _),
        list__member(c, BackendLangs)
    ->
        % XXX we assume lang = c for exports
        Lang = c,
        Info1 = Info0 ^ used_foreign_languages :=
            set__insert(Info0 ^ used_foreign_languages, Lang),
        Info = Info1 ^ module_contains_foreign_export :=
            contains_foreign_export
    ;
        Pragma = foreign_import_module(Lang, Import),
        list__member(Lang, BackendLangs)
    ->
        Info = Info0 ^ all_foreign_import_module_info :=
            [foreign_import_module(Lang, Import, Context) |
                Info0 ^ all_foreign_import_module_info]
    ;
        % We generate some C code for fact tables,
        % so we need to treat modules containing
        % fact tables as if they contain foreign
        % code.
        ( Target = asm
        ; Target = c
        ),
        Pragma = fact_table(_, _, _)
    ->
        Info = Info0 ^ used_foreign_languages :=
            set__insert(Info0 ^ used_foreign_languages, c)
    ;
        Info = Info0
    ).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(module_name)::in, deps_map::in,
    list(module_name)::out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
    map__lookup(DepsMap, Module, deps(_, ModuleImports)),
    module_imports_get_error(ModuleImports, Error),
    ( Error = fatal_module_errors ->
        Modules = Modules1
    ;
        Modules = [Module | Modules1]
    ),
    select_ok_modules(Modules0, DepsMap, Modules1).

%-----------------------------------------------------------------------------%

:- pred write_dependencies_list(list(module_name)::in, string::in,
    io__output_stream::in, io::di, io::uo) is det.

write_dependencies_list([], _, _, !IO).
write_dependencies_list([Module | Modules], Suffix, DepStream, !IO) :-
    module_name_to_file_name(Module, Suffix, no, FileName, !IO),
    io__write_string(DepStream, " \\\n\t", !IO),
    io__write_string(DepStream, FileName, !IO),
    write_dependencies_list(Modules, Suffix, DepStream, !IO).

referenced_dlls(Module, DepModules0) = Modules :-
    DepModules = [Module | DepModules0],

        % If we are not compiling a module in the mercury
        % std library then replace all the std library dlls with
        % one reference to mercury.dll.
    (
        Module = unqualified(Str),
        mercury_std_library_module(Str)
    ->
            % In the standard library we need to add the
            % runtime dlls.
        Modules = list__remove_dups(
            [unqualified("mercury_dotnet"), unqualified("mercury_il")
                | DepModules])
    ;
        F = (func(M) =
            (
                M = unqualified(S),
                mercury_std_library_module(S)
            ->
                unqualified("mercury")
            ;
                    % A sub module is located in the top level assembly.
                unqualified(outermost_qualifier(M))
            )
        ),
        Modules = list__remove_dups(list__map(F, DepModules))
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
        \+ mercury_std_library_module(Str)
    ->
        P = (pred(M::in) is semidet :-
            Str = outermost_qualifier(M),
            M \= Module
        ),
        list__filter(P, Modules0, Modules)
    ;
        Modules = []
    ).

:- pred write_dll_dependencies_list(list(module_name)::in,
    string::in, io__output_stream::in, io::di, io::uo) is det.

write_dll_dependencies_list(Modules, Prefix, DepStream, !IO) :-
    list__foldl(write_dll_dependency(DepStream, Prefix), Modules, !IO).

:- pred write_dll_dependency(io__output_stream::in, string::in,
    module_name::in, io::di, io::uo) is det.

write_dll_dependency(DepStream, Prefix, Module, !IO) :-
    module_name_to_file_name(Module, ".dll", no, FileName, !IO),
    io__write_string(DepStream, " \\\n\t", !IO),
    io__write_string(DepStream, Prefix, !IO),
    io__write_string(DepStream, FileName, !IO).

:- pred write_fact_table_dependencies_list(module_name::in,
    list(file_name)::in, string::in, io__output_stream::in,
    io::di, io::uo) is det.

write_fact_table_dependencies_list(_, [], _, _, !IO).
write_fact_table_dependencies_list(Module, [FactTable | FactTables], Suffix,
        DepStream, !IO) :-
    fact_table_file_name(Module, FactTable, Suffix, no, FileName, !IO),
    io__write_string(DepStream, " \\\n\t", !IO),
    io__write_string(DepStream, FileName, !IO),
    write_fact_table_dependencies_list(Module, FactTables, Suffix,
        DepStream, !IO).

:- pred write_extra_link_dependencies_list(
    assoc_list(file_name, module_name)::in, string::in,
    io__output_stream::in, io::di, io::uo) is det.

write_extra_link_dependencies_list([], _, _, !IO).
write_extra_link_dependencies_list([ExtraLink - Module | ExtraLinks], Suffix,
        DepStream, !IO) :-
    extra_link_obj_file_name(Module, ExtraLink, Suffix, no, FileName, !IO),
    io__write_string(DepStream, " \\\n\t", !IO),
    io__write_string(DepStream, FileName, !IO),
    write_extra_link_dependencies_list(ExtraLinks, Suffix, DepStream, !IO).

:- pred write_file_dependencies_list(list(string)::in, string::in,
    io__output_stream::in, io::di, io::uo) is det.

write_file_dependencies_list([], _, _, !IO).
write_file_dependencies_list([FileName | FileNames], Suffix, DepStream, !IO) :-
    io__write_string(DepStream, " \\\n\t", !IO),
    io__write_string(DepStream, FileName, !IO),
    io__write_string(DepStream, Suffix, !IO),
    write_file_dependencies_list(FileNames, Suffix, DepStream, !IO).

%-----------------------------------------------------------------------------%

:- pred write_compact_dependencies_list(list(module_name)::in, string::in,
    string::in, maybe(pair(string))::in, io__output_stream::in,
    io::di, io::uo) is det.

write_compact_dependencies_list(Modules, Prefix, Suffix, Basis, DepStream,
        !IO) :-
    (
        Basis = yes(VarName - OldSuffix),
        io__write_string(DepStream, "$(", !IO),
        io__write_string(DepStream, VarName, !IO),
        io__write_string(DepStream, ":%", !IO),
        io__write_string(DepStream, OldSuffix, !IO),
        io__write_string(DepStream, "=", !IO),
        io__write_string(DepStream, Prefix, !IO),
        io__write_string(DepStream, "%", !IO),
        io__write_string(DepStream, Suffix, !IO),
        io__write_string(DepStream, ")", !IO)
    ;
        Basis = no,
        write_dependencies_list(Modules, Suffix, DepStream, !IO)
    ).

:- pred write_compact_dependencies_separator(maybe(pair(string))::in,
    io__output_stream::in, io::di, io::uo) is det.

write_compact_dependencies_separator(no, _DepStream, !IO).
write_compact_dependencies_separator(yes(_), DepStream, !IO) :-
    io__write_string(DepStream, " ", !IO).

%-----------------------------------------------------------------------------%

    % Look up a module in the dependency map.
    % If we don't know its dependencies, read the module and
    % save the dependencies in the dependency map.

:- pred lookup_dependencies(module_name::in, bool::in, bool::out,
    deps_map::in, deps_map::out, module_imports::out,
    io::di, io::uo) is det.

lookup_dependencies(Module, Search, Done, !DepsMap, ModuleImports, !IO) :-
    ( map__search(!.DepsMap, Module, deps(DonePrime, ModuleImportsPrime)) ->
        Done = DonePrime,
        ModuleImports = ModuleImportsPrime
    ;
        read_dependencies(Module, Search, ModuleImportsList, !IO),
        list__foldl(insert_into_deps_map, ModuleImportsList, !DepsMap),
        map__lookup(!.DepsMap, Module, deps(Done, ModuleImports))
    ).

    % insert_into_deps_map/3:
    %
    % Insert a new entry into the deps_map.
    % If the module already occured in the deps_map, then we just
    % replace the old entry (presumed to be a dummy entry) with the
    % new one.
    %
    % This can only occur for sub-modules which have
    % been imported before their parent module was imported:
    % before reading a module and inserting it into the
    % deps map, we check if it was already there, but
    % when we read in the module, we try to insert not just
    % that module but also all the nested sub-modules inside
    % that module.  If a sub-module was previously imported,
    % then it may already have an entry in the deps_map.
    % However, unless the sub-module is defined both as a
    % separate sub-module and also as a nested sub-module,
    % the previous entry will be a dummy entry that we inserted
    % after trying to read the source file and failing.
    %
    % Note that the case where a module is defined as both a
    % separate sub-module and also as a nested sub-module is
    % caught in split_into_submodules.
    %
:- pred insert_into_deps_map(module_imports::in, deps_map::in, deps_map::out)
    is det.

insert_into_deps_map(ModuleImports, DepsMap0, DepsMap) :-
    module_imports_get_module_name(ModuleImports, ModuleName),
    map__set(DepsMap0, ModuleName, deps(no, ModuleImports), DepsMap).

    % Read a module to determine the (direct) dependencies
    % of that module and any nested sub-modules it contains.

:- pred read_dependencies(module_name::in, bool::in, list(module_imports)::out,
    io::di, io::uo) is det.

read_dependencies(ModuleName, Search, ModuleImportsList, !IO) :-
    read_mod_ignore_errors(ModuleName, ".m",
        "Getting dependencies for module", Search, no, Items0, Error,
        FileName0, _, !IO),
    (
        Items0 = [],
        Error = fatal_module_errors
    ->
        read_mod_ignore_errors(ModuleName, ".int",
            "Getting dependencies for module interface", Search,
            no, Items, _Error, FileName, _, !IO),
        SubModuleList = [ModuleName - Items]
    ;
        FileName = FileName0,
        Items = Items0,
        split_into_submodules(ModuleName, Items, SubModuleList, !IO)
    ),
    globals__io_get_globals(Globals, !IO),
    assoc_list__keys(SubModuleList, SubModuleNames),
    list__map(init_dependencies(FileName, ModuleName, SubModuleNames,
        Error, Globals), SubModuleList, ModuleImportsList).

init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
        Error, Globals, ModuleName - Items, ModuleImports) :-
    ParentDeps = get_ancestors(ModuleName),

    get_dependencies(Items, ImplImportDeps0, ImplUseDeps0),
    add_implicit_imports(Items, Globals, ImplImportDeps0, ImplImportDeps,
        ImplUseDeps0, ImplUseDeps),
    list__append(ImplImportDeps, ImplUseDeps, ImplementationDeps),

    get_interface(ModuleName, no, Items, InterfaceItems),
    get_dependencies(InterfaceItems, InterfaceImportDeps0, InterfaceUseDeps0),
    add_implicit_imports(InterfaceItems, Globals,
        InterfaceImportDeps0, InterfaceImportDeps,
        InterfaceUseDeps0, InterfaceUseDeps),
    list__append(InterfaceImportDeps, InterfaceUseDeps, InterfaceDeps),

    % we don't fill in the indirect dependencies yet
    IndirectDeps = [],

    get_children(Items, IncludeDeps),
    get_children(InterfaceItems, InterfaceIncludeDeps),

    ( ModuleName = SourceFileModuleName ->
        list__delete_all(NestedModuleNames, ModuleName, NestedDeps)
    ;
        NestedDeps = []
    ),

    get_fact_table_dependencies(Items, FactTableDeps),

    % Figure out whether the items contain foreign code.
    get_item_list_foreign_code(Globals, Items, LangSet, ForeignImports0,
        ContainsPragmaExport),
    ( set__empty(LangSet) ->
        ContainsForeignCode = no_foreign_code
    ;
        ContainsForeignCode = contains_foreign_code(LangSet)
    ),

    % If this module contains `:- pragma export' or
    % `:- pragma foreign_type' declarations, importing modules
    % may need to import its `.mh' file.
    get_foreign_self_imports(Items, SelfImportLangs),
    ForeignSelfImports = list__map(
        (func(Lang) = foreign_import_module(Lang, ModuleName,
            term__context_init)),
        SelfImportLangs),
    ForeignImports = ForeignSelfImports ++ ForeignImports0,

    %
    % Work out whether the items contain main/2.
    %
    (
        list__member(Item, Items),
        Item = pred_or_func(_, _, _, predicate, Name, [_, _], WithType,
            _, _, _, _, _) - _,
        unqualify_name(Name, "main"),

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
        [], Error, no, HasMain, dir__this_directory).

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
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(VeryVerbose, "% ", !IO),
    maybe_write_string(VeryVerbose, Descr, !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    maybe_write_string(VeryVerbose, FileName0, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),

    (
        Search = yes,
        globals__io_lookup_accumulating_option(search_directories, SearchDirs,
            !IO)
    ;
        Search = no,
        SearchDirs = [dir__this_directory]
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
        prog_io__read_module_if_changed(OpenFile, ModuleName,
            OldTimestamp, Error, MaybeFileName, ActualModuleName,
            Messages, Items, MaybeTimestamp0, !IO)
    ;
        MaybeOldTimestamp = no,
        prog_io__read_module(OpenFile, ModuleName,
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
            io__set_exit_status(1, !IO)
        ;
            Error = some_module_errors,
            maybe_write_string(VeryVerbose, "parse error(s).\n", !IO),
            io__set_exit_status(1, !IO)
        ;
            Error = no_module_errors,
            maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
        ),
        prog_out__write_messages(Messages, !IO)
    ).

read_mod_from_file(FileName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, ModuleName, MaybeTimestamp, !IO) :-
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(VeryVerbose, "% ", !IO),
    maybe_write_string(VeryVerbose, Descr, !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    maybe_write_string(VeryVerbose, FileName, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),
    string__append(FileName, Extension, FullFileName),
    ( dir__basename(FileName, BaseFileName0) ->
        BaseFileName = BaseFileName0
    ;
        BaseFileName = ""
    ),
    file_name_to_module_name(BaseFileName, DefaultModuleName),
    (
        Search = yes,
        globals__io_lookup_accumulating_option(search_directories,
            SearchDirs, !IO)
    ;
        Search = no,
        SearchDirs = [dir__this_directory]
    ),
    OpenFile = search_for_file(SearchDirs, FullFileName),
    prog_io__read_module(OpenFile, DefaultModuleName, ReturnTimestamp, Error,
        _, ModuleName, Messages, Items, MaybeTimestamp0, !IO),
    check_timestamp(FullFileName, MaybeTimestamp0, MaybeTimestamp, !IO),
    (
        Error = fatal_module_errors,
        maybe_write_string(VeryVerbose, "fatal error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        Error = some_module_errors,
        maybe_write_string(VeryVerbose, "parse error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        Error = no_module_errors,
        maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
    ),
    prog_out__write_messages(Messages, !IO).

:- pred check_timestamp(file_name::in, maybe(io__res(timestamp))::in,
    maybe(timestamp)::out, io::di, io::uo) is det.

check_timestamp(FileName, MaybeTimestamp0, MaybeTimestamp, !IO) :-
    (
        MaybeTimestamp0 = yes(ok(Timestamp)),
        MaybeTimestamp = yes(Timestamp)
    ;
        MaybeTimestamp0 = yes(error(IOError)),
        MaybeTimestamp = no,
        globals__io_lookup_bool_option(smart_recompilation, SmartRecompilation,
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
        error("modules.m: module is its own ancestor?")
    ; list__member(Ancestor, ModAncestors0) ->
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

        globals__io_lookup_bool_option(statistics, Statistics, !IO),
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
        !:Module = ((!.Module ^ items := ModItems)
            ^ parent_deps := ModAncestors)
            ^ error := ModError,
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
        ; list__member(Import, !.Module ^ parent_deps)
        ; list__member(Import, !.Module ^ int_deps)
        ; list__member(Import, ModImplementationImports0)
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

        globals__io_lookup_bool_option(statistics, Statistics, !IO),
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
        list__append(ModItems0, Items, ModItems),
        !:Module = ((!.Module ^ impl_deps := ModImplementationImports)
            ^ items := ModItems)
            ^ error := ModError,

        process_module_long_interfaces(ReadModules, NeedQualifier,
            Imports, Ext, IntStatusItem, ImpStatusItem,
            !IndirectImports, !ImplIndirectImports, !Module, !IO)
    ).

:- pred check_imports_accessibility(module_name::in, list(module_name)::in,
    item_list::in, io::di, io::uo) is det.

    %
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
check_imports_accessibility(ModuleName, Imports, Items, !IO) :-
    get_accessible_children(Items, AccessibleSubModules),
    list__foldl(check_module_accessibility(ModuleName,
        AccessibleSubModules, Items), Imports, !IO).

:- pred check_module_accessibility(module_name::in, list(module_name)::in,
    item_list::in, module_name::in, io::di, io::uo) is det.

check_module_accessibility(ModuleName, AccessibleSubModules, Items,
        ImportedModule, !IO) :-
    ( ImportedModule = qualified(ParentModule, SubModule) ->
        ( list__member(ImportedModule, AccessibleSubModules) ->
            true
        ;
            % The user attempted to import an inaccessible
            % sub-module, so report an error.
            % Unfortunately we didn't get passed the
            % context of the `import_module' or `use_module'
            % declaration(s), so we need to search the item
            % list again to find them.
            FindImports = (pred(Item::in) is semidet :-
                Item = module_defn(_, ModuleDefn) - _,
                ( ModuleDefn = import(module(Mods))
                ; ModuleDefn = use(module(Mods))
                ),
                list__member(ImportedModule, Mods)
            ),
            list__filter(FindImports, Items, ImportItems),
            ( ImportItems = [] ->
                error("check_parent_module")
            ;
                true
            ),
            list__foldl(report_inaccessible_module_error(
                ModuleName, ParentModule, SubModule),
                ImportItems, !IO)
        )
    ;
        true
    ).

:- pred report_inaccessible_module_error(module_name::in, module_name::in,
    string::in, item_and_context::in, io::di, io::uo) is det.

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
        Item - Context, !IO) :-
    ( Item = module_defn(_, import(module(_))) ->
        DeclName = "import_module"
    ; Item = module_defn(_, use(module(_))) ->
        DeclName = "use_module"
    ;
        error("report_inaccessible_parent_error: invalid item")
    ),
    prog_out__write_context(Context, !IO),
    io__write_string("In module `", !IO),
    prog_out__write_sym_name(ModuleName, !IO),
    io__write_string("':\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_strings(["  error in `", DeclName, "' declaration:\n"], !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  module `", !IO),
    prog_out__write_sym_name(qualified(ParentModule, SubModule), !IO),
    io__write_string("' is inaccessible.\n", !IO),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        prog_out__write_context(Context, !IO),
        io__write_string("  Either there was no prior ", !IO),
        io__write_string("`import_module' or\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  `use_module' declaration to import ", !IO),
        io__write_string("module\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  `", !IO),
        prog_out__write_sym_name(ParentModule, !IO),
        io__write_string("', or the interface for module\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  `", !IO),
        prog_out__write_sym_name(ParentModule, !IO),
        io__write_string("' does not contain an `include_module'\n", !IO),
        prog_out__write_context(Context, !IO),
        io__write_string("  declaration for module `", !IO),
        io__write_string(SubModule, !IO),
        io__write_string("'.\n", !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO)
    ),
    io__set_exit_status(1, !IO).

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
        ; list__member(Import, !.Module ^ parent_deps)
        ; list__member(Import, !.Module ^ int_deps)
        ; list__member(Import, !.Module ^ impl_deps)
        ; list__member(Import, ModIndirectImports0)
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

        globals__io_lookup_bool_option(statistics, Statistics, !IO),
        maybe_report_stats(Statistics, !IO),

        ModIndirectImports = [Import | ModIndirectImports0],
        !:IndirectImports = !.IndirectImports ++ IntImports1 ++ IntUses1,
        !:ImpIndirectImports = !.ImpIndirectImports ++ ImpImports1 ++ ImpUses1,
        ModItems = ModItems0 ++ Items,
        !:Module = ((!.Module ^ indirect_deps := ModIndirectImports)
            ^ items := ModItems)
            ^ error := ModError,
        process_module_short_interfaces(ReadModules, Imports, Ext,
            IntStatusItem, ImpStatusItem, !IndirectImports,
            !ImpIndirectImports, !Module, !IO)
    ).

replace_section_decls(IntStatusItem, ImpStatusItem, !Items) :-
    list__map(
        (pred(Item0::in, Item::out) is det :-
            (
                Item0 = module_defn(_, Defn) - _,
                (
                    Defn = interface,
                    Item1 = IntStatusItem
                ;
                    Defn = implementation,
                    Item1 = ImpStatusItem
                )
            ->
                Item = Item1
            ;
                Item = Item0
            )
        ), !Items).

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
get_children_2([Item - _Context | Items], IncludeDeps0, IncludeDeps) :-
    ( Item = module_defn(_VarSet, include_module(Modules)) ->
        list__append(IncludeDeps0, Modules, IncludeDeps1)
    ;
        IncludeDeps1 = IncludeDeps0
    ),
    get_children_2(Items, IncludeDeps1, IncludeDeps).

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
get_accessible_children_2(!.Visible, [Item - _ | Items], !IncludeDeps) :-
    (
        Item = module_defn(_VarSet, Defn),
        ( Defn = abstract_imported
        ; Defn = opt_imported
        ; Defn = transitively_imported
        )
    ->
        !:Visible = no
    ;
        Item = module_defn(_VarSet, Defn),
        ( Defn = imported(_)
        ; Defn = used(_)
        ; Defn = interface
        ; Defn = implementation
        ; Defn = private_interface
        )
    ->
        !:Visible = yes
    ;
        Item = module_defn(_VarSet, include_module(Modules)),
        !.Visible = yes
    ->
        !:IncludeDeps = !.IncludeDeps ++ Modules
    ;
        true
    ),
    get_accessible_children_2(!.Visible, Items, !IncludeDeps).

%-----------------------------------------------------------------------------%

get_dependencies(Items, ImportDeps, UseDeps) :-
    get_dependencies_implementation(Items,
        [], IntImportDeps, [], IntUseDeps, [], ImpImportDeps, [], ImpUseDeps),
    list__append(IntImportDeps, ImpImportDeps, ImportDeps),
    list__append(IntUseDeps, ImpUseDeps, UseDeps).

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

get_dependencies(Items, IntImportDeps, IntUseDeps, ImpImportDeps, ImpUseDeps)
        :-
    get_dependencies_implementation(Items,
        [], IntImportDeps, [], IntUseDeps, [], ImpImportDeps, [], ImpUseDeps).

:- pred get_dependencies_implementation(item_list::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

get_dependencies_implementation([],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps).
get_dependencies_implementation([Item - _Context | Items],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps) :-
    ( Item = module_defn(_VarSet, interface) ->
        get_dependencies_interface(Items,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ;
        ( Item = module_defn(_VarSet, import(module(Modules))) ->
            list__append(!.ImpImportDeps, Modules, !:ImpImportDeps)
        ; Item = module_defn(_VarSet, use(module(Modules))) ->
            list__append(!.ImpUseDeps, Modules, !:ImpUseDeps)
        ;
            true
        ),
        get_dependencies_implementation(Items,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ).

:- pred get_dependencies_interface(item_list::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

get_dependencies_interface([],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps).
get_dependencies_interface([Item - _Context | Items],
        !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps) :-
    ( Item = module_defn(_VarSet, implementation) ->
        get_dependencies_implementation(Items,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ;
        ( Item = module_defn(_VarSet, import(module(Modules))) ->
            list__append(!.IntImportDeps, Modules, !:IntImportDeps)
        ; Item = module_defn(_VarSet, use(module(Modules))) ->
            list__append(!.IntUseDeps, Modules, !:IntUseDeps)
        ;
            true
        ),
        get_dependencies_interface(Items,
            !IntImportDeps, !IntUseDeps, !ImpImportDeps, !ImpUseDeps)
    ).

%-----------------------------------------------------------------------------%

    % get the fact table dependencies for a module
:- pred get_fact_table_dependencies(item_list::in, list(string)::out) is det.

get_fact_table_dependencies(Items, Deps) :-
    get_fact_table_dependencies_2(Items, [], Deps).

:- pred get_fact_table_dependencies_2(item_list::in, list(string)::in,
    list(string)::out) is det.

get_fact_table_dependencies_2([], Deps, Deps).
get_fact_table_dependencies_2([Item - _Context | Items], Deps0, Deps) :-
    ( Item = pragma(_, fact_table(_SymName, _Arity, FileName)) ->
        Deps1 = [FileName | Deps0]
    ;
        Deps1 = Deps0
    ),
    get_fact_table_dependencies_2(Items, Deps1, Deps).

%-----------------------------------------------------------------------------%

:- type submodule_map == map(module_name, item_list).

    % Given a module (well, a list of items), split it into
    % its constituent sub-modules, in top-down order.
    %
split_into_submodules(ModuleName, Items0, ModuleList, !IO) :-
    InParentInterface = no,
    split_into_submodules_2(ModuleName, Items0, InParentInterface,
        Items, ModuleList, !IO),

    %
    % Check that there are no items after the end_module declaration.
    %
    ( Items = [ _ - Context | _] ->
        report_items_after_end_module(Context, !IO)
    ;
        true
    ),
    %
    % check for modules declared as both nested and separate sub-modules
    %
    get_children(Items0, NestedSubmodules),
    assoc_list__keys(ModuleList, SeparateSubModules),
    Duplicates = set__intersect(
        set__list_to_set(NestedSubmodules),
        set__list_to_set(SeparateSubModules)),
    ( set__empty(Duplicates) ->
        true
    ;
        report_duplicate_modules(Duplicates, Items0, !IO)
    ).

:- pred split_into_submodules_2(module_name::in, item_list::in, bool::in,
    item_list::out, module_list::out, io::di, io::uo) is det.

split_into_submodules_2(ModuleName, Items0, InParentInterface, Items,
        ModuleList, !IO) :-
    InInterface0 = no,
    split_into_submodules_3(ModuleName, Items0,
        InParentInterface, InInterface0,
        ThisModuleItems, Items, SubModules, !IO),
    map__to_assoc_list(SubModules, SubModuleList),
    ModuleList = [ModuleName - ThisModuleItems | SubModuleList].

:- pred split_into_submodules_3(module_name::in, item_list::in, bool::in,
    bool::in, item_list::out, item_list::out,
    map(module_name, item_list)::out, io::di, io::uo) is det.

split_into_submodules_3(_ModuleName, [], _, _, [], [], SubModules, !IO) :-
    map__init(SubModules).
split_into_submodules_3(ModuleName, [Item | Items1],
        InParentInterface, InInterface0,
        ThisModuleItems, OtherItems, SubModules, !IO) :-
    (
        %
        % check for a `module' declaration, which signals
        % the start of a nested module
        %
        Item = module_defn(VarSet, module(SubModuleName)) - Context
    ->
        %
        % parse in the items for the nested submodule
        %
        split_into_submodules_2(SubModuleName, Items1, InInterface0,
            Items2, SubModules0, !IO),
        %
        % parse in the remaining items for this module
        %
        split_into_submodules_3(ModuleName, Items2, InParentInterface,
            InInterface0, ThisModuleItems0, Items3, SubModules1, !IO),

        %
        % combine the sub-module declarations from the previous two
        % steps
        %
        list__foldl(add_submodule, SubModules0, SubModules1, SubModules),
        %
        % replace the nested submodule with an `include_module'
        % declaration
        %
        IncludeSubMod = module_defn(VarSet,
            include_module([SubModuleName])) - Context,
        ThisModuleItems = [IncludeSubMod | ThisModuleItems0],
        OtherItems = Items3
    ;
        %
        % check for a matching `end_module' declaration
        %
        Item = module_defn(_VarSet, end_module(ModuleName)) - _
    ->
        %
        % if so, thats the end of this module
        %
        ThisModuleItems = [],
        OtherItems = Items1,
        map__init(SubModules)
    ;
        %
        % otherwise, process the next item in this module
        %

        %
        % update the flag which records whether
        % we're currently in the interface section,
        % and report an error if there is an `implementation'
        % section inside an `interface' section.
        %
        ( Item = module_defn(_, interface) - _Context ->
            InInterface1 = yes
        ; Item = module_defn(_, implementation) - ImplContext ->
            (
                InParentInterface = yes,
                report_error_implementation_in_interface(ModuleName,
                    ImplContext, !IO)
            ;
                InParentInterface = no
            ),
            InInterface1 = no
        ;
            InInterface1 = InInterface0
        ),
        %
        % Check to make sure that a non-abstract instance declaration
        % does not occur in a module interface.
        %
        (
            InInterface1 = yes,
            Item = instance(_, _, _, Body, _, _) - InstanceContext,
            Body \= abstract
        ->
            report_non_abstract_instance_in_interface(InstanceContext, !IO)
        ;
            true
        ),

        %
        % parse the remaining items for this module,
        %
        split_into_submodules_3(ModuleName, Items1,
            InParentInterface, InInterface1,
            ThisModuleItems0, Items2, SubModules, !IO),
        %
        % put the current item back onto the
        % front of the item list for this module
        %
        ThisModuleItems = [Item | ThisModuleItems0],
        OtherItems = Items2
    ).

:- pred add_submodule(pair(module_name, item_list)::in,
    submodule_map::in, submodule_map::out) is det.

add_submodule(ModuleName - ModuleItemList, !SubModules) :-
    %
    % If the same module name occurs twice, then just append
    % the lists of items together.
    % Perhaps we should be a bit more strict about this, for
    % example by only allowing one `:- implementation' section
    % and one `:- interface' section for each module?
    % (That is what the Mercury language reference manual mandates.
    % On the other hand, it also says that top-level modules
    % should only have one `:- interface' and one `:- implementation'
    % section, and we don't enforce that either...)
    %
    ( map__search(!.SubModules, ModuleName, ItemList0) ->
        list__append(ModuleItemList, ItemList0, ItemList),
        svmap__det_update(ModuleName, ItemList, !SubModules)
    ;
        svmap__det_insert(ModuleName, ModuleItemList, !SubModules)
    ).

:- pred report_error_implementation_in_interface(module_name::in,
    prog_context::in, io::di, io::uo) is det.

report_error_implementation_in_interface(ModuleName, Context, !IO) :-
    ( ModuleName = qualified(ParentModule0, ChildModule0) ->
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        error("report_error_implementation_in_interface")
    ),
    prog_out__write_context(Context, !IO),
    io__write_string("In interface for module `", !IO),
    prog_out__write_sym_name(ParentModule, !IO),
    io__write_string("':\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  in definition of sub-module `", !IO),
    io__write_string(ChildModule, !IO),
    io__write_string("':\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string(
        "  error: `:- implementation.' declaration for sub-module\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string(
        "  occurs in interface section of parent module.\n", !IO),
    io__set_exit_status(1, !IO).

:- pred report_duplicate_modules(set(module_name)::in, item_list::in,
    io::di, io::uo) is det.

report_duplicate_modules(Duplicates, Items, !IO) :-
    IsDuplicateError =
        (pred(SubModuleName - Context::out) is nondet :-
            list__member(Item, Items),
            Item = module_defn(_VarSet, ModuleDefn) - Context,
            (
                ModuleDefn = module(SubModuleName)
            ;
                ModuleDefn = include_module(SubModuleNames),
                list__member(SubModuleName, SubModuleNames)
            ),
            set__member(SubModuleName, Duplicates)
        ),
    solutions(IsDuplicateError, DuplicateErrors),
    list__foldl(report_error_duplicate_module_decl, DuplicateErrors, !IO).

:- pred report_error_duplicate_module_decl(pair(module_name, prog_context)::in,
    io::di, io::uo) is det.

report_error_duplicate_module_decl(ModuleName - Context, !IO) :-
    ( ModuleName = qualified(ParentModule0, ChildModule0) ->
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        error("report_error_duplicate_module_decl")
    ),
    % The error message should look this this:
    % foo.m:123: In module `foo':
    % foo.m:123:   error: sub-module `bar' declared as both
    % foo.m:123:   a separate sub-module and a nested sub-module.
    prog_out__write_context(Context, !IO),
    io__write_string("In module `", !IO),
    prog_out__write_sym_name(ParentModule, !IO),
    io__write_string("':\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  error: sub-module `", !IO),
    io__write_string(ChildModule, !IO),
    io__write_string("' declared as both\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  a separate sub-module and a nested sub-module.\n", !IO),
    io__set_exit_status(1, !IO).

:- pred report_items_after_end_module(prog_context::in, io::di, io::uo) is det.

report_items_after_end_module(Context, !IO) :-
    ErrorPieces = [words("Error: item(s) after end_module declaration.")],
    write_error_pieces(Context, 0, ErrorPieces, !IO),
    io.set_exit_status(1, !IO).
            
:- pred report_non_abstract_instance_in_interface(prog_context::in,
    io::di, io::uo) is det.

report_non_abstract_instance_in_interface(Context, !IO) :-
    ErrorPieces = [words("Error: non-abstract instance declaration"),
        words("in module interface.")],
    write_error_pieces(Context, 0, ErrorPieces, !IO),
    io.set_exit_status(1, !IO).

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
    list__reverse(RevItems, Items1),
    maybe_add_foreign_import_module(ModuleName, Items1, Items2),
    order_items(Items2, Items).

:- pred get_interface_and_implementation(module_name::in, bool::in,
    item_list::in, item_list::out, item_list::out) is det.

get_interface_and_implementation(ModuleName, IncludeImplTypes,
        Items0, InterfaceItems, ImplementationItems) :-
    AddToImpl = (func(ImplItem, ImplItems) = [ImplItem | ImplItems]),
    get_interface_and_implementation_2(IncludeImplTypes, Items0, no,
        [], RevIntItems, AddToImpl, [], RevImplItems),
    list__reverse(RevIntItems, InterfaceItems0),
    list__reverse(RevImplItems, ImplementationItems),
    maybe_add_foreign_import_module(ModuleName,
        InterfaceItems0, InterfaceItems).

:- pred maybe_add_foreign_import_module(module_name::in,
    item_list::in, item_list::out) is det.

maybe_add_foreign_import_module(ModuleName, Items0, Items) :-
    get_foreign_self_imports(Items0, Langs),
    MakeForeignImport = (func(Lang) = ImportItem :-
            Origin     = compiler(foreign_imports),
            Pragma     = foreign_import_module(Lang, ModuleName),
            Context    = term.context_init,
            ImportItem = pragma(Origin, Pragma) - Context
    ),
    Imports = list.map(MakeForeignImport, Langs),
    Items = Imports ++ Items0.

:- pred get_foreign_self_imports(item_list::in, list(foreign_language)::out)
    is det.

get_foreign_self_imports(Items, Langs) :-
    list.foldl(accumulate_item_foreign_import_langs, Items, set.init, LangSet),
    set.to_sorted_list(LangSet, Langs).

:- pred accumulate_item_foreign_import_langs(item_and_context::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

accumulate_item_foreign_import_langs(Item - _, !LangSet) :-
    solutions(item_needs_foreign_imports(Item), Langs),
    set.insert_list(!.LangSet, Langs, !:LangSet).

:- pred get_interface_and_implementation_2(bool::in, item_list::in, bool::in,
    item_list::in, item_list::out,
    func(item_and_context, T) = T::in, T::in, T::out) is det.

get_interface_and_implementation_2(_, [], _, !IntItems, _, !ImplItems).
get_interface_and_implementation_2(IncludeImplTypes, [ItemAndContext | Rest],
        InInterface0, !IntItems, AddImplItem, !ImplItems) :-
    ItemAndContext = Item - Context,
    (
        Item = module_defn(_, interface)
    ->
        !:IntItems = [ItemAndContext | !.IntItems],
        InInterface1 = yes,
        Continue = yes
    ;
        Item = module_defn(_, Defn),
        ( Defn = imported(_)
        ; Defn = used(_)
        )
    ->
        % Items after here are not part of this module.
        InInterface1 = no,
        Continue = no
    ;
        Item = module_defn(_, implementation)
    ->
        !:IntItems = [ItemAndContext | !.IntItems],
        InInterface1 = no,
        Continue = yes
    ;
        (
            InInterface0 = yes,
            ( make_abstract_instance(Item, Item1) ->
                ItemToWrite = Item1,
                !:ImplItems = AddImplItem(ItemAndContext, !.ImplItems)
            ;
                ItemToWrite = Item
            ),
            !:IntItems = [ItemToWrite - Context | !.IntItems]
        ;
            InInterface0 = no,
            !:ImplItems = AddImplItem(ItemAndContext, !.ImplItems),
            (
                IncludeImplTypes = yes,
                include_in_int_file_implementation(Item)
            ->
                (
                    make_abstract_defn(Item, int2, ImpItem1)
                ->
                    ImpItem = ImpItem1
                ;
                    make_abstract_unify_compare(Item, int2, ImpItem1)
                ->
                    ImpItem = ImpItem1
                ;
                    ImpItem = Item
                ),
                !:IntItems = [ImpItem - Context | !.IntItems]
            ;
                true
            )
        ),
        InInterface1 = InInterface0,
        Continue = yes
    ),
    (
        Continue = yes,
        get_interface_and_implementation_2(IncludeImplTypes,
            Rest, InInterface1, !IntItems, AddImplItem, !ImplItems)
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
    list__reverse(RevItems, Items1),
    maybe_strip_import_decls(Items1, Items2),
    order_items(Items2, Items).

:- pred get_short_interface_2(item_list::in, short_interface_kind::in,
    item_list::in, item_list::out) is det.

get_short_interface_2([], _Kind, Items, Items).
get_short_interface_2([ItemAndContext | Rest], Kind, Items0, Items) :-
    ItemAndContext = Item0 - Context,
    ( make_abstract_defn(Item0, Kind, Item1) ->
        Items1 = [Item1 - Context | Items0]
    ; make_abstract_unify_compare(Item0, Kind, Item1) ->
        Items1 = [Item1 - Context | Items0]
    ; include_in_short_interface(Item0) ->
        Items1 = [ItemAndContext | Items0]
    ;
        Items1 = Items0
    ),
    get_short_interface_2(Rest, Kind, Items1, Items).

:- pred include_in_short_interface(item::in) is semidet.

include_in_short_interface(type_defn(_, _, _, _, _)).
include_in_short_interface(inst_defn(_, _, _, _, _)).
include_in_short_interface(mode_defn(_, _, _, _, _)).
include_in_short_interface(module_defn(_, _)).
include_in_short_interface(instance(_, _, _, _, _, _)).
include_in_short_interface(pragma(_, foreign_import_module(_, _))).

    % Could this item use items from imported modules.
:- func item_needs_imports(item) = bool.

item_needs_imports(clause(_, _, _, _, _, _)) = yes.
item_needs_imports(Item @ type_defn(_, _, _, _, _)) =
        ( Item ^ td_ctor_defn = abstract_type(_) -> no ; yes ).
item_needs_imports(inst_defn(_, _, _, _, _)) = yes.
item_needs_imports(mode_defn(_, _, _, _, _)) = yes.
item_needs_imports(module_defn(_, _)) = no.
item_needs_imports(pragma(_, _)) = yes.
item_needs_imports(pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _)) = yes.
item_needs_imports(pred_or_func_mode(_, _, _, _, _, _, _)) = yes.
item_needs_imports(Item @ typeclass(_, _, _, _, _, _)) =
    (
        Item ^ tc_class_methods = abstract,
        \+ (
            list__member(Constraint, Item ^ tc_constraints),
            Constraint = constraint(_, ConstraintArgs),
            list__member(ConstraintArg, ConstraintArgs),
            type_is_nonvar(ConstraintArg)
        )
    ->
        no
    ;
        yes
    ).
item_needs_imports(instance(_, _, _, _, _, _)) = yes.
item_needs_imports(promise(_, _, _, _)) = yes.
item_needs_imports(initialise(_, _, _)) = yes.
item_needs_imports(finalise(_, _, _)) = yes.
item_needs_imports(mutable(_, _, _, _, _)) = yes.
item_needs_imports(nothing(_)) = no.

:- pred item_needs_foreign_imports(item::in, foreign_language::out) is nondet.

item_needs_foreign_imports(pragma(_, export(_, _, _, _)), Lang) :-
    foreign_language(Lang).

    % `:- pragma import' is only supported for C.
item_needs_foreign_imports(pragma(_, import(_, _, _, _, _)), c).
item_needs_foreign_imports(Item @ type_defn(_, _, _, _, _), Lang) :-
    Item ^ td_ctor_defn = foreign_type(ForeignType, _, _),
    Lang = foreign_type_language(ForeignType).
item_needs_foreign_imports(pragma(_, foreign_decl(Lang, _, _)), Lang).
item_needs_foreign_imports(pragma(_, foreign_code(Lang, _)), Lang).
item_needs_foreign_imports(pragma(_, foreign_proc(Attrs, _, _, _, _, _)),
        foreign_language(Attrs)).
item_needs_foreign_imports(mutable(_, _, _, _, _), Lang) :-
    foreign_language(Lang).

:- pred include_in_int_file_implementation(item::in) is semidet.

include_in_int_file_implementation(type_defn(_, _, _, _, _)).
include_in_int_file_implementation(module_defn(_, Defn)) :-
    Defn \= external(_, _).

    % `:- typeclass declarations' may be referred to
    % by the constructors in type declarations.
    % Since these constructors are abstractly exported,
    % we won't need the local instance declarations.
    %
include_in_int_file_implementation(typeclass(_, _, _, _, _, _)).
include_in_int_file_implementation(pragma(_, foreign_import_module(_, _))).

:- pred make_abstract_defn(item::in, short_interface_kind::in, item::out)
    is semidet.

make_abstract_defn(type_defn(_VarSet, _Name, _Args, TypeDefn, _Cond) @ Item0,
        ShortInterfaceKind,
        Item0 ^ td_ctor_defn := abstract_type(IsSolverType)) :-
    (
        TypeDefn = du_type(_, _),
        IsSolverType = non_solver_type,
        % For the `.int2' files, we need the full definitions of
        % discriminated union types.  Even if the functors for a type
        % are not used within a module, we may need to know them for
        % comparing insts, e.g. for comparing `ground' and `bound(...)'.
        ShortInterfaceKind = int3
    ;
        TypeDefn = abstract_type(IsSolverType)
    ;
        TypeDefn = solver_type(_, _),
        % rafe: XXX we need to also export the details of the
        % forwarding type for the representation and the forwarding
        % pred for initialization.
        IsSolverType = solver_type
    ;
        TypeDefn = eqv_type(_),
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
        TypeDefn = foreign_type(_, _, _),
        % We always need the definitions of foreign types
        % to handle inter-language interfacing correctly.
        IsSolverType = non_solver_type,
        semidet_fail
    ).
make_abstract_defn(instance(_, _, _, _, _, _) @ Item0, int2, Item) :-
    make_abstract_instance(Item0, Item).
make_abstract_defn(typeclass(_, _, _, _, _, _) @ Item, _,
        Item ^ tc_class_methods := abstract).

:- pred make_abstract_unify_compare(item::in, short_interface_kind::in,
    item::out) is semidet.

make_abstract_unify_compare(type_defn(VarSet, Name, Args, TypeDefn0, Cond),
        int2, type_defn(VarSet, Name, Args, TypeDefn, Cond)) :-
    (
        TypeDefn0 = du_type(Constructors, yes(_UserEqComp)),
        TypeDefn  = du_type(Constructors, yes(
                abstract_noncanonical_type(non_solver_type)))
    ;
        TypeDefn0 = foreign_type(ForeignType, yes(_UserEqComp), Assertions),
        TypeDefn  = foreign_type(ForeignType, yes(
            abstract_noncanonical_type(non_solver_type)),
            Assertions)
    ;
        TypeDefn0 = solver_type(SolverTypeDetails, yes(_UserEqComp)),
        TypeDefn  = solver_type(SolverTypeDetails,
            yes(abstract_noncanonical_type(solver_type)))
    ).

    % All instance declarations must be written
    % to `.int' files as abstract instance
    % declarations because the method names
    % have not yet been module qualified.
    % This could cause the wrong predicate to be
    % used if calls to the method are specialized.
:- pred make_abstract_instance(item::in, item::out) is semidet.

make_abstract_instance(Item0, Item) :-
    Item0 = instance(_Constraints, _Class, _ClassTypes, Body0,
        _TVarSet, _ModName),
    Body0 = concrete(_),
    Body = abstract,
    Item = Item0 ^ ci_method_instances := Body.

:- pred maybe_strip_import_decls(item_list::in, item_list::out) is det.

maybe_strip_import_decls(!Items) :-
    (
        some [Item] (
            list__member(Item - _, !.Items),
            item_needs_imports(Item) = yes
        )
    ->
        true
    ;
        list__filter(
            (pred((ThisItem - _)::in) is semidet :-
                \+ (
                    ThisItem = module_defn(_, Defn),
                    ( Defn = import(_)
                    ; Defn = use(_)
                    )
                )
            ), !Items)
    ),
    (
        some [Item] (
            list__member(Item - _, !.Items),
            item_needs_foreign_imports(Item, _)
        )
    ->
        true
    ;
        list__filter(
            (pred((ThisItem - _)::in) is semidet :-
                ThisItem \= pragma(_, foreign_import_module(_, _))
            ), !Items)
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

order_items(Items0, Items) :-
    filter_unnecessary_flips(Items0, other, Items1),
    do_order_items(Items1, Items2),
        % Delete any redundant :- interface and :- implementation markers
        % at the end, to make Items as insensitive as we can to the number
        % of interface sections in the source file. If some of the
        % implementation sections are not empty, we won't be fully successful.
    list__reverse(Items2, RevItems2),
    list__takewhile(interface_or_import_marker, RevItems2, _, RevItems),
    list__reverse(RevItems, Items).

:- pred interface_or_import_marker(item_and_context::in) is semidet.

interface_or_import_marker(module_defn(_, interface) - _).
interface_or_import_marker(module_defn(_, implementation) - _).

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
filter_unnecessary_flips([Item], _, [Item]).
filter_unnecessary_flips([Item1, Item2 | Items0], CurPos, Items) :-
    (
        CurPos = in_interface,
        Item1 = module_defn(_, implementation) - _,
        Item2 = module_defn(_, interface) - _
    ->
        filter_unnecessary_flips(Items0, CurPos, Items)
    ;
        CurPos = in_implementation,
        Item1 = module_defn(_, interface) - _,
        Item2 = module_defn(_, implementation) - _
    ->
        filter_unnecessary_flips(Items0, CurPos, Items)
    ;
        ( Item1 = module_defn(_, implementation) - _ ->
            NextPos = in_implementation
        ; Item1 = module_defn(_, interface) - _ ->
            NextPos = in_interface
        ; chunkable_item_and_context(Item1) = yes ->
            NextPos = CurPos
        ;
            NextPos = other
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
:- pred do_order_items(item_list::in, item_list::out) is det.

do_order_items([], []).
do_order_items([Item0 | Items0], OrderedItems) :-
    ( chunkable_item_and_context(Item0) = yes ->
        list__takewhile(is_chunkable, Items0, FrontItems, RemainItems),
        list__filter(is_reorderable, [Item0 | FrontItems],
            ReorderableItems, NonReorderableItems),
        list__filter(import_or_use, ReorderableItems,
            ImportReorderableItems, NonImportReorderableItems),
        list__filter(symname_orderable, NonReorderableItems,
            SymNameItems, NonSymNameItems),
            % We rely on the sort being stable to keep the items
            % with the same sym_names in their original order.
        list__sort(compare_by_symname, SymNameItems, OrderedSymNameItems),
        do_order_items(RemainItems, OrderedRemainItems),
        OrderedItems = list__sort(ImportReorderableItems) ++
            list__sort(NonImportReorderableItems) ++
            OrderedSymNameItems ++ NonSymNameItems ++ OrderedRemainItems
    ;
        do_order_items(Items0, OrderedItemsTail),
        OrderedItems = [Item0 | OrderedItemsTail]
    ).

:- pred import_or_use(item_and_context::in) is semidet.

import_or_use(module_defn(_, import(_)) - _).
import_or_use(module_defn(_, use(_)) - _).

:- pred is_reorderable(item_and_context::in) is semidet.

is_reorderable(ItemAndContext) :-
    reorderable_item_and_context(ItemAndContext) = yes.

:- func reorderable_item_and_context(item_and_context) = bool.

reorderable_item_and_context(Item - _Context) =
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

reorderable_item(module_defn(_, ModuleDefn)) = Reorderable :-
    ( ModuleDefn = import(_), Reorderable = yes
    ; ModuleDefn = abstract_imported, Reorderable = no
    ; ModuleDefn = end_module(_), Reorderable = no
    ; ModuleDefn = export(_), Reorderable = yes
    ; ModuleDefn = external(_, _), Reorderable = yes
    ; ModuleDefn = implementation, Reorderable = no
    ; ModuleDefn = imported(_), Reorderable = no
    ; ModuleDefn = include_module(_), Reorderable = no
    ; ModuleDefn = interface, Reorderable = no
    ; ModuleDefn = module(_), Reorderable = no
    ; ModuleDefn = opt_imported, Reorderable = no
    ; ModuleDefn = private_interface, Reorderable = no
    ; ModuleDefn = transitively_imported, Reorderable = no
    ; ModuleDefn = use(_), Reorderable = yes
    ; ModuleDefn = used(_), Reorderable = no
    ; ModuleDefn = version_numbers(_, _), Reorderable = no
    ).
reorderable_item(pragma(_, Pragma)) = Reorderable :-
    ( Pragma = aditi(_, _), Reorderable = no
    ; Pragma = aditi_index(_, _, _), Reorderable = no
    ; Pragma = aditi_memo(_, _), Reorderable = no
    ; Pragma = aditi_no_memo(_, _), Reorderable = no
    ; Pragma = base_relation(_, _), Reorderable = no
    ; Pragma = check_termination(_, _), Reorderable = yes
    ; Pragma = context(_, _), Reorderable = no
    ; Pragma = does_not_terminate(_, _), Reorderable = yes
    ; Pragma = exceptions(_, _, _, _, _), Reorderable = no
    ; Pragma = export(_, _, _, _), Reorderable = yes
    ; Pragma = fact_table(_, _, _), Reorderable = no
    ; Pragma = foreign_code(_, _), Reorderable = no
    ; Pragma = foreign_decl(_, _, _), Reorderable = no
    ; Pragma = foreign_import_module(_, _), Reorderable = no
    ; Pragma = foreign_proc(_, _, _, _, _, _), Reorderable = no
    ; Pragma = import(_, _, _, _, _), Reorderable = no
    ; Pragma = inline(_, _), Reorderable = yes
    ; Pragma = mode_check_clauses(_, _), Reorderable = yes
    ; Pragma = naive(_, _), Reorderable = no
    ; Pragma = no_inline(_, _), Reorderable = yes
    ; Pragma = obsolete(_, _), Reorderable = yes
    ; Pragma = owner(_, _, _), Reorderable = no
    ; Pragma = promise_pure(_, _), Reorderable = yes
    ; Pragma = promise_semipure(_, _), Reorderable = yes
    ; Pragma = psn(_, _), Reorderable = no
    ; Pragma = reserve_tag(_, _), Reorderable = yes
    ; Pragma = source_file(_), Reorderable = no
    ; Pragma = supp_magic(_, _), Reorderable = no
    ; Pragma = tabled(_, _, _, _, _), Reorderable = yes
    ; Pragma = terminates(_, _), Reorderable = yes
    ; Pragma = termination2_info(_, _, _, _, _, _), Reorderable = no
    ; Pragma = termination_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = type_spec(_, _, _, _, _, _, _, _), Reorderable = yes
    ; Pragma = unused_args(_, _, _, _, _), Reorderable = yes
    ).
reorderable_item(type_defn(_, _, _, _, _)) = yes.
reorderable_item(inst_defn(_, _, _, _, _)) = yes.
reorderable_item(mode_defn(_, _, _, _, _)) = yes.
reorderable_item(promise(_, _, _, _)) = yes.
reorderable_item(typeclass(_, _, _, _, _, _)) = yes.
reorderable_item(instance(_, _, _, _, _, _)) = yes.
reorderable_item(clause(_, _, _, _, _, _)) = no.
reorderable_item(nothing(_)) = no.
reorderable_item(pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _)) = no.
reorderable_item(pred_or_func_mode(_, _, _, _, _, _, _)) = no.
reorderable_item(initialise(_, _, _)) = no.
reorderable_item(finalise(_, _, _)) = no.
reorderable_item(mutable(_, _, _, _, _)) = no.

:- pred is_chunkable(item_and_context::in) is semidet.

is_chunkable(ItemAndContext) :-
    chunkable_item_and_context(ItemAndContext) = yes.

:- func chunkable_item_and_context(item_and_context) = bool.

chunkable_item_and_context(Item - _Context) =
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

chunkable_item(module_defn(_, ModuleDefn)) = Reorderable :-
    ( ModuleDefn = abstract_imported, Reorderable = no
    ; ModuleDefn = end_module(_), Reorderable = no
    ; ModuleDefn = export(_), Reorderable = yes
    ; ModuleDefn = external(_, _), Reorderable = yes
    ; ModuleDefn = implementation, Reorderable = no
    ; ModuleDefn = import(_), Reorderable = yes
    ; ModuleDefn = imported(_), Reorderable = no
    ; ModuleDefn = include_module(_), Reorderable = no
    ; ModuleDefn = interface, Reorderable = no
    ; ModuleDefn = module(_), Reorderable = no
    ; ModuleDefn = opt_imported, Reorderable = no
    ; ModuleDefn = private_interface, Reorderable = no
    ; ModuleDefn = transitively_imported, Reorderable = no
    ; ModuleDefn = use(_), Reorderable = yes
    ; ModuleDefn = used(_), Reorderable = no
    ; ModuleDefn = version_numbers(_, _), Reorderable = no
    ).
chunkable_item(pragma(_, Pragma)) = Reorderable :-
    ( Pragma = aditi(_, _), Reorderable = no
    ; Pragma = aditi_index(_, _, _), Reorderable = no
    ; Pragma = aditi_memo(_, _), Reorderable = no
    ; Pragma = aditi_no_memo(_, _), Reorderable = no
    ; Pragma = base_relation(_, _), Reorderable = no
    ; Pragma = check_termination(_, _), Reorderable = yes
    ; Pragma = context(_, _), Reorderable = no
    ; Pragma = does_not_terminate(_, _), Reorderable = yes
    ; Pragma = exceptions(_, _, _, _, _), Reorderable = no
    ; Pragma = export(_, _, _, _), Reorderable = yes
    ; Pragma = fact_table(_, _, _), Reorderable = no
    ; Pragma = foreign_code(_, _), Reorderable = no
    ; Pragma = foreign_decl(_, _, _), Reorderable = no
    ; Pragma = foreign_import_module(_, _), Reorderable = no
    ; Pragma = foreign_proc(_, _, _, _, _, _), Reorderable = no
    ; Pragma = import(_, _, _, _, _), Reorderable = no
    ; Pragma = inline(_, _), Reorderable = yes
    ; Pragma = mode_check_clauses(_, _), Reorderable = yes
    ; Pragma = naive(_, _), Reorderable = no
    ; Pragma = no_inline(_, _), Reorderable = yes
    ; Pragma = obsolete(_, _), Reorderable = yes
    ; Pragma = owner(_, _, _), Reorderable = no
    ; Pragma = promise_pure(_, _), Reorderable = yes
    ; Pragma = promise_semipure(_, _), Reorderable = yes
    ; Pragma = psn(_, _), Reorderable = no
    ; Pragma = reserve_tag(_, _), Reorderable = yes
    ; Pragma = source_file(_), Reorderable = no
    ; Pragma = supp_magic(_, _), Reorderable = no
    ; Pragma = tabled(_, _, _, _, _), Reorderable = yes
    ; Pragma = terminates(_, _), Reorderable = yes
    ; Pragma = termination2_info( _, _, _, _, _, _), Reorderable = no
    ; Pragma = termination_info(_, _, _, _, _), Reorderable = yes
    ; Pragma = type_spec(_, _, _, _, _, _, _, _), Reorderable = yes
    ; Pragma = unused_args(_, _, _, _, _), Reorderable = yes
    ).
chunkable_item(type_defn(_, _, _, _, _)) = yes.
chunkable_item(inst_defn(_, _, _, _, _)) = yes.
chunkable_item(mode_defn(_, _, _, _, _)) = yes.
chunkable_item(pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _)) = yes.
chunkable_item(pred_or_func_mode(_, _, _, _, _, _, _)) = yes.
chunkable_item(promise(_, _, _, _)) = yes.
chunkable_item(typeclass(_, _, _, _, _, _)) = yes.
chunkable_item(instance(_, _, _, _, _, _)) = yes.
chunkable_item(clause(_, _, _, _, _, _)) = yes.
chunkable_item(initialise(_, _, _)) = yes.
chunkable_item(finalise(_, _, _)) = yes.
chunkable_item(mutable(_, _, _, _, _)) = no.
chunkable_item(nothing(_)) = yes.

    % Given a list of items for which symname_ordered succeeds, we need to keep
    % the relative order of the items with the same sym_name as returned by
    % symname_ordered, but the relative order of items with different sym_names
    % doesn't matter.
    %
:- pred symname_ordered(item_and_context::in, sym_name::out) is semidet.

symname_ordered(pred_or_func(_, _, _, _, Name, _, _, _, _, _, _, _) - _, Name).
symname_ordered(pred_or_func_mode(_, _, Name, _, _, _, _) - _, Name).

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
            map__set(Timestamps0, ModuleName, TimestampInfo, Timestamps),
            !:Module = !.Module ^ maybe_timestamps := yes(Timestamps)
        ;
            MaybeTimestamp = no
        )
    ;
        !.Module ^ maybe_timestamps = no
    ).

:- pred report_modification_time_warning(file_name::in, io__error::in,
    io::di, io::uo) is det.

report_modification_time_warning(SourceFileName, Error, !IO) :-
    globals__io_set_option(smart_recompilation, bool(no), !IO),
    globals__io_set_option(generate_item_version_numbers, bool(no), !IO),
    globals__io_lookup_bool_option(warn_smart_recompilation, Warn, !IO),
    (
        Warn = yes,
        io__write_string("Warning: cannot find modification time for ", !IO),
        io__write_string(SourceFileName, !IO),
        io__write_string(":\n", !IO),
        io__error_message(Error, Msg),
        io__write_string("  ", !IO),
        io__write_string(Msg, !IO),
        io__write_string(".\n", !IO),
        io__write_string("  Smart recompilation will not work.\n", !IO),
        globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
        (
            HaltAtWarn = yes,
            io__set_exit_status(1, !IO)
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

    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Generating shell script `" ++
        FileName ++ "'...\n", !IO),

    module_name_to_file_name(MainModuleName, ".class", no, ClassFileName, !IO),
    DirName = dir.dirname(ClassFileName),

    % XXX PathSeparator should be ";" on Windows
    PathSeparator = ":",
    globals__io_lookup_accumulating_option(java_classpath, Java_Incl_Dirs0,
        !IO),
    % We prepend the .class files' directory and the current CLASSPATH.
    Java_Incl_Dirs = [DirName, "$CLASSPATH" | Java_Incl_Dirs0],
    ClassPath = string.join_list(PathSeparator, Java_Incl_Dirs),

    globals__io_lookup_string_option(java_interpreter, Java, !IO),
    module_name_to_file_name(MainModuleName, "", no, Name_No_Extn, !IO),

    io__open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(ShellScript),
        % XXX On Windows we should output a .bat file instead
        io__write_string(ShellScript, "#!/bin/sh\n", !IO),
        io__write_string(ShellScript, "CLASSPATH=" ++ ClassPath ++ " ", !IO),
        io__write_string(ShellScript, Java ++ " ", !IO),
        io__write_string(ShellScript, Name_No_Extn ++ "\n", !IO),
        io__close_output(ShellScript, !IO),
        io__call_system("chmod a+x " ++ FileName, ChmodResult, !IO),
        (
            ChmodResult = ok(Status),
            ( Status = 0 ->
                Succeeded = yes,
                maybe_write_string(Verbose, "% done.\n", !IO)
            ;
                error("chmod exit status != 0"),
                Succeeded = no
            )
        ;
            ChmodResult = error(Message),
            error(io__error_message(Message)),
            Succeeded = no
        )
    ;
        OpenResult = error(Message),
        error(io__error_message(Message)),
        Succeeded = no
    ).

list_class_files_for_jar(ModuleName, ClassFiles, ListClassFiles, !IO) :-
    globals__io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
    globals__io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs,
        !IO),
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
    io__get_environment_var("CLASSPATH", MaybeCP, !IO),
    (
        MaybeCP = yes(Classpath)
    ;
        MaybeCP = no,
        io__get_environment_var("java.class.path", MaybeJCP, !IO),
        (
            MaybeJCP = yes(Classpath)
        ;
            MaybeJCP = no,
            Classpath = ""
        )
    ).

%-----------------------------------------------------------------------------%

get_install_name_option(OutputFileName, InstallNameOpt, !IO) :-
    globals.io_lookup_string_option(shlib_linker_install_name_flag,
        InstallNameFlag, !IO),
    globals.io_lookup_string_option(shlib_linker_install_name_path,
        InstallNamePath, !IO),
    dir.directory_separator(Slash),
    InstallNameOpt = InstallNameFlag++InstallNamePath++
        char_to_string(Slash)++OutputFileName.

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "modules.m".

%-----------------------------------------------------------------------------%
:- end_module modules.
%-----------------------------------------------------------------------------%
