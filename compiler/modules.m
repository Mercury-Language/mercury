%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: modules.m
% main author: fjh

% This module contains all the code for handling module imports and exports,
% for computing module dependencies, and for generating makefile fragments to
% record those dependencies.
%
%
% The interface system works as follows:
%
% 1. a .int3 file is written, which contains all the types, typeclasses, insts
% and modes defined in the interface. Equivalence types, insts and
% modes are written in full, others are written in abstract form.
% These are module qualified as far as possible given the information
% present in the current module. The datestamp on the .date3 file
% gives the last time the .int3 file was checked for consistency.
%
% 2. The .int and .int2 files are created, using the .int3 files
% of imported modules to fully module qualify all items. Therefore
% the .int2 file is just a fully qualified version of the .int3 file.
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

:- import_module backend_libs__foreign, parse_tree__prog_data.
:- import_module parse_tree__prog_io, libs__globals, libs__timestamp.
:- import_module std_util, bool, list, map, set, io.

%-----------------------------------------------------------------------------%

	% Succeeds iff the string is the (unqualified) name of one of the
	% modules in the Mercury standard library.
	%
:- pred mercury_std_library_module(string::in) is semidet.

	% module_name_to_file_name(Module, Extension, Mkdir, FileName):
	%	Convert a module name and file extension to the
	%	corresponding file name.  If `MkDir' is yes, then
	%	create any directories needed.
	%
	%	Currently we use the convention that the module
	%	`foo:bar:baz' should be named `foo.bar.baz.m',
	%	but eventually we will also allow other file
	%	naming conventions.
	%
	%	Note that this predicate is also used to create
	%	some "phony" Makefile targets that do not have
	%	corresponding files, e.g. `<foo>.clean'.
	%
:- pred module_name_to_file_name(module_name, string, bool, file_name,
				io__state, io__state).
:- mode module_name_to_file_name(in, in, in, out, di, uo) is det.

	% module_name_to_lib_file_name(Prefix, Module, Extension, MkDir,
	%		FileName):
	%	Like module_name_to_file_name, but also allows a prefix.
	%
	%	Used for creating library names, e.g. `lib<foo>.$A'
	%	and `lib<foo>.so'.
	%
:- pred module_name_to_lib_file_name(string, module_name, string, bool,
				file_name, io__state, io__state).
:- mode module_name_to_lib_file_name(in, in, in, in, out, di, uo)
	is det.

	% module_name_to_split_c_file_name(Module, Num, Extension, FileName):
	%	Like module_name_to_file_name, but also allows a sequence
	%	number.  The files produced by this predicate will all be
	%	in a subdirectory DirName, which be obtained by calling
	%	`module_name_to_file_name(Module, ".dir", DirName)'.
	%	This predicate does not create that directory.
	%
	%	This predicate is used for the names of .c and .$O files
	%	for --split-c-files.
	%
:- pred module_name_to_split_c_file_name(module_name, int, string, file_name,
				io__state, io__state).
:- mode module_name_to_split_c_file_name(in, in, in, out, di, uo) is det.

	% module_name_to_split_c_file_pattern(Module, Extension, FileName):
	%	Like module_name_to_split_c_file_name, but generates a
	%	wildcard pattern to match all such files with the given
	%	extension for the given module.
:- pred module_name_to_split_c_file_pattern(module_name, string, file_name,
				io__state, io__state).
:- mode module_name_to_split_c_file_pattern(in, in, out, di, uo) is det.

	% fact_table_file_name(Module, FactTableFileName, Ext, FileName):
	%	Returns the filename to use when compiling fact table
	%	files.
:- pred fact_table_file_name(module_name, file_name, string, file_name,
				io__state, io__state).
:- mode fact_table_file_name(in, in, in, out, di, uo) is det.

	% convert a file name (excluding the trailing `.m')
	% to the corresponding module name
	% 
:- pred file_name_to_module_name(file_name, module_name).
:- mode file_name_to_module_name(in, out) is det.

	% Convert a module name to something that is suitable
	% for use as a variable name in makefiles.
:- pred module_name_to_make_var_name(module_name, string).
:- mode module_name_to_make_var_name(in, out) is det.

%-----------------------------------------------------------------------------%

	% read_mod(ModuleName, Extension, Descr, Search, ReturnTimestamp,
	%		Items, Error, SourceFileName, MaybeTimestamp):
	%	Given a module name and a file extension (e.g. `.m',
	%	`.int', or `int2'), read in the list of items in that file.
	%	If Extension is ".m", and ModuleName is a nested module,
	%	then try searching for different filenames:
	%	for modules such as `foo.bar.baz.m' search first for
	%	`foo.bar.baz.m', then `bar.baz.m', then `baz.m'.
	%	If Search is yes, search all directories given by the option
	%	search_directories for the module.
	%	If ReturnTimestamp is yes, attempt to return the modification
	%	time of the file in MaybeTimestamp.
	%	If the actual module name (as determined by the
	%	`:- module' declaration) does not match the specified
	%	module name, then report an error message.
	%	Return the actual source file name found
	%	(excluding the directory part).
	%
	%	N.B.  This reads a module given the module name.
	%	If you want to read a module given the file name,
	%	use `read_mod_from_file'.
	%
:- pred read_mod(module_name, string, string, bool, bool,
		item_list, module_error, file_name, maybe(timestamp),
		io__state, io__state).
:- mode read_mod(in, in, in, in, in, out, out, out, out, di, uo) is det.

	% read_mod_if_changed(ModuleName, Extension, Descr, Search,
	%	OldTimestamp, Items, Error, SourceFileName,
	%	MaybeTimestamp):
	%
	%	If the timestamp of the file specified by the given
	%	module name and file extension is newer than OldTimestamp,
	%	read the file, returning the new timestamp.
	%
	%	If the file was read, MaybeTimestamp will contain the
	%	new timestamp.
	%	If the timestamp was unchanged, MaybeTimestamp will
	%	be `yes(OldTimestamp)'.
	%	If the file could not be read, MaybeTimestamp will be `no'.
:- pred read_mod_if_changed(module_name, string, string, bool, timestamp,
		item_list, module_error, file_name, maybe(timestamp),
		io__state, io__state).
:- mode read_mod_if_changed(in, in, in, in, in,
		out, out, out, out, di, uo) is det.

	% Similar to read_mod, but doesn't return error messages.
:- pred read_mod_ignore_errors(module_name, string, string, bool, bool,
		item_list, module_error, file_name, maybe(timestamp),
		io__state, io__state).
:- mode read_mod_ignore_errors(in, in, in, in, in, out, out, out, out,
		di, uo) is det.

	% read_mod_from_file(SourceFileName, Extension, Descr, Search,
	%		ReturnTimestamp, Items, Error,
	%		ModuleName, MaybeTimestamp):
	%	Given a file name and a file extension (e.g. `.m',
	%	`.int', or `int2'), read in the list of items in that file.
	%	If Search is yes, search all directories given by the option
	%	search_directories for the module.
	%	If ReturnTimestamp is yes, attempt to return the modification
	%	time of the file in MaybeTimestamp.
	%	Return the module name (as determined by the
	%	`:- module' declaration, if any).
	%
	%	N.B.  This reads a module given the file name.
	%	If you want to read a module given the module name,
	%	use `read_mod'.
	%
:- pred read_mod_from_file(file_name, string, string, bool, bool,
		item_list, module_error, module_name,
		maybe(timestamp), io__state, io__state).
:- mode read_mod_from_file(in, in, in, in, in, out, out, out, out,
		di, uo) is det.

%-----------------------------------------------------------------------------%

	% make_private_interface(SourceFileName, SourceFileModuleName,
	%		ModuleName, MaybeTimestamp, Items):
	%	Given a source file name and module name,
	%	the timestamp of the source file,
	%	and the list of items in that module,
	%	output the private (`.int0') interface file for the module.
	%	(The private interface contains all the declarations in
	%	the module, including those in the `implementation'
	%	section; it is used when compiling sub-modules.)
	%
:- pred make_private_interface(file_name, module_name, module_name,
		maybe(timestamp), item_list, io__state, io__state).
:- mode make_private_interface(in, in, in, in, in, di, uo) is det.

	% make_interface(SourceFileName, SourceFileModuleName,
	%		ModuleName, MaybeTimestamp, Items):
	%	Given a source file name and module name,
	%	the timestamp of the source file,
	%	and the list of items in that module,
	%	output the long (`.int') and short (`.int2') interface files
	%	for the module.
	%
:- pred make_interface(file_name, module_name, module_name, maybe(timestamp),
		item_list, io__state, io__state).
:- mode make_interface(in, in, in, in, in, di, uo) is det.

	% 	Output the unqualified short interface file to <module>.int3.
	%
:- pred make_short_interface(file_name, module_name, item_list,
		io__state, io__state).
:- mode make_short_interface(in, in, in, di, uo) is det.

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
		source_file_name :: file_name,
				% The source file
		source_file_module_name :: module_name,
				% The name of the top-level module in
				% the source file containing the module
				% that we are compiling.
		module_name :: module_name,	  
				% The module (or sub-module)
				% that we are compiling.
		parent_deps :: list(module_name),
				% The list of ancestor modules it inherits
		int_deps :: list(module_name), 
				% The list of modules it directly imports
				% in the interface
				% (imports via ancestors count as direct)
		impl_deps :: list(module_name),
				% The list of modules it directly imports
				% in the implementation.
		indirect_deps :: list(module_name),
				% The list of modules it indirectly imports
		children :: list(module_name),
		public_children :: list(module_name),
				% The list of its public children,
				% i.e. child modules that it includes
				% in the interface section.
		nested_children :: list(module_name),
				% The modules included in the same source
				% file. This field is only set for the
				% top-level module in each file.
		fact_table_deps :: list(string),  
				% The list of filenames for fact tables
				% in this module.
		foreign_code :: contains_foreign_code,
				% Whether or not the module contains
				% foreign code (and which languages if it does)
		foreign_import_module_info :: foreign_import_module_info,
				% The `:- pragma foreign_import_module'
				% declarations.
		contains_foreign_export :: contains_foreign_export,
				% Does the module contain any
				% `:- pragma export' declarations.
		items :: item_list,
				% The contents of the module and its imports
		error :: module_error,
				% Whether an error has been encountered
				% when reading in this module.

		maybe_timestamps :: maybe(module_timestamps),
				% If we are doing smart recompilation,
				% we need to keep the timestamps of the
				% modules read in.

		has_main :: has_main,
				% Does this module contain main/2.
	
		module_dir :: dir_name
				% The directory containing the module source.
	).

:- type contains_foreign_code
	--->	contains_foreign_code(set(foreign_language))
	;	no_foreign_code
	;	unknown.

:- type contains_foreign_export
	--->	contains_foreign_export
	;	no_foreign_export.

:- type has_main
	--->	has_main
	;	no_main
	.

	% When doing smart recompilation record for each module
	% the suffix of the file that was read and the modification
	% time of the file.
:- type module_timestamps == map(module_name, module_timestamp).
:- type module_timestamp
	--->	module_timestamp(
			suffix :: string,
			timestamp :: timestamp,
			need_qualifier :: need_qualifier
		).

	% recompilation_check.m records each file read to avoid
	% reading it again. The string is the suffix of the file
	% name.
:- type read_modules == map(pair(module_name, string), read_module).

:- type read_module
	---> read_module(
			module_timestamp,
			item_list,
			module_error,
			file_name
	).

	% find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp,
	%	Items, MaybeTimestamp, Error, FileName)
	%
	% Check whether a file was read during recompilation checking.
:- pred find_read_module(read_modules, module_name, string, bool, item_list,
		maybe(timestamp), module_error, file_name).
:- mode find_read_module(in, in, in, in, out, out, out, out) is semidet.

% Some access predicates for the module_imports structure

:- pred module_imports_get_source_file_name(module_imports, file_name).
:- mode module_imports_get_source_file_name(in, out) is det.

:- pred module_imports_get_module_name(module_imports, module_name).
:- mode module_imports_get_module_name(in, out) is det.

:- pred module_imports_get_impl_deps(module_imports, list(module_name)).
:- mode module_imports_get_impl_deps(in, out) is det.

:- pred module_imports_get_items(module_imports, item_list).
:- mode module_imports_get_items(in, out) is det.

:- pred module_imports_set_items(module_imports, item_list, module_imports).
:- mode module_imports_set_items(in, in, out) is det.

:- pred module_imports_get_error(module_imports, module_error).
:- mode module_imports_get_error(in, out) is det.

:- pred module_imports_set_error(module_imports, module_error, module_imports).
:- mode module_imports_set_error(in, in, out) is det.

% set the interface dependencies
:- pred module_imports_set_int_deps(module_imports, list(module_name),
				module_imports).
:- mode module_imports_set_int_deps(in, in, out) is det.

% set the implementation dependencies
:- pred module_imports_set_impl_deps(module_imports, list(module_name),
				module_imports).
:- mode module_imports_set_impl_deps(in, in, out) is det.

% set the indirect dependencies
:- pred module_imports_set_indirect_deps(module_imports, list(module_name),
				module_imports).
:- mode module_imports_set_indirect_deps(in, in, out) is det.

	% make an item_and_context for a module declaration
	% or pseudo-declaration such as `:- imported'
	% (which is inserted by the compiler, but can't be used
	% in user code).
:- pred make_pseudo_decl(module_defn, item_and_context).
:- mode make_pseudo_decl(in, out) is det.

	% append_pseudo_decl(Module0, PseudoDecl, Module):
	%	append the specified module declaration to the list
	%	of items in Module0 to give Module.
	%
:- pred append_pseudo_decl(module_imports, module_defn, module_imports).
:- mode append_pseudo_decl(in, in, out) is det.

	% Strip off the `:- interface' declaration at the start of
	% the item list, if any.
:- pred strip_off_interface_decl(item_list, item_list).
:- mode strip_off_interface_decl(in, out) is det.

	% Remove all the imported items the list.
:- pred strip_imported_items(item_list, item_list).
:- mode strip_imported_items(in, out) is det.

%-----------------------------------------------------------------------------%

	% Given a module (well, a list of items), split it into
	% its constituent sub-modules, in top-down order.
	% Also do some error checking:
	% - report an error if the `implementation' section of a sub-module
	%   is contained inside the `interface' section of its parent module
	% - check for modules declared as both nested and separate sub-modules.

:- type module_list == list(pair(module_name, item_list)).

:- pred split_into_submodules(module_name, item_list, module_list,
					io__state, io__state).
:- mode split_into_submodules(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

	% grab_imported_modules(SourceFileName, SourceFileModuleName,
	%		ModuleName, NestedSubModules, ReadModules,
	%		ModuleTimestamp, Items, Module, Error)
	%	Given a source file name and the top-level module
	%	name in that file, the current module name,
	%	the nested sub-modules in the file if this module is
	%	the top-level module,
	%	the timestamp of the file SourceFileName
	%	and the list of items in the current module,
	%	read in the private interface files for all the parent modules,
	%	the long interface files for all the imported modules,
	%	and the short interface files for all the indirectly imported
	%	modules, and return a `module_imports' structure containing the
	%	relevant information.
	%	ReadModules contains the interface files read during
	%	recompilation checking.
	%
:- pred grab_imported_modules(file_name, module_name, module_name,
		list(module_name), read_modules, maybe(timestamp),
		item_list, module_imports, module_error, io__state, io__state).
:- mode grab_imported_modules(in, in, in, in, in, in, in,
		out, out, di, uo) is det.

	% grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
	%		ModuleName, Items, Module, Error):
	%	Similar to grab_imported_modules, but only reads in
	%	the unqualified short interfaces (.int3s),
	%	and the .int0 files for parent modules,
	%	instead of reading the long interfaces and
	%	qualified short interfaces (.int and int2s).
	%	Does not set the `PublicChildren' or `FactDeps'
	%	fields of the module_imports structure.

:- pred grab_unqual_imported_modules(file_name, module_name, module_name,
		item_list, module_imports, module_error, io__state, io__state).
:- mode grab_unqual_imported_modules(in, in, in, in, out, out, di, uo) is det.

	% process_module_private_interfaces(Ancestors, DirectImports0,
	%			DirectImports, DirectUses0, DirectUses,
	%			Module0, Module):
	%  	Read the complete private interfaces for modules in Ancestors,
	%	and append any imports/uses in the ancestors to the
	%	corresponding previous lists.
	%
:- pred process_module_private_interfaces(read_modules, list(module_name),
		list(module_name), list(module_name),
		list(module_name), list(module_name),
		module_imports, module_imports, io__state, io__state).
:- mode process_module_private_interfaces(in, in, in, out, in, out, in, out,
		di, uo) is det.

	% process_module_long_interfaces(ReadModules, NeedQualifier, Imports,
	%	Ext, IndirectImports0, IndirectImports, Module0, Module):
	%
	%  	Read the long interfaces for modules in Imports
	%	(unless they've already been read in)
	%	from files with filename extension Ext,
	%	and append any imports/uses in those modules to the
	%	IndirectImports list.
	%
:- pred process_module_long_interfaces(read_modules, need_qualifier,
		list(module_name), string, list(module_name),
		list(module_name), module_imports, module_imports,
		io__state, io__state).
:- mode process_module_long_interfaces(in, in, in, in, in, out, in, out,
		di, uo) is det.

	% process_module_short_interfaces_transitively(ReadModules,
	%		IndirectImports, Ext, Module0, Module):
	%  	Read the short interfaces for modules in IndirectImports
	%	(unless they've already been read in) and any
	%	modules that those modules import (transitively).
	%
:- pred process_module_short_interfaces_transitively(read_modules,
		list(module_name), string, module_imports, module_imports,
		io__state, io__state).
:- mode process_module_short_interfaces_transitively(in, in, in, in,
		out, di, uo) is det.

	% process_module_short_interfaces(ReadModules, Modules, Ext,
	%		IndirectImports0, IndirectImports, Module0, Module):
	%  	Read the short interfaces for modules in Modules
	%	(unless they've already been read in).
	%	Append the modules imported by Modules to
	%	IndirectImports0 to give IndirectImports.
	%
:- pred process_module_short_interfaces(read_modules, list(module_name),
		string, list(module_name), list(module_name),
		module_imports, module_imports, io__state, io__state).
:- mode process_module_short_interfaces(in, in, in, in, out, in, out, di, uo)
		is det.

%-----------------------------------------------------------------------------%

	% write_dependency_file(Module, AllDeps, MaybeTransOptDeps):
	%	Write out the per-module makefile dependencies (`.d') file
	%	for the specified module.
	%	AllDeps is the set of all module names which the generated
	%	code for this module might depend on, i.e. all that have been
	%	used or imported, directly or indirectly, into this module,
	%	including via .opt or .trans_opt files, and including
	%	parent modules of nested modules.
	%	MaybeTransOptDeps is a list of module names which the
	%	`.trans_opt' file may depend on.  This is set to `no' if the
	%	dependency list is not available.
	%
:- pred write_dependency_file(module_imports, set(module_name),
		maybe(list(module_name)), io__state, io__state).
:- mode write_dependency_file(in, in, in, di, uo) is det.

	%	maybe_read_dependency_file(ModuleName, MaybeTransOptDeps).
	%	If transitive intermodule optimization has been enabled,
	%	then read <ModuleName>.d to find the modules which
	%	<ModuleName>.trans_opt may depend on.  Otherwise return
	%	`no'.
:- pred maybe_read_dependency_file(module_name, maybe(list(module_name)),
		io__state, io__state).
:- mode maybe_read_dependency_file(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

	% generate_module_dependencies(ModuleName):
	%	Generate the per-program makefile dependencies (`.dep') file
	%	for a program whose top-level module is `ModuleName'.
	%	This involves first transitively reading in all imported
	%	or ancestor modules.  While we're at it, we also save the
	%	per-module makefile dependency (`.d') files for all those
	%	modules.
	%
:- pred generate_module_dependencies(module_name, io__state, io__state).
:- mode generate_module_dependencies(in, di, uo) is det.

	% generate_file_dependencies(FileName):
	%	Same as generate_module_dependencies, but takes 
	%	a file name instead of a module name.
	%
:- pred generate_file_dependencies(file_name, io__state, io__state).
:- mode generate_file_dependencies(in, di, uo) is det.

	% get_dependencies(Items, ImportDeps, UseDeps).
	%	Get the list of modules that a list of items (explicitly)
	%	depends on.  ImportDeps is the list of modules imported using
	% 	`:- import_module', UseDeps is the list of modules imported
	%	using `:- use_module'.
	%	N.B. Typically you also need to consider the module's
	%	implicit dependencies (see get_implicit_dependencies/3),
	%	its parent modules (see get_ancestors/1) and possibly
	%	also the module's child modules (see get_children/2).
	%	You may also need to consider indirect dependencies.
	%
:- pred get_dependencies(item_list, list(module_name), list(module_name)).
:- mode get_dependencies(in, out, out) is det.

	% get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps):
	%	Get the list of builtin modules (e.g. "public_builtin",
	%	"private_builtin") that a list of items may implicitly
	% 	depend on.  ImportDeps is the list of modules which 
	%	should be automatically implicitly imported as if via
	%	`:- import_module', and UseDeps is the list which should
	%	be automatically implicitly imported as if via
	%	`:- use_module'.
	%
:- pred get_implicit_dependencies(item_list, globals,
		list(module_name), list(module_name)).
:- mode get_implicit_dependencies(in, in, out, out) is det.

	% get_ancestors(ModuleName) =  ParentDeps:
	%	ParentDeps is the list of ancestor modules for this
	%	module, oldest first; e.g. if the ModuleName is 
	%	`foo:bar:baz', then ParentDeps would be [`foo', `foo:bar'].
	%
:- func get_ancestors(module_name) = list(module_name).

	% init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
	%	Error, Globals, ModuleName - Items, ModuleImports).
:- pred init_dependencies(file_name, module_name, list(module_name),
		module_error, globals, pair(module_name, item_list),
		module_imports).
:- mode init_dependencies(in, in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% touch_interface_datestamp(ModuleName, Ext).
	%
	% Touch the datestamp file `ModuleName.Ext'. Datestamp files
	% are used to record when each of the interface files was last
	% updated.

:- pred touch_interface_datestamp(module_name, string, io__state, io__state).
:- mode touch_interface_datestamp(in, in, di, uo) is det.

	% touch_datestamp(FileName).
	%
	% Update the modification time for the given file,
	% clobbering the contents of the file.
	
:- pred touch_datestamp(file_name, io__state, io__state).
:- mode touch_datestamp(in, di, uo) is det.

	% update_interface(FileName, Succeeded)
	%
	% Call the shell script mercury_update_interface to update the
	% interface file FileName if it has changed.

:- pred update_interface(string, bool, io__state, io__state).
:- mode update_interface(in, out, di, uo) is det.

:- pred update_interface(string, io__state, io__state).
:- mode update_interface(in, di, uo) is det.

	% make_directory(Dir)
	%
	% Make the directory Dir and all its parents.
:- pred make_directory(string, io__state, io__state).
:- mode make_directory(in, di, uo) is det.

%-----------------------------------------------------------------------------%

	% Check whether a particular `pragma' declaration is allowed
	% in the interface section of a module.

:- pred pragma_allowed_in_interface(pragma_type, bool).
:- mode pragma_allowed_in_interface(in, out) is det.

	% Given a module name and a list of the items in that module,
	% this procedure checks if the module doesn't export anything,
	% and if so, and --warn-nothing-exported is set, it reports
	% a warning.

:- pred check_for_no_exports(item_list, module_name, io__state, io__state).
:- mode check_for_no_exports(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module ll_backend__llds_out, hlds__passes_aux, parse_tree__prog_out.
:- import_module parse_tree__prog_util, parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_io_util, libs__options.
:- import_module parse_tree__source_file_map.
:- import_module parse_tree__module_qual, backend_libs__foreign.
:- import_module recompilation__version.
:- import_module make. % XXX undesirable dependency

:- import_module string, map, term, varset, dir, library.
:- import_module assoc_list, relation, char, require.
:- import_module getopt.

%-----------------------------------------------------------------------------%

mercury_std_library_module("array").
mercury_std_library_module("assoc_list").
mercury_std_library_module("bag").
mercury_std_library_module("benchmarking").
mercury_std_library_module("bimap").
mercury_std_library_module("bintree").
mercury_std_library_module("bintree_set").
mercury_std_library_module("bitmap").
mercury_std_library_module("bool").
mercury_std_library_module("bt_array").
mercury_std_library_module("builtin").
mercury_std_library_module("char").
mercury_std_library_module("construct").
mercury_std_library_module("counter").
mercury_std_library_module("deconstruct").
mercury_std_library_module("dir").
mercury_std_library_module("enum").
mercury_std_library_module("eqvclass").
mercury_std_library_module("exception").
mercury_std_library_module("float").
mercury_std_library_module("gc").
mercury_std_library_module("getopt").
mercury_std_library_module("graph").
mercury_std_library_module("group").
mercury_std_library_module("hash_table").
mercury_std_library_module("int").
mercury_std_library_module("integer").
mercury_std_library_module("io").
mercury_std_library_module("lexer").
mercury_std_library_module("library").
mercury_std_library_module("list").
mercury_std_library_module("map").
mercury_std_library_module("math").
mercury_std_library_module("multi_map").
mercury_std_library_module("ops").
mercury_std_library_module("parser").
mercury_std_library_module("pprint").
mercury_std_library_module("pqueue").
mercury_std_library_module("private_builtin").
mercury_std_library_module("profiling_builtin").
mercury_std_library_module("prolog").
mercury_std_library_module("queue").
mercury_std_library_module("random").
mercury_std_library_module("rational").
mercury_std_library_module("rbtree").
mercury_std_library_module("relation").
mercury_std_library_module("require").
mercury_std_library_module("rtti_implementation").
mercury_std_library_module("set").
mercury_std_library_module("set_bbbtree").
mercury_std_library_module("set_ordlist").
mercury_std_library_module("set_unordlist").
mercury_std_library_module("sparse_bitset").
mercury_std_library_module("stack").
mercury_std_library_module("std_util").
mercury_std_library_module("store").
mercury_std_library_module("string").
mercury_std_library_module("table_builtin").
mercury_std_library_module("term").
mercury_std_library_module("term_io").
mercury_std_library_module("time").
mercury_std_library_module("tree234").
mercury_std_library_module("type_desc").
mercury_std_library_module("varset").

module_name_to_file_name(ModuleName, Ext, MkDir, FileName) -->
	( { Ext = ".m" } ->
		% Look up the module in the module->file mapping.
		source_file_map__lookup_module_source_file(ModuleName,
			FileName)
	;
		{ prog_out__sym_name_to_string(ModuleName,
			".", BaseFileName) },
		{ string__append_list([BaseFileName, Ext], BaseName) },
		choose_file_name(ModuleName, BaseName, Ext, MkDir, FileName)
	).

module_name_to_lib_file_name(Prefix, ModuleName, Ext, MkDir, FileName) -->
	{ prog_out__sym_name_to_string(ModuleName, ".", BaseFileName) },
	{ string__append_list([Prefix, BaseFileName, Ext], BaseName) },
	choose_file_name(ModuleName, BaseName, Ext, MkDir, FileName).

fact_table_file_name(ModuleName, FactTableFileName, Ext, FileName) -->
	extra_link_obj_file_name(ModuleName, FactTableFileName, Ext, FileName).

	% extra_link_obj_file_name(Module, ExtraLinkObjName, Ext, FileName):
	%	Returns the filename to use when compiling extra objects
	%	that must be linked into the executable
	%	(currently used only for fact tables).
:- pred extra_link_obj_file_name(module_name, file_name, string, file_name,
				io__state, io__state).
:- mode extra_link_obj_file_name(in, in, in, out, di, uo) is det.
extra_link_obj_file_name(ModuleName, ExtraLinkObjName, Ext, FileName) -->
	{ string__append(ExtraLinkObjName, Ext, BaseName) },
	choose_file_name(ModuleName, BaseName, Ext, no, FileName).

:- pred choose_file_name(module_name, string, string, bool, file_name,
			io__state, io__state).
:- mode choose_file_name(in, in, in, in, out, di, uo) is det.

choose_file_name(ModuleName, BaseName, Ext, MkDir, FileName) -->
	globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
	( { UseSubdirs = no } ->
		{ FileName0 = BaseName }
	;
		%
		% the source files, the final executables,
		% library files (including .init files)
		% output files intended for use by the user,
		% and phony Mmake targets names go in the current directory
		%
		{
			% executable files
			( Ext = ""
			; Ext = ".split"
			% library files
			; Ext = ".a"
			; Ext = ".$A"
			; Ext = ".so"
			; Ext = ".$(EXT_FOR_SHARED_LIB)"
			; Ext = ".split.a"
			; Ext = ".split.$A"
			; Ext = ".split.so"
			; Ext = ".split.$(EXT_FOR_SHARED_LIB)"
			; Ext = ".init"
			% output files intended for use by the user
			% (the .h_dump* and .c_dump* MLDS dumps also
			% fit into this category, but for efficiency,
			% to keep this as a switch, we deal with them below)
			; Ext = ".h"
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
			; Ext = ".install_hdrs"
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
			% The current interface to `mercury_update_interface'
			% requires .h.tmp files to be in the same directory as
			% the .h files
			; Ext = ".h.tmp"
			% The following files are only used by the Aditi
			% query shell which doesn't know about --use-subdirs.
			; Ext = ".base_schema"
			; Ext = ".derived_schema"
			; Ext = ".rlo"
			)
		;
			% output files intended for use by the user
			( string__prefix(Ext, ".c_dump")
			; string__prefix(Ext, ".h_dump")
			)
		}
	->
		{ FileName0 = BaseName }
	;
		%
		% we need to handle a few cases specially
		%
		{
			( Ext = ".dir/*.o"
			; Ext = ".dir/*.$O"
			)

		->
			SubDirName = "dirs"
		;
			% .$O and .pic_o files need to go in the
			% same directory, so that using
			% .$(EXT_FOR_PIC_OBJECTS) will work.
			( Ext = ".o"
			; Ext = ".$O"
			; Ext = ".pic_o"
			; Ext = "$(EXT_FOR_PIC_OBJECTS)"
			; Ext = "_init.o"
			; Ext = "_init.$O"
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
			string__append_list(["unknown extension `", Ext, "'"],
				ErrorMsg),
			error(ErrorMsg)
		},
		{ dir__directory_separator(SlashChar) },
		{ string__char_to_string(SlashChar, Slash) },
		{ string__append_list(["Mercury", Slash, SubDirName],
			DirName) },
		( { MkDir = yes } ->
			make_directory(DirName)
		;
			[]
		),
		{ string__append_list([DirName, Slash, BaseName], FileName0) }
	),
	%
	% For --high-level-code, the header files for the standard
	% library are named specially (they get a `mercury.' prefix).
	%
	globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
	{
		HighLevelCode = yes,
		( Ext = ".h" ; Ext = ".h.tmp" ),
		ModuleName = unqualified(UnqualModuleName),
		mercury_std_library_module(UnqualModuleName),
		\+ string__prefix(FileName0, "mercury.")
	->
		string__append("mercury.", FileName0, FileName)
	;
		FileName = FileName0
	}.

module_name_to_split_c_file_name(ModuleName, Num, Ext, FileName) -->
	module_name_to_file_name(ModuleName, ".dir", no, DirName),
	{ unqualify_name(ModuleName, BaseFileName) },
	{ dir__directory_separator(Slash) },
	{ string__format("%s%c%s_%03d%s",
		[s(DirName), c(Slash), s(BaseFileName), i(Num), s(Ext)],
		FileName) }.

module_name_to_split_c_file_pattern(ModuleName, Ext, Pattern) -->
	module_name_to_file_name(ModuleName, ".dir", no, DirName),
	{ dir__directory_separator(Slash) },
	{ string__format("%s%c*%s",
		[s(DirName), c(Slash), s(Ext)],
		Pattern) }.

file_name_to_module_name(FileName, ModuleName) :-
	string_to_sym_name(FileName, ".", ModuleName).

module_name_to_make_var_name(ModuleName, MakeVarName) :-
	prog_out__sym_name_to_string(ModuleName, ".", MakeVarName).

make_directory(DirName) -->
	( { dir__this_directory(DirName) } ->
		[]
	;
		{ make_command_string(string__format(
			"[ -d %s ] || mkdir -p %s",
			[s(DirName), s(DirName)]), forward, Command) },
		io__call_system(Command, _Result)
	).

%-----------------------------------------------------------------------------%

	% Read in the .int3 files that the current module depends on,
	% and use these to qualify all the declarations
	% as much as possible. Then write out the .int0 file.
make_private_interface(SourceFileName, SourceFileModuleName, ModuleName,
		MaybeTimestamp, Items0) -->
	grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
		ModuleName, Items0, Module, Error),
		%
		% Check whether we succeeded
		%
	% XXX zs: why does this code not check for fatal_module_errors?
	( { Error = some_module_errors } ->
		module_name_to_file_name(ModuleName, ".int0", no, FileName),
		io__write_strings(["Error reading interface files.\n",
				"`", FileName, "' not written.\n"])
	;
			%
			% Module-qualify all items.
			%
		{ module_imports_get_items(Module, Items1) },
		module_qual__module_qualify_items(Items1,
				Items2, ModuleName, yes, _, _, _, _),
		io__get_exit_status(Status),
		( { Status \= 0 } ->
			module_name_to_file_name(ModuleName, ".int0", no,
				FileName),
			io__write_strings(["`", FileName, "' not written.\n"])
		;
				%
				% Write out the `.int0' file.
				%
			{ strip_imported_items(Items2, [], Items3) },
			{ strip_clauses_from_interface(Items3, Items4) },
			{ list__map(
			    (pred(Item0::in, Item::out) is det :-
				Item0 = Item1 - Context,
				( make_abstract_instance(Item1, Item2) ->
					Item = Item2 - Context
				;
					Item = Item0
				)
			    ), Items4, Items) },
				
			write_interface_file(SourceFileName, ModuleName,
				".int0", MaybeTimestamp, Items),
			touch_interface_datestamp(ModuleName, ".date0")
		)
	).

	% Read in the .int3 files that the current module depends on,
	% and use these to qualify all items in the interface as much as
	% possible. Then write out the .int and .int2 files.
make_interface(SourceFileName, SourceFileModuleName, ModuleName,
		MaybeTimestamp, Items0) -->
	{ get_interface(Items0, InterfaceItems0) },
		% 
		% Get the .int3 files for imported modules
		%
	grab_unqual_imported_modules(SourceFileName, SourceFileModuleName,
		ModuleName, InterfaceItems0, Module0, Error),

		%
		% Check whether we succeeded
		%
	{ module_imports_get_items(Module0, InterfaceItems1) },
	% XXX zs: why does this code not check for fatal_module_errors?
	( { Error = some_module_errors } ->
		module_name_to_file_name(ModuleName, ".int", no, IntFileName),
		module_name_to_file_name(ModuleName, ".int2", no, Int2FileName),
		io__write_strings(["Error reading short interface files.\n",
				"`", IntFileName, "' and ",
				"`", Int2FileName, "' not written.\n"])
	;
			%
			% Module-qualify all items.
			%
		module_qual__module_qualify_items(InterfaceItems1,
				InterfaceItems2, ModuleName, yes, _, _, _, _),
		io__get_exit_status(Status),
		( { Status \= 0 } ->
			module_name_to_file_name(ModuleName, ".int", no,
				IntFileName),
			io__write_strings(["`", IntFileName, "' ",
				"not written.\n"])
		;
			%
			% Strip out the imported interfaces,
			% assertions are also stripped since they should
			% only be written to .opt files,
			% check for some warnings, and then 
			% write out the `.int' and `int2' files
			% and touch the `.date' file.
			%
			{ strip_imported_items(InterfaceItems2, [],
							InterfaceItems3) },
			{ strip_assertions(InterfaceItems3, InterfaceItems4) },
			check_for_clauses_in_interface(InterfaceItems4,
							InterfaceItems),
			check_int_for_no_exports(InterfaceItems, ModuleName),
			write_interface_file(SourceFileName, ModuleName,
				".int", MaybeTimestamp, InterfaceItems),
			{ get_short_interface(InterfaceItems, int2,
						ShortInterfaceItems) },
			write_interface_file(SourceFileName, ModuleName,
				".int2", MaybeTimestamp, ShortInterfaceItems),
			touch_interface_datestamp(ModuleName, ".date")
		)
	).

	% This qualifies everything as much as it can given the
	% information in the current module and writes out the .int3 file.
make_short_interface(SourceFileName, ModuleName, Items0) -->
	{ get_interface(Items0, InterfaceItems0) },
		% assertions are also stripped since they should
		% only be written to .opt files,
	{ strip_assertions(InterfaceItems0, InterfaceItems1) },
	check_for_clauses_in_interface(InterfaceItems1, InterfaceItems),
	{ get_short_interface(InterfaceItems, int3, ShortInterfaceItems0) },
	module_qual__module_qualify_items(ShortInterfaceItems0,
			ShortInterfaceItems, ModuleName, no, _, _, _, _),
	write_interface_file(SourceFileName, ModuleName, ".int3",
			no, ShortInterfaceItems),
	touch_interface_datestamp(ModuleName, ".date3").

%-----------------------------------------------------------------------------%

strip_imported_items(Items0, Items) :-
	strip_imported_items(Items0, [], Items).

:- pred strip_imported_items(item_list::in, item_list::in,
						item_list::out) is det.

strip_imported_items([], Items0, Items) :-
	list__reverse(Items0, Items). 
strip_imported_items([Item - Context | Rest], Items0, Items) :-
	( Item = module_defn(_, imported(_)) ->
		list__reverse(Items0, Items)
	; Item = module_defn(_, used(_)) ->
		list__reverse(Items0, Items)
	;
		strip_imported_items(Rest, [Item - Context | Items0], Items)
	).

:- pred strip_assertions(item_list::in, item_list::out) is det.

strip_assertions([], []).
strip_assertions([Item - Context | Rest], Items) :-
	( 
		Item = promise(true, _, _, _)
	->
		strip_assertions(Rest, Items)
	; 
		strip_assertions(Rest, Items0),
		Items = [Item - Context | Items0]

	).

:- pred check_for_clauses_in_interface(item_list, item_list,
					io__state, io__state).
:- mode check_for_clauses_in_interface(in, out, di, uo) is det.

check_for_clauses_in_interface([], []) --> [].
check_for_clauses_in_interface([ItemAndContext0 | Items0], Items) -->
	{ ItemAndContext0 = Item0 - Context },
	(
		{ Item0 = clause(_,_,_,_,_) }
	->
		prog_out__write_context(Context),
		report_warning("Warning: clause in module interface.\n"),
		check_for_clauses_in_interface(Items0, Items)
	;
		{ Item0 = pragma(Pragma) },
		{ pragma_allowed_in_interface(Pragma, no) }
	->
		prog_out__write_context(Context),
		report_warning("Warning: pragma in module interface.\n"),
		check_for_clauses_in_interface(Items0, Items)
	;
		{ Items = [ItemAndContext0 | Items1] },
		check_for_clauses_in_interface(Items0, Items1)
	).

% strip_clauses_from_interface is the same as check_for_clauses_in_interface
% except that it doesn't issue any warnings, and that it also strips out
% the `:- interface' and `:- implementation' declarations.
%
% This is used when creating the private interface (`.int0') files
% for packages with sub-modules.

:- pred strip_clauses_from_interface(item_list, item_list).
:- mode strip_clauses_from_interface(in, out) is det.

strip_clauses_from_interface(Items0, Items) :-
	split_clauses_and_decls(Items0, _Clauses, Items).


:- pred split_clauses_and_decls(item_list, item_list, item_list).
:- mode split_clauses_and_decls(in, out, out) is det.

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
		( Item0 = clause(_,_,_,_,_)
		; Item0 = pragma(Pragma),
		  pragma_allowed_in_interface(Pragma, no)
		)
	->
		split_clauses_and_decls(Items0, ClauseItems1, InterfaceItems),
		ClauseItems = [ItemAndContext0 | ClauseItems1]
	;
		split_clauses_and_decls(Items0, ClauseItems, InterfaceItems1),
		InterfaceItems = [ItemAndContext0 | InterfaceItems1]
	).

% pragma `obsolete', `terminates', `does_not_terminate' 
% `termination_info', `check_termination', `aditi', `base_relation'
% and `owner' pragma declarations are supposed to go in the interface,
% but all other pragma declarations are implementation
% details only, and should go in the implementation.

% XXX we should allow c_header_code;
% but if we do allow it, we should put it in the generated
% header file, which currently we don't.

pragma_allowed_in_interface(foreign_decl(_, _), no).
pragma_allowed_in_interface(foreign_import_module(_, _), no).
pragma_allowed_in_interface(foreign_code(_, _), no).
pragma_allowed_in_interface(foreign_proc(_, _, _, _, _, _), no).
pragma_allowed_in_interface(foreign_type(_, _, _), yes).
pragma_allowed_in_interface(inline(_, _), no).
pragma_allowed_in_interface(no_inline(_, _), no).
pragma_allowed_in_interface(obsolete(_, _), yes).
pragma_allowed_in_interface(export(_, _, _, _), no).
pragma_allowed_in_interface(import(_, _, _, _, _), no).
pragma_allowed_in_interface(source_file(_), yes).
	% yes, but the parser will strip out `source_file' pragmas anyway...
pragma_allowed_in_interface(fact_table(_, _, _), no).
pragma_allowed_in_interface(tabled(_, _, _, _, _), no).
pragma_allowed_in_interface(promise_pure(_, _), no).
pragma_allowed_in_interface(promise_semipure(_, _), no).
pragma_allowed_in_interface(unused_args(_, _, _, _, _), no).
pragma_allowed_in_interface(type_spec(_, _, _, _, _, _, _, _), yes).
pragma_allowed_in_interface(termination_info(_, _, _, _, _), yes).
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

check_for_no_exports(Items, ModuleName) -->
	globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning),
	( { ExportWarning = no } ->
		[]
	;
		{ get_interface(Items, InterfaceItems) },
		check_int_for_no_exports(InterfaceItems, ModuleName)
	).

	% Given a module name and a list of the items in that module's
	% interface, this procedure checks if the module doesn't export
	% anything, and if so, and --warn-nothing-exported is set, it reports
	% a warning.
:- pred check_int_for_no_exports(item_list, module_name, io__state, io__state).
:- mode check_int_for_no_exports(in, in, di, uo) is det.

check_int_for_no_exports([], ModuleName) -->
	warn_no_exports(ModuleName).
check_int_for_no_exports([Item - _Context | Items], ModuleName) -->
	(
		{ Item = nothing(_)
		; Item = module_defn(_, ModuleDefn),
		  ModuleDefn \= include_module(_)
		}
	->
		% nothing useful - keep searching
		check_int_for_no_exports(Items, ModuleName)
	;
		% we found something useful - don't issue the warning
		[]
	).

:- pred warn_no_exports(module_name, io__state, io__state).
:- mode warn_no_exports(in, di, uo) is det.

warn_no_exports(ModuleName) -->
	globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning),
	( 	
		{ ExportWarning = yes }
	->
		module_name_to_file_name(ModuleName, ".m", no, FileName),
		{ sym_name_to_string(ModuleName, ModuleNameString) },
		{ string__append_list(["interface for module `",
			ModuleNameString, "' does not export anything."],
			Message) },
		report_warning(FileName, 1, Message),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		(
			{ VerboseErrors = yes }
		->
			io__write_strings([ "\t\t",
	"To be useful, a module should export something.\n\t\t",
	"A file should contain at least one declaration other than\n\t\t",
	"`:- import_module' in its interface section(s).\n\t\t",
	"This would normally be a `:- pred', `:- func', `:- type',\n\t\t",
	"`:- inst' or `:- mode' declaration.\n"
				])
		;
			[]
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred write_interface_file(file_name, module_name, string,
		maybe(timestamp), item_list, io__state, io__state).
:- mode write_interface_file(in, in, in, in, in, di, uo) is det.

write_interface_file(_SourceFileName, ModuleName, Suffix,
		MaybeTimestamp, InterfaceItems0) -->

		% Create (e.g.) `foo.int.tmp'.
	{ string__append(Suffix, ".tmp", TmpSuffix) },
	module_name_to_file_name(ModuleName, Suffix, yes, OutputFileName),
	module_name_to_file_name(ModuleName, TmpSuffix, no, TmpOutputFileName),

	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	globals__io_set_option(line_numbers, bool(no)),

	globals__io_lookup_bool_option(generate_item_version_numbers,
		GenerateVersionNumbers),

	( { GenerateVersionNumbers = yes } ->
		% Find the timestamp of the current module.
		(
			{ MaybeTimestamp = yes(Timestamp) },

			% Read in the previous version of the file.
			read_mod_ignore_errors(ModuleName, Suffix,
				"Reading old interface for module", yes, no,
				OldItems0, OldError, _OldIntFileName,
				_OldTimestamp),
			( { OldError = no_module_errors } ->
				{ strip_off_interface_decl(OldItems0,
					OldItems) },
				{ MaybeOldItems = yes(OldItems) }
			;
				% If we can't read in the old file, the
				% timestamps will all be set to the
				% modification time of the source file.
				{ MaybeOldItems = no }
			),
			{ recompilation__version__compute_version_numbers(
				Timestamp, InterfaceItems0, MaybeOldItems,
				VersionNumbers) },
			{ VersionNumberItem = module_defn(VarSet,
				version_numbers(ModuleName, VersionNumbers))
				- Context },
			{ InterfaceItems1 =
				[VersionNumberItem | InterfaceItems0] }
		;
			{ MaybeTimestamp = no },
			{ error(
"write_interface_file with `--smart-recompilation', timestamp not read") }
		)
	;
		{ InterfaceItems1 = InterfaceItems0 }
	),

		% Add a `:- interface' declaration at the start
		% of the item list.
	{ varset__init(VarSet) },
	{ term__context_init(Context) },
	{ InterfaceDeclaration = module_defn(VarSet, interface) - Context },
	{ InterfaceItems = [InterfaceDeclaration | InterfaceItems1] },

	convert_to_mercury(ModuleName, TmpOutputFileName, InterfaceItems),
	globals__io_set_option(line_numbers, bool(LineNumbers)),
	update_interface(OutputFileName).

		% invoke the shell script `mercury_update_interface'
		% to update <Module>.int from <Module>.int.tmp if
		% necessary

update_interface(OutputFileName) -->
	update_interface(OutputFileName, Succeeded),
	( { Succeeded = no } ->
		report_error("problem updating interface files.")
	;
		[]
	).

update_interface(OutputFileName, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Updating interface:\n"),
	( { Verbose = yes } ->
		{ Command = "mercury_update_interface -v " }
	;
		{ Command = "mercury_update_interface " }
	),
	{ string__append(Command, OutputFileName, ShellCommand) },
	io__output_stream(OutputStream),
	invoke_shell_command(OutputStream, verbose, ShellCommand, Succeeded).

%-----------------------------------------------------------------------------%

touch_interface_datestamp(ModuleName, Ext) -->
	module_name_to_file_name(ModuleName, Ext, yes, OutputFileName),
	touch_datestamp(OutputFileName).

touch_datestamp(OutputFileName) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Touching `"),
	maybe_write_string(Verbose, OutputFileName),
	maybe_write_string(Verbose, "'... "),
	maybe_flush_output(Verbose),
	io__open_output(OutputFileName, Result),
	( { Result = ok(OutputStream) },
		io__write_string(OutputStream, "\n"),
		io__close_output(OutputStream),
		maybe_write_string(Verbose, " done.\n")
	; { Result = error(IOError) },
		{ io__error_message(IOError, IOErrorMessage) },
		io__write_string("\nError opening `"),
		io__write_string(OutputFileName),
		io__write_string("'for output: "),
		io__write_string(IOErrorMessage),
		io__write_string(".\n")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

grab_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
		NestedChildren, ReadModules, MaybeTimestamp,
		Items0, Module, Error) -->
		%
		% Find out which modules this one depends on
		%
	{ AncestorModules = get_ancestors(ModuleName) },
	{ get_dependencies(Items0, IntImportedModules0, IntUsedModules0,
			ImpImportedModules0, ImpUsedModules0) },

	{ list__append(IntImportedModules0, ImpImportedModules0,
			ImportedModules0) },
	{ list__append(IntUsedModules0, ImpUsedModules0, UsedModules0) },

	warn_if_import_self_or_ancestor(ModuleName, AncestorModules,
		ImportedModules0, UsedModules0),

	warn_if_duplicate_use_import_decls(ModuleName,
			IntImportedModules0, IntImportedModules1, 
			IntUsedModules0, IntUsedModules1, 
			ImpImportedModules0, ImpImportedModules, 
			ImpUsedModules0, ImpUsedModules),

	{ get_fact_table_dependencies(Items0, FactDeps) },
	{ get_interface(Items0, InterfaceItems) },
	{ get_children(InterfaceItems, PublicChildren) },
	{ MaybeTimestamp = yes(Timestamp) ->
		MaybeTimestamps = yes(map__det_insert(map__init, ModuleName,
			module_timestamp(".m", Timestamp, may_be_unqualified)))
	;
		MaybeTimestamps = no	

	},
	{ init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
		Items0, PublicChildren, NestedChildren, FactDeps,
		MaybeTimestamps, Module0) },

		% If this module has any seperately-compiled sub-modules,
		% then we need to make everything in this module
		% exported_to_submodules.  We do that by splitting
		% out the declarations and putting them in a special
		% `:- private_interface' section.
	{ get_children(Items0, Children) },
	{ Children = [] ->
		Module1 = Module0
	;
		split_clauses_and_decls(Items0, Clauses, Decls),
		make_pseudo_decl(private_interface, PrivateInterfaceDecl),
		make_pseudo_decl(implementation, ImplementationDecl),
		list__append([PrivateInterfaceDecl | Decls],
			[ImplementationDecl | Clauses], Items1),
		module_imports_set_items(Module0, Items1, Module1)
	},

		% We add a pseudo-declarations `:- imported' at the end
		% of the item list. Uses of the items with declarations 
		% following this do not need module qualifiers.
	{ append_pseudo_decl(Module1, imported(interface), Module2) },

		% Add `builtin' and `private_builtin' to the
		% list of imported modules
	globals__io_get_globals(Globals),
	{ add_implicit_imports(Items0, Globals,
			IntImportedModules1, IntUsedModules1,
			IntImportedModules2, IntUsedModules2) },

		% Process the ancestor modules
	process_module_private_interfaces(ReadModules, AncestorModules,
		IntImportedModules2, IntImportedModules,
		IntUsedModules2, IntUsedModules,
		Module2, Module3),

		% Process the modules imported using `import_module'.
	{ IntIndirectImports0 = [] },
	process_module_long_interfaces(ReadModules, may_be_unqualified,
		IntImportedModules, ".int", IntIndirectImports0,
		IntIndirectImports1, Module3, Module4),

	{ append_pseudo_decl(Module4, imported(implementation), Module5) },

	{ ImpIndirectImports0 = [] },
	process_module_long_interfaces(ReadModules, may_be_unqualified,
		ImpImportedModules, ".int", ImpIndirectImports0,
		ImpIndirectImports1, Module5, Module6),

		% Process the modules imported using `use_module' .
	{ append_pseudo_decl(Module6, used(interface), Module7) },
	process_module_long_interfaces(ReadModules, must_be_qualified,
		IntUsedModules, ".int", IntIndirectImports1,
		IntIndirectImports, Module7, Module8),
	{ append_pseudo_decl(Module8, used(implementation), Module9) },
	process_module_long_interfaces(ReadModules, must_be_qualified,
		ImpUsedModules, ".int", ImpIndirectImports1,
		ImpIndirectImports, Module9, Module10),

		% Process the short interfaces for indirectly imported modules.
		% The short interfaces are treated as if
		% they are imported using `use_module'.
	{ append_pseudo_decl(Module10, transitively_imported, Module11) },
	{ append_pseudo_decl(Module11, used(interface), Module12) },
	process_module_short_interfaces_transitively(ReadModules,
		IntIndirectImports, ".int2", Module12, Module13),
	{ append_pseudo_decl(Module13, used(implementation), Module14) },
	process_module_short_interfaces_transitively(ReadModules,
		ImpIndirectImports, ".int2", Module14, Module),

	{ module_imports_get_items(Module, Items) },
	check_imports_accessibility(ModuleName,
		IntImportedModules ++ IntUsedModules ++
		ImpImportedModules ++ ImpUsedModules, Items),

	{ module_imports_get_error(Module, Error) }.

% grab_unqual_imported_modules:
%	like grab_imported_modules, but gets the `.int3' files
%	instead of the `.int' and `.int2' files.

grab_unqual_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
		Items0, Module, Error) -->
		%
		% Find out which modules this one depends on
		%
	{ ParentDeps = get_ancestors(ModuleName) },
	{ get_dependencies(Items0, IntImportDeps0, IntUseDeps0,
			ImpImportDeps0, ImpUseDeps0) },

		%
		% Construct the initial module import structure,
		% and append a `:- imported' decl to the items.
		%
	{ init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
		Items0, [], [], [], no, Module0) },
	{ append_pseudo_decl(Module0, imported(interface), Module1) },

		% Add `builtin' and `private_builtin' to the imported modules.
	globals__io_get_globals(Globals),
	{ add_implicit_imports(Items0, Globals, IntImportDeps0, IntUseDeps0,
			IntImportDeps1, IntUseDeps1) },

		%
		% Get the .int3s and .int0s that the current module depends on.
		%
	{ map__init(ReadModules) },

		% first the .int0s for parent modules
	process_module_private_interfaces(ReadModules, ParentDeps,
			IntImportDeps1, IntImportDeps, IntUseDeps1, IntUseDeps,
			Module1, Module2),

		% then the .int3s for `:- import'-ed modules
	process_module_long_interfaces(ReadModules, may_be_unqualified,
			IntImportDeps, ".int3", [],
			IntIndirectImportDeps0, Module2, Module3),

	{ append_pseudo_decl(Module3, imported(implementation), Module4) },

	process_module_private_interfaces(ReadModules, ParentDeps,
			ImpImportDeps0, ImpImportDeps, ImpUseDeps0, ImpUseDeps,
			Module4, Module5),

	process_module_long_interfaces(ReadModules, may_be_unqualified,
			ImpImportDeps, ".int3", [], ImpIndirectImportDeps0,
			Module5, Module6),

		% then (after appropriate `:- used' decls)
		% the .int3s for `:- use'-ed modules
	{ append_pseudo_decl(Module6, used(interface), Module7) },
	process_module_long_interfaces(ReadModules, must_be_qualified,
			IntUseDeps, ".int3", IntIndirectImportDeps0,
			IntIndirectImportDeps, Module7, Module8),
	{ append_pseudo_decl(Module8, used(implementation), Module9) },
	process_module_long_interfaces(ReadModules, must_be_qualified,
			ImpUseDeps, ".int3", ImpIndirectImportDeps0,
			ImpIndirectImportDeps, Module9, Module10),

		% then (after appropriate `:- used' decl)
		% the .int3s for indirectly imported modules
	{ append_pseudo_decl(Module10, used(interface), Module11) },
	process_module_short_interfaces_transitively(ReadModules,
			IntIndirectImportDeps, ".int3", Module11, Module12),

	{ append_pseudo_decl(Module12, used(implementation), Module13) },
	process_module_short_interfaces_transitively(ReadModules,
			ImpIndirectImportDeps, ".int3", Module13, Module),

	{ module_imports_get_items(Module, Items) },
	check_imports_accessibility(ModuleName,
		IntImportDeps ++ IntUseDeps ++ ImpImportDeps ++ ImpUseDeps,
		Items),

	{ module_imports_get_error(Module, Error) }.

%-----------------------------------------------------------------------------%

find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp,
		Items, MaybeTimestamp, Error, FileName) :-
	map__search(ReadModules, ModuleName - Suffix, ReadModule),
	ReadModule = read_module(ModuleTimestamp, Items, Error, FileName),
	( ReturnTimestamp = yes ->
		ModuleTimestamp = module_timestamp(_, Timestamp, _),
		MaybeTimestamp = yes(Timestamp)
	;
		MaybeTimestamp = no
	).

:- pred init_module_imports(file_name, module_name, module_name, item_list,
			list(module_name), list(module_name), list(string),
			maybe(module_timestamps), module_imports).
:- mode init_module_imports(in, in, in, in, in, in, in, in, out) is det.

init_module_imports(SourceFileName, SourceFileModuleName, ModuleName,
		Items, PublicChildren, NestedChildren, FactDeps,
		MaybeTimestamps, Module) :-
	Module = module_imports(SourceFileName, SourceFileModuleName,
		ModuleName, [], [], [], [], [], PublicChildren,
		NestedChildren, FactDeps, unknown, [], no_foreign_export,
		Items, no_module_errors,
		MaybeTimestamps, no_main, dir__this_directory).

module_imports_get_source_file_name(Module, Module ^ source_file_name).
module_imports_get_module_name(Module, Module ^ module_name).
module_imports_get_impl_deps(Module, Module ^ impl_deps).
module_imports_get_items(Module, Module ^ items).
module_imports_set_items(Module, Items, Module ^ items := Items).
module_imports_get_error(Module, Module ^ error).
module_imports_set_error(Module, Error, Module ^ error := Error).
module_imports_set_int_deps(Module, IntDeps, Module ^ int_deps := IntDeps).
module_imports_set_impl_deps(Module, ImplDeps,
	Module ^ impl_deps := ImplDeps).
module_imports_set_indirect_deps(Module, IndirectDeps,
	Module ^ indirect_deps := IndirectDeps).

append_pseudo_decl(Module0, PseudoDecl, Module) :-
	Items0 = Module0 ^ items,
	make_pseudo_decl(PseudoDecl, Item),
	list__append(Items0, [Item], Items),
	Module = Module0 ^ items := Items.

make_pseudo_decl(PseudoDecl, Item) :-
	term__context_init(Context),
	varset__init(Varset),
	Item = module_defn(Varset, PseudoDecl) - Context.

%-----------------------------------------------------------------------------%

get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps) :-
	add_implicit_imports(Items, Globals, [], [], ImportDeps, UseDeps).

:- pred add_implicit_imports(item_list, globals,
			list(module_name), list(module_name),
			list(module_name), list(module_name)).
:- mode add_implicit_imports(in, in, in, in, out, out) is det.

add_implicit_imports(Items, Globals, ImportDeps0, UseDeps0,
		ImportDeps, UseDeps) :-
	mercury_public_builtin_module(MercuryPublicBuiltin),
	mercury_private_builtin_module(MercuryPrivateBuiltin),
	mercury_table_builtin_module(MercuryTableBuiltin),
	mercury_profiling_builtin_module(MercuryProfilingBuiltin),
	ImportDeps = [MercuryPublicBuiltin | ImportDeps0],
	UseDeps1 = [MercuryPrivateBuiltin | UseDeps0],
	(
		%
		% we should include MercuryTableBuiltin if
		% the Items contain a tabling pragma, or if
		% --trace-table-io is specified
		%
		( contains_tabling_pragma(Items)
		; globals__lookup_bool_option(Globals, trace_table_io, yes)
		)
	->
		UseDeps2 = [MercuryTableBuiltin | UseDeps1]
	;
		UseDeps2 = UseDeps1
	),
	( globals__lookup_bool_option(Globals, profile_deep, yes) ->
		UseDeps = [MercuryProfilingBuiltin|UseDeps2]
	;
		UseDeps = UseDeps2
	).


:- pred contains_tabling_pragma(item_list::in) is semidet.

contains_tabling_pragma([Item|Items]) :-
	(
		Item = pragma(Pragma) - _Context,
		Pragma = tabled(_, _, _, _, _)
	;
		contains_tabling_pragma(Items)
	).

:- pred warn_if_import_self_or_ancestor(module_name, list(module_name),
		list(module_name), list(module_name), 
		io__state, io__state).
:- mode warn_if_import_self_or_ancestor(in, in, in, in, di, uo) is det.

% Warn if a module imports itself, or an ancestor.

warn_if_import_self_or_ancestor(ModuleName, AncestorModules,
		ImportedModules, UsedModules) -->
	globals__io_lookup_bool_option(warn_simple_code, Warn),
	( { Warn = yes } ->
		(
			{ list__member(ModuleName, ImportedModules)
			; list__member(ModuleName, UsedModules)
			}
		->
			module_name_to_file_name(ModuleName, ".m", no,
				FileName),
			{ term__context_init(FileName, 1, Context) },
			prog_out__write_context(Context),
			report_warning("Warning: module `"),
			prog_out__write_sym_name(ModuleName),
			io__write_string("' imports itself!\n")
		;
			[]
		),
		{ IsImportedAncestor = lambda([Import::out] is nondet, (
			list__member(Import, AncestorModules),
			( list__member(Import, ImportedModules)
			; list__member(Import, UsedModules)
			))) },
		aggregate(IsImportedAncestor,
			warn_imported_ancestor(ModuleName))
	;
		[]
	).

:- pred warn_imported_ancestor(module_name, module_name, io__state, io__state).
:- mode warn_imported_ancestor(in, in, di, uo) is det.

warn_imported_ancestor(ModuleName, AncestorName) -->
	module_name_to_file_name(ModuleName, ".m", no, FileName),
	{ term__context_init(FileName, 1, Context) },
	prog_out__write_context(Context),
	report_warning("module `"),
	prog_out__write_sym_name(ModuleName),
	io__write_string("' imports its own ancestor,\n"),
	prog_out__write_context(Context),
	io__write_string("  module `"),
	prog_out__write_sym_name(AncestorName),
	io__write_string("'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_strings([
		"\tEvery sub-module implicitly imports its ancestors.\n",
		"\tThere is no need to explicitly import them.\n"
		])
	;
		[]
	).

:- pred warn_if_duplicate_use_import_decls(module_name, list(module_name),
		list(module_name), list(module_name), list(module_name), 
		list(module_name), list(module_name), list(module_name), 
		list(module_name), io__state, io__state).
:- mode warn_if_duplicate_use_import_decls(in, in, out, in, out, in, out,
		in, out, di, uo) is det.

% This predicate ensures that all every import_module declaration is
% checked against every use_module declaration, except for the case
% where the interface has `:- use_module foo.' and the implementation
% `:- import_module foo.'.
% warn_if_duplicate_use_import_decls/7 is called to generate the actual
% warnings.

warn_if_duplicate_use_import_decls(ModuleName,
		IntImportedModules0, IntImportedModules, 
		IntUsedModules0, IntUsedModules, 
		ImpImportedModules0, ImpImportedModules, 
		ImpUsedModules0, ImpUsedModules) -->

	warn_if_duplicate_use_import_decls(ModuleName,
		IntImportedModules0, IntImportedModules1,
		IntUsedModules0, IntUsedModules),
	warn_if_duplicate_use_import_decls(ModuleName,
		IntImportedModules1, IntImportedModules,
		ImpUsedModules0, ImpUsedModules1),

	warn_if_duplicate_use_import_decls(ModuleName,
		ImpImportedModules0, ImpImportedModules,
		ImpUsedModules1, ImpUsedModules).

:- pred warn_if_duplicate_use_import_decls(module_name, list(module_name),
		list(module_name), list(module_name), list(module_name), 
		io__state, io__state).
:- mode warn_if_duplicate_use_import_decls(in, in, out, in, out, di, uo) is det.

% Report warnings for modules imported using both `:- use_module'
% and `:- import_module'.  Remove the unnecessary `:- use_module'
% declarations.

warn_if_duplicate_use_import_decls(ModuleName,
		ImportedModules0, ImportedModules,
		UsedModules0, UsedModules) -->
	{ set__list_to_set(ImportedModules0, ImportedSet) },
	{ set__list_to_set(UsedModules0, UsedSet) },
	{ set__intersect(ImportedSet, UsedSet, BothSet) },
	( { set__empty(BothSet) } ->
		{ ImportedModules = ImportedModules0 },
		{ UsedModules = UsedModules0 }
	;
		{ set__to_sorted_list(BothSet, BothList) },
		globals__io_lookup_bool_option(warn_simple_code, WarnSimple),
		( { WarnSimple = yes } ->
			module_name_to_file_name(ModuleName, ".m", no,
				FileName),
			{ term__context_init(FileName, 1, Context) },
			prog_out__write_context(Context),
			io__write_string("Warning:"),
			( { BothList = [_] } ->
				io__write_string(" module "),
				prog_out__write_module_list(BothList),
				io__write_string(" is ")
			;
				io__write_string(" modules "),
				prog_out__write_module_list(BothList),
				io__write_string(" are ")
			),
			io__write_string("imported using both\n"),
			prog_out__write_context(Context),
			io__write_string("  `:- import_module' and "),
			io__write_string("`:- use_module' declarations.\n"),

			globals__io_lookup_bool_option(halt_at_warn, Halt),
			( { Halt = yes } ->
				io__set_exit_status(1)
			;
				[]
			)
		;
			[]
		),

		% Treat the modules with both types of import as if they 
		% were imported using `:- import_module.'
		{ ImportedModules = ImportedModules0 },
		{ list__delete_elems(UsedModules0, BothList, UsedModules) }
	).

%-----------------------------------------------------------------------------%

write_dependency_file(Module, AllDepsSet, MaybeTransOptDeps) -->
	{ Module = module_imports(SourceFileName, _SourceFileModuleName,
			ModuleName, ParentDeps, IntDeps, ImplDeps,
			IndirectDeps, _Children, InclDeps, _NestDeps,
			FactDeps0, ContainsForeignCode, ForeignImports0,
			_ContainsForeignExport, Items, _Error,
			_Timestamps, _HasMain, _Dir) },
	globals__io_lookup_bool_option(verbose, Verbose),
	{ module_name_to_make_var_name(ModuleName, MakeVarName) },
	module_name_to_file_name(ModuleName, ".d", yes, DependencyFileName),
	module_name_to_file_name(ModuleName, ".trans_opt_date", no,
						TransOptDateFileName),
	%
	% To avoid problems with concurrent updates of `.d' files
	% during parallel makes, we first create the file with a
	% temporary name, and then rename it to the desired name
	% when we've finished.
	%
	{ dir__dirname(DependencyFileName, DirName) },
	io__make_temp(DirName, "tmp_d", TmpDependencyFileName),
	maybe_write_string(Verbose, "% Writing auto-dependency file `"),
	maybe_write_string(Verbose, DependencyFileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__open_output(TmpDependencyFileName, Result),
	( { Result = error(IOError) },
		maybe_write_string(Verbose, " failed.\n"),
		maybe_flush_output(Verbose),
		{ io__error_message(IOError, IOErrorMessage) },
		{ string__append_list(["error opening temporary file `",
			TmpDependencyFileName, "' for output: ",
			IOErrorMessage], Message) },
		report_error(Message)
	; { Result = ok(DepStream) },
		{ list__append(IntDeps, ImplDeps, LongDeps0) },
		{ ShortDeps0 = IndirectDeps },
		{ set__list_to_set(LongDeps0, LongDepsSet0) },
		{ set__delete(LongDepsSet0, ModuleName, LongDepsSet) },
		{ set__list_to_set(ShortDeps0, ShortDepsSet0) },
		{ set__difference(ShortDepsSet0, LongDepsSet, ShortDepsSet1) },
		{ set__delete(ShortDepsSet1, ModuleName, ShortDepsSet) },
		{ set__to_sorted_list(LongDepsSet, LongDeps) },
		{ set__to_sorted_list(ShortDepsSet, ShortDeps) },
		{ set__to_sorted_list(AllDepsSet, AllDeps) },
		{ list__sort_and_remove_dups(FactDeps0, FactDeps) },

		( { MaybeTransOptDeps = yes(TransOptDeps0) } ->
			{ set__list_to_set(TransOptDeps0, TransOptDepsSet0) },
			{ set__intersect(TransOptDepsSet0, LongDepsSet,
				TransOptDepsSet) },
			{ set__to_sorted_list(TransOptDepsSet, 
				TransOptDateDeps) },
			%
			% note that maybe_read_dependency_file searches for
			% this exact pattern
			%
			io__write_strings(DepStream,
				[TransOptDateFileName, " :"]),
			write_dependencies_list(TransOptDateDeps, ".trans_opt", 
				DepStream)
		;
			[]
		),

		( { FactDeps \= [] } ->
			io__write_strings(DepStream, 
				["\n\n", MakeVarName, ".fact_tables ="]),
			write_file_dependencies_list(FactDeps, "", DepStream),
			io__nl(DepStream),
			globals__io_lookup_bool_option(assume_gmake,
				AssumeGmake),
			( { AssumeGmake = yes } ->
				io__write_strings(DepStream, [
					"\n\n", MakeVarName,
					".fact_tables.os = $(", MakeVarName,
					".fact_tables:%=$(os_subdir)%.$O)\n\n",
					MakeVarName,
					".fact_tables.cs = $(", MakeVarName,
					".fact_tables:%=$(cs_subdir)%.c)\n\n"
				])
			;
				io__write_strings(DepStream,
					[MakeVarName, ".fact_tables.cs ="]),
				write_fact_table_dependencies_list(ModuleName,
					FactDeps, ".c", DepStream),
				io__write_strings(DepStream, ["\n\n", 
					MakeVarName, ".fact_tables.os ="]),
				write_fact_table_dependencies_list(ModuleName,
					FactDeps, ".$O", DepStream),
				io__nl(DepStream)
			)
		;
			[]
		),

		{
			string__remove_suffix(SourceFileName, ".m",
				SourceFileBase)
		->
			ErrFileName = SourceFileBase ++ ".err"
		;
			error("modules.m: source file doesn't end in `.m'")
		},
		module_name_to_file_name(ModuleName, ".optdate", no,
					OptDateFileName),
		module_name_to_file_name(ModuleName, ".c_date", no,
			CDateFileName),
		module_name_to_file_name(ModuleName, ".s_date", no,
			AsmDateFileName),
		module_name_to_file_name(ModuleName, ".pic_s_date", no,
			PicAsmDateFileName),
		module_name_to_file_name(ModuleName, ".$O", no, ObjFileName),
		module_name_to_file_name(ModuleName, ".rlo", no, RLOFileName),
		module_name_to_file_name(ModuleName, ".il_date", no,
			ILDateFileName),
		module_name_to_file_name(ModuleName, ".java_date", no,
			JavaDateFileName),
		module_name_to_file_name(ModuleName, ".pic_o", no,
							PicObjFileName),
		module_name_to_file_name(ModuleName, ".int0", no,
							Int0FileName),
		module_name_to_split_c_file_pattern(ModuleName, ".$O",
			SplitObjPattern),
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
		] ),
		write_dependencies_list(ParentDeps, ".optdate", DepStream),
		write_dependencies_list(ParentDeps,
				".trans_opt_date", DepStream),
		write_dependencies_list(ParentDeps, ".c_date", DepStream),
		write_dependencies_list(ParentDeps, ".s_date", DepStream),
		write_dependencies_list(ParentDeps, ".pic_s_date", DepStream),
		write_dependencies_list(ParentDeps, ".dir/*.$O", DepStream),
		write_dependencies_list(ParentDeps, ".rlo", DepStream),
		write_dependencies_list(ParentDeps, ".il_date", DepStream),
		write_dependencies_list(ParentDeps, ".java_date", DepStream),
		io__write_strings(DepStream, [" : ", SourceFileName]),
		% If the module contains nested sub-modules then `.int0'
		% file must first be built.
		( { InclDeps = [_ | _] } ->
			io__write_strings(DepStream, [" ", Int0FileName])
		;
			[]
		),
		write_dependencies_list(ParentDeps, ".int0", DepStream),
		write_dependencies_list(LongDeps, ".int", DepStream),
		write_dependencies_list(ShortDeps, ".int2", DepStream),

		( { FactDeps \= [] } ->
			io__write_strings(DepStream, [
				" \\\n\t$(", MakeVarName, ".fact_tables)\n\n",
				"$(", MakeVarName, ".fact_tables.os) : $(",
				MakeVarName, ".fact_tables) ",
				SourceFileName, "\n\n",
				"$(", MakeVarName, ".fact_tables.cs) : ",
				ObjFileName, "\n"
			] )
		;
			[]
		),

		globals__io_lookup_bool_option(use_opt_files, UseOptFiles),
		globals__io_lookup_bool_option(intermodule_optimization,
			Intermod),
		globals__io_lookup_accumulating_option(intermod_directories,
			IntermodDirs),
		( { Intermod = yes ; UseOptFiles = yes } ->
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
			]),

			% The target (e.g. C) file only depends on the .opt
			% files from the current directory, so that
			% inter-module optimization works when the .opt files
			% for the library are unavailable. This is only
			% necessary because make doesn't allow conditional
			% dependencies.
			% The dependency on the current module's .opt file
			% is to make sure the module gets type-checked without
			% having the definitions of abstract types from other
			% modules.
			%
			% XXX The code here doesn't correctly handle
			% dependencies on `.int' and `.int2' files needed
			% by the `.opt' files.
			globals__io_lookup_bool_option(transitive_optimization,
				TransOpt),
			globals__io_lookup_bool_option(use_trans_opt_files,
				UseTransOpt),

			( { TransOpt = yes ; UseTransOpt = yes } ->
				{ bool__not(UseTransOpt, BuildOptFiles) },
				get_both_opt_deps(BuildOptFiles,
					[ModuleName | LongDeps], IntermodDirs,
					OptDeps, TransOptDeps),
				{ OptInt0Deps = sort_and_remove_dups(
					condense(map(get_ancestors,
					OptDeps))) },
				write_dependencies_list(OptDeps,
					".opt", DepStream),
				write_dependencies_list(OptInt0Deps,
					".int0", DepStream),
				
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
				]),
				write_dependencies_list(TransOptDeps,
					".trans_opt", DepStream)
			;
				{ bool__not(UseOptFiles, BuildOptFiles) },
				get_opt_deps(BuildOptFiles,
					[ModuleName | LongDeps],
					IntermodDirs, ".opt", OptDeps),
				{ OptInt0Deps = sort_and_remove_dups(
					condense(map(get_ancestors,
					OptDeps))) },
				write_dependencies_list(OptDeps,
					".opt", DepStream),
				write_dependencies_list(OptInt0Deps,
					".int0", DepStream)
			)
		;
			[]
		),

		globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
		globals__io_get_target(CompilationTarget),
		( { HighLevelCode = yes, CompilationTarget = c } ->
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
			]),
			write_dependencies_list(AllDeps, ".h", DepStream)
		;
			[]
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
		module_name_to_file_name(ModuleName, ".c", no, CFileName),
		module_name_to_file_name(ModuleName, ".s", no, AsmFileName),
		module_name_to_file_name(ModuleName, ".h", no, HeaderFileName),
		io__write_strings(DepStream, [
				"\n\n",
				"ifeq ($(TARGET_ASM),yes)\n",
				HeaderFileName, " : ", AsmFileName, "\n",
				"else\n",
				HeaderFileName, " : ", CFileName, "\n",
				"endif"
		]),

		%
		% The `.module_dep' file is made as a side effect of
		% creating the `.c', `.s' or `.il'.
		%
		module_name_to_file_name(ModuleName, ".il", no, ILFileName),
		module_name_to_file_name(ModuleName, module_dep_file_extension,
			no, ModuleDepFileName),
		io__write_strings(DepStream, [
				"\n\n",
				"ifeq ($(TARGET_ASM),yes)\n",
				ModuleDepFileName, " : ", AsmFileName, "\n",
				"else\n",
				"ifeq ($(findstring il,$(GRADE)),il)\n",
				ModuleDepFileName, " : ", ILFileName, "\n",
				"else\n",
				ModuleDepFileName, " : ", CFileName, "\n",
				"endif\n",
				"endif"
		]),

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

		module_name_to_file_name(ModuleName, ".date", no,
						DateFileName),
		module_name_to_file_name(ModuleName, ".date0", no,
						Date0FileName),
		io__write_strings(DepStream, [
				"\n\n", DateFileName, " ",
				Date0FileName
		]),
		write_dependencies_list(ParentDeps, ".date", DepStream),
		io__write_strings(DepStream, [
				" : ",
				SourceFileName
		]),
		write_dependencies_list(ParentDeps, ".int0", DepStream),
		write_dependencies_list(LongDeps, ".int3", DepStream),
		write_dependencies_list(ShortDeps, ".int3", DepStream),

		io__write_strings(DepStream, ["\n\n", Date0FileName]),
		write_dependencies_list(ParentDeps, ".date0", DepStream),
		io__write_strings(DepStream, [
				" : ",
				SourceFileName
		]),
		write_dependencies_list(LongDeps, ".int3", DepStream),
		write_dependencies_list(ShortDeps, ".int3", DepStream),

		module_name_to_file_name(ModuleName, ".dir", no, DirFileName),
		module_name_to_split_c_file_name(ModuleName, 0, ".$O",
			SplitCObj0FileName),
		io__write_strings(DepStream, [
			"\n\n",
			SplitCObj0FileName, " : ",
				SourceFileName, "\n",
			"\trm -rf ", DirFileName, "\n",
			"\t$(MCS) $(ALL_GRADEFLAGS) $(ALL_MCSFLAGS) ",
				SourceFileName, "\n\n"
		]),

		globals__io_get_target(Target),
		globals__io_lookup_bool_option(sign_assembly, SignAssembly),
		globals__io_get_globals(Globals),

			
		    % If we are on the IL backend, add the dependency that the
		    % top level dll of a nested module hierachy depends on all
		    % of it sub-modules dlls, as they are referenced from
		    % inside the top level dll.

		{ SubModules = submodules(ModuleName, AllDeps) },
		( { Target = il, SubModules \= [] } ->
			module_name_to_file_name(ModuleName, ".dll", no,
					DllFileName),
			io__write_strings(DepStream, [DllFileName, " : "]),
			write_dll_dependencies_list(SubModules, "", DepStream),
			io__nl(DepStream)
		;
			[]
		),
		
		{ ContainsForeignCode = contains_foreign_code(LangSet),
			ForeignImports = ForeignImports0
		; ContainsForeignCode = unknown,
			get_item_list_foreign_code(Globals, Items,
				LangSet, ForeignImports, _)
		; ContainsForeignCode = no_foreign_code,
			set__init(LangSet),
			ForeignImports = ForeignImports0
		},

		%
		% Handle dependencies introduced by
		% `:- pragma foreign_import_module' declarations.
		%
		{ ForeignImportedModules =
		    list__map(
			(func(foreign_import_module(_, ForeignImportModule, _))
				= ForeignImportModule),
			ForeignImports) },
		( { ForeignImports = [] } ->
			[]
		;
			io__write_string(DepStream, "\n\n"),
			io__write_string(DepStream, ObjFileName),
			io__write_string(DepStream, " : "),
			write_dependencies_list(ForeignImportedModules, ".h",
				DepStream),
			io__write_string(DepStream, "\n\n")
		),

		(
			{ Target = il },
			{ not set__empty(LangSet) }
		->
			{ Langs = set__to_sorted_list(LangSet) },
			list__foldl(write_foreign_dependency_for_il(DepStream,
				ModuleName, AllDeps), Langs)
		;
			[]
		),

			% If we are signing the assembly, then we will
			% need the strong key to sign the il file with
			% so add a dependency that the il file requires
			% the strong name file `mercury.sn'.
			% Also add the variable ILASM_KEYFLAG-<module> which
			% is used to build the command line for ilasm.
		( { Target = il, SignAssembly = yes } ->
			{ prog_out__sym_name_to_string(ModuleName, ".",
					ModuleNameString) },
			module_name_to_file_name(ModuleName, ".il",
					no, IlFileName),
			
			io__write_strings(DepStream, [
				"ILASM_KEYFLAG-", ModuleNameString,
						" = /keyf=mercury.sn\n",
				IlFileName, " : mercury.sn\n"])
		;
			[]
		),

		module_name_to_file_name(ModuleName, ".int", no,
							IntFileName),
		module_name_to_file_name(ModuleName, ".int2", no,
							Int2FileName),
		module_name_to_file_name(ModuleName, ".int3", no,
							Int3FileName),
		module_name_to_file_name(ModuleName, ".opt", no,
							OptFileName),
		module_name_to_file_name(ModuleName, ".trans_opt", no,
							TransOptFileName),
		module_name_to_file_name(ModuleName, ".date3", no,
							Date3FileName),

		/*
		** We add some extra dependencies to the generated `.d' files, so
		** that local `.int', `.opt', etc. files shadow the installed
		** versions properly (e.g. for when you're trying to build a new
		** version of an installed library).  This saves the user from
		** having to add these explicitly if they have multiple libraries
		** installed in the same installation hierarchy which aren't
		** independent (e.g. one uses another).
		** These extra dependencies are necessary due to the way the
		** combination of search paths and pattern rules works in Make.
		**
		** Be very careful about changing the following rules.
		** The `@:' is a silent do-nothing command.
		** It is used to force GNU Make to recheck the timestamp
		** on the target file.  (It is a pity that GNU Make doesn't
		** have a way of handling these sorts of rules in a nicer
		** manner.)
		*/

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
		]),

		( { SourceFileName \= default_source_file(ModuleName) } ->
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
				"\t$(MCPI) $(ALL_MCPIFLAGS) $<\n",
				DateFileName, " : ", SourceFileName, "\n",
				"\t$(MCI) $(ALL_MCIFLAGS) $<\n",
				Date3FileName, " : ", SourceFileName, "\n",
				"\t$(MCSI) $(ALL_MCSIFLAGS) $<\n",
				OptDateFileName, " : ", SourceFileName, "\n",
				"\t$(MCOI) $(ALL_MCOIFLAGS) $<\n",
				TransOptDateFileName, " : ", SourceFileName,
					"\n",
				"\t$(MCTOI) $(ALL_MCTOIFLAGS) $<\n",
				CDateFileName, " : ", SourceFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"$< > ", ErrFileName, " 2>&1\n",
				"ifeq ($(TARGET_ASM),yes)\n",
				AsmDateFileName, " : ", SourceFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"--target-code-only $< > ", ErrFileName,
					" 2>&1\n",
				PicAsmDateFileName, " : ", SourceFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"--target-code-only --pic ",
					"\\\n",
				"\t\t--cflags ""$(GCCFLAGS_FOR_PIC)"" ",
					"$< > ", ErrFileName,
					" 2>&1\n",
				"endif # TARGET_ASM\n",
				ILDateFileName, " : ", SourceFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"--il-only $< > ", ErrFileName,
					" 2>&1\n",
				JavaDateFileName, " : ", SourceFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"--java-only $< > ", ErrFileName,
					" 2>&1\n",
				RLOFileName, " : ", SourceFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"--aditi-only $< > ", ErrFileName,
					" 2>&1\n"
			])
		;
			[]
		),

		io__close_output(DepStream),
		io__rename_file(TmpDependencyFileName, DependencyFileName,
			Result3),
		( { Result3 = error(_) } ->
			% On some systems, we need to remove the existing file
			% first, if any.  So try again that way.
			io__remove_file(DependencyFileName, Result4),
			( { Result4 = error(Error4) } ->
				maybe_write_string(Verbose, " failed.\n"),
				maybe_flush_output(Verbose),
				{ io__error_message(Error4, ErrorMsg) },
				{ string__append_list(["can't remove file `",
					DependencyFileName, "': ", ErrorMsg],
					Message) },
				report_error(Message)
			;
				io__rename_file(TmpDependencyFileName,
					DependencyFileName, Result5),
				( { Result5 = error(Error5) } ->
					maybe_write_string(Verbose,
						" failed.\n"),
					maybe_flush_output(Verbose),
					{ io__error_message(Error5,
						ErrorMsg) },
					{ string__append_list(
						["can't rename file `",
						TmpDependencyFileName,
						"' as `",
						DependencyFileName,
						"': ",
						ErrorMsg],
						Message) },
					report_error(Message)
				;
					maybe_write_string(Verbose, " done.\n")
				)
			)
		;
			maybe_write_string(Verbose, " done.\n")
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
	% 	<module>__cpp_code.dll : <module>.dll <imports>.dll
	%	<module>__cpp_code.cpp : <module>.il
	%
	% (the rule to generate .dll from .cpp is a pattern rule in
	% scripts/Mmake.rules).
	% 
:- pred write_foreign_dependency_for_il(io__output_stream::in,sym_name::in,
		list(module_name)::in, foreign_language::in,
		io__state::di, io__state::uo) is det.
write_foreign_dependency_for_il(DepStream, ModuleName, AllDeps, ForeignLang)
		-->
	( 
		{ ForeignModuleName = foreign_language_module_name(
			ModuleName, ForeignLang) },
		{ ForeignExt = foreign_language_file_extension(ForeignLang) }
	->
		module_name_to_file_name(ForeignModuleName, "", no,
			ForeignModuleNameString),
		module_name_to_file_name(ForeignModuleName, ForeignExt, no,
			ForeignFileName),
		module_name_to_file_name(ModuleName, ".il", no, IlFileName),
		module_name_to_file_name(ModuleName, ".dll", no, DllFileName),
		module_name_to_file_name(ForeignModuleName, ".dll", no,
			ForeignDllFileName),

		io__write_strings(DepStream, [
			ForeignDllFileName, " : ", DllFileName]),
			% XXX This change doesn't work correctly because
			% mmake can't find the dlls which don't reside
			% in the current directory.
		/*
		write_dll_dependencies_list(ModuleName, AllDeps, DepStream),
		*/
		io__nl(DepStream),

		io__write_strings(DepStream, [
			ForeignFileName, " : ", IlFileName, "\n\n"]),

		( { ForeignLang = csharp } ->
			% Store in the variable
			% CSHARP_ASSEMBLY_REFS-foreign_code_name
			% the command line argument to reference all the
			% dlls the foreign code module references.
			io__write_strings(DepStream, 
				["CSHARP_ASSEMBLY_REFS-", 
					ForeignModuleNameString, "="]),
			{
				ModuleName = unqualified(Str),
				mercury_std_library_module(Str)
			->
				Prefix = "/addmodule:"
			;
				Prefix = "/r:"
			},
			write_dll_dependencies_list(
				referenced_dlls(ModuleName, AllDeps),
				Prefix, DepStream),
			io__nl(DepStream)
		;
			[]
		)
	;
		% This foreign language doesn't generate an external file
		% so there are no dependencies to generate.
		[]
	).


maybe_read_dependency_file(ModuleName, MaybeTransOptDeps) -->
	globals__io_lookup_bool_option(transitive_optimization, TransOpt),
	( { TransOpt = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		module_name_to_file_name(ModuleName, ".d", no,
					DependencyFileName),
		maybe_write_string(Verbose, "% Reading auto-dependency file `"),
		maybe_write_string(Verbose, DependencyFileName),
		maybe_write_string(Verbose, "'..."),
		maybe_flush_output(Verbose),
		io__open_input(DependencyFileName, OpenResult),
		( { OpenResult = ok(Stream) },
			io__set_input_stream(Stream, OldStream),
			module_name_to_file_name(ModuleName, ".trans_opt_date",
				no, TransOptDateFileName0),
			{ string__to_char_list(TransOptDateFileName0, 
				TransOptDateFileName) },
			{ list__append(TransOptDateFileName, [' ', ':'],
				SearchPattern) },
			read_dependency_file_find_start(SearchPattern,
				FindResult),
			( { FindResult = yes } ->
				read_dependency_file_get_modules(TransOptDeps),
				{ MaybeTransOptDeps = yes(TransOptDeps) }
			;
				% error reading .d file
				{ MaybeTransOptDeps = no }
			),
			io__set_input_stream(OldStream, _),	
			io__close_input(Stream),
			maybe_write_string(Verbose, " done.\n")
		; { OpenResult = error(IOError) },
			maybe_write_string(Verbose, " failed.\n"),
			maybe_flush_output(Verbose),
			{ io__error_message(IOError, IOErrorMessage) },
			{ string__append_list(["error opening file `", 
				DependencyFileName, "' for input: ",
				IOErrorMessage], Message) },
			report_error(Message),
			{ MaybeTransOptDeps = no }
		)
	;
		{ MaybeTransOptDeps = no }
	).
			
	% Read lines from the dependency file (module.d) until one is found
	% which begins with SearchPattern.
:- pred read_dependency_file_find_start(list(char)::in, bool::out, 
	io__state::di, io__state::uo) is det.
read_dependency_file_find_start(SearchPattern, Success) -->
	io__read_line(Result),
	( { Result = ok(CharList) } ->
		( { list__append(SearchPattern, _, CharList) } ->
			% Have found the start
			{ Success = yes }
		;
			read_dependency_file_find_start(SearchPattern, Success)
		)
	; 
		{ Success = no }
	).
		
	% Read lines until one is found which does not contain whitespace
	% followed by a word which ends in .trans_opt.  Remove the
	% .trans_opt ending from all the words which are read in and return
	% the resulting list of modules..
:- pred read_dependency_file_get_modules(list(module_name)::out, io__state::di,
	io__state::uo) is det.
read_dependency_file_get_modules(TransOptDeps) -->
	io__read_line(Result),
	( 
		{ Result = ok(CharList0) },
		% Remove any whitespace from the beginning of the line,
		% then take all characters until another whitespace occurs.
		{ list__takewhile(char__is_whitespace, CharList0, _, 
			CharList1) },
		{ NotIsWhitespace = lambda([Char::in] is semidet, (
			\+ char__is_whitespace(Char)
		)) },
		{ list__takewhile(NotIsWhitespace, CharList1, CharList, _) },
		{ string__from_char_list(CharList, FileName0) },
		{ string__remove_suffix(FileName0, ".trans_opt", FileName) }
	->
		(
			{ string__append("Mercury/trans_opts/", BaseFileName,
				FileName) }
		->
			{ ModuleFileName = BaseFileName }
		;
			{ ModuleFileName = FileName }
		),
		{ file_name_to_module_name(ModuleFileName, Module) },
		read_dependency_file_get_modules(TransOptDeps0),
		{ TransOptDeps = [ Module | TransOptDeps0 ] }
	;
		{ TransOptDeps = [] }
	).

	% get_both_opt_deps(Deps, Directories, OptDeps, TransOptDeps).
	% For each dependency, search intermod_directories for a .m file.
	% If it exists, add it to both output lists. Otherwise, if a .opt
	% file exists, add it to the OptDeps list, and if a .trans_opt
	% file exists, add it to the TransOptDeps list.
	% If --use-opt-files is set, don't look for `.m' files, since
	% we are not building `.opt' files, only using those which
	% are available.
	% XXX This won't find nested sub-modules.
	% XXX Use `mmc --make' if that matters.
:- pred get_both_opt_deps(bool::in, list(module_name)::in, list(string)::in, 
	list(module_name)::out, list(module_name)::out, 
	io__state::di, io__state::uo) is det.
get_both_opt_deps(_, [], _, [], []) --> [].
get_both_opt_deps(BuildOptFiles, [Dep | Deps], IntermodDirs,
		OptDeps, TransOptDeps) -->
	get_both_opt_deps(BuildOptFiles, Deps, IntermodDirs,
		OptDeps0, TransOptDeps0),
	( { BuildOptFiles = yes } ->
		search_for_module_source(IntermodDirs, Dep, Result1),
		( { Result1 = ok(_) } ->
			{ OptDeps1 = [Dep | OptDeps0] },
			{ TransOptDeps1 = [Dep | TransOptDeps0] },
			io__seen,
			{ Found = yes }
		;
			{ OptDeps1 = OptDeps0 },
			{ TransOptDeps1 = TransOptDeps0 },
			{ Found = no }
		)
	;
		{ OptDeps1 = OptDeps0 },
		{ TransOptDeps1 = TransOptDeps0 },
		{ Found = no }
	),
	{ is_bool(Found) },
	( { Found = no } ->
		module_name_to_file_name(Dep, ".opt", no, OptName), 
		search_for_file(IntermodDirs, OptName, Result2),
		( { Result2 = ok(_) } ->
			{ OptDeps = [Dep | OptDeps1] },
			io__seen
		;
			{ OptDeps = OptDeps1 }
		),
		module_name_to_file_name(Dep, ".trans_opt", no, TransOptName), 
		search_for_file(IntermodDirs, TransOptName, Result3),
		( { Result3 = ok(_) } ->
			{ TransOptDeps = [Dep | TransOptDeps1] },
			io__seen
		;
			{ TransOptDeps = TransOptDeps1 }
		)
	;
		{ TransOptDeps = TransOptDeps1 },
		{ OptDeps = OptDeps1 }
	).

	% For each dependency, search intermod_directories for a .Suffix
	% file or a .m file, filtering out those for which the search fails.
	% If --use-opt-files is set, only look for `.opt' files,
	% not `.m' files.
	% XXX This won't find nested sub-modules.
	% XXX Use `mmc --make' if that matters.
:- pred get_opt_deps(bool::in, list(module_name)::in, list(string)::in,
	string::in, list(module_name)::out,
	io__state::di, io__state::uo) is det.
get_opt_deps(_, [], _, _, []) --> [].
get_opt_deps(BuildOptFiles, [Dep | Deps], IntermodDirs, Suffix, OptDeps) -->
	get_opt_deps(BuildOptFiles, Deps, IntermodDirs, Suffix, OptDeps0),
	( { BuildOptFiles = yes } ->
		search_for_module_source(IntermodDirs, Dep, Result1),
		( { Result1 = ok(_) } ->
			{ OptDeps1 = [Dep | OptDeps0] },
			{ Found = yes },
			io__seen
		;
			{ Found = no },
			{ OptDeps1 = OptDeps0 }
		)
	;
		{ Found = no },
		{ OptDeps1 = OptDeps0 }
	),
	{ is_bool(Found) },
	( { Found = no } ->
		module_name_to_file_name(Dep, Suffix, no, OptName),
		search_for_file(IntermodDirs, OptName, Result2),
		( { Result2 = ok(_) } ->
			{ OptDeps = [Dep | OptDeps1] },
			io__seen
		;
			{ OptDeps = OptDeps1 }
		)
	;
		{ OptDeps = OptDeps1 }
	).

:- pred is_bool(bool::in) is det.
is_bool(_).

%-----------------------------------------------------------------------------%

generate_module_dependencies(ModuleName) -->
	{ map__init(DepsMap0) },
	generate_dependencies(ModuleName, DepsMap0).

generate_file_dependencies(FileName) -->
	% read in the top-level file (to figure out its module name)
	read_mod_from_file(FileName, ".m", "Reading file", no, no,
		Items, Error, ModuleName, _),
	{ string__append(FileName, ".m", SourceFileName) },
	split_into_submodules(ModuleName, Items, SubModuleList),
	globals__io_get_globals(Globals),
	{ assoc_list__keys(SubModuleList, SubModuleNames) },
	{ list__map(
		init_dependencies(SourceFileName, ModuleName, SubModuleNames,
			Error, Globals),
		SubModuleList, ModuleImportsList) },
	{ map__init(DepsMap0) },
	{ list__foldl(insert_into_deps_map, ModuleImportsList,
		DepsMap0, DepsMap1) },
	generate_dependencies(ModuleName, DepsMap1).

:- pred generate_dependencies(module_name, deps_map, io__state, io__state).
:- mode generate_dependencies(in, in, di, uo) is det.
generate_dependencies(ModuleName, DepsMap0) -->
	% first, build up a map of the dependencies.
	generate_deps_map([ModuleName], DepsMap0, DepsMap),
	%
	% check whether we could read the main `.m' file
	%
	{ map__lookup(DepsMap, ModuleName, deps(_, ModuleImports)) },
	{ module_imports_get_error(ModuleImports, Error) },
	( { Error = fatal_module_errors } ->
		{ prog_out__sym_name_to_string(ModuleName, ModuleString) },
		{ string__append_list(["can't read source file for module `",
			ModuleString, "'."], Message) },
		report_error(Message)
	;
		{ module_imports_get_source_file_name(ModuleImports,
			SourceFileName) },
		generate_dependencies_write_dv_file(SourceFileName,
			ModuleName, DepsMap),
		generate_dependencies_write_dep_file(SourceFileName,
			ModuleName, DepsMap),

		%
		% compute the interface deps relation and
		% the implementation deps relation from the deps map
		%
		{ relation__init(IntDepsRel0) },
		{ relation__init(ImplDepsRel0) },
		{ map__values(DepsMap, DepsList) },
		{ deps_list_to_deps_rel(DepsList, DepsMap, 
			IntDepsRel0, IntDepsRel, ImplDepsRel0, ImplDepsRel) },

		%
		% compute the trans-opt deps ordering, by doing an
		% approximate topological sort of the implementation deps,
		% and then finding the subset of those for which of those
		% we have (or can make) trans-opt files.
		%
		{ relation__atsort(ImplDepsRel, ImplDepsOrdering0) },
		maybe_output_module_order(ModuleName, ImplDepsOrdering0),
		{ list__map(set__to_sorted_list, ImplDepsOrdering0, 
			ImplDepsOrdering) },
		{ list__condense(ImplDepsOrdering, TransOptDepsOrdering0) },
		globals__io_lookup_accumulating_option(intermod_directories, 
			IntermodDirs),
		get_opt_deps(yes, TransOptDepsOrdering0, IntermodDirs,
			".trans_opt", TransOptDepsOrdering),

		% { relation__to_assoc_list(ImplDepsRel, ImplDepsAL) },
		% print("ImplDepsAL:\n"),
		% write_list(ImplDepsAL, "\n", print), nl,

		%
		% compute the indirect dependencies: they are equal to the
		% composition of the implementation dependencies
		% with the transitive closure of the interface dependencies.
		%
		{ relation__tc(IntDepsRel, TransIntDepsRel) },
		{ relation__compose(ImplDepsRel, TransIntDepsRel,
			IndirectDepsRel) },

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
		{ relation__tc(ImplDepsRel, IndirectOptDepsRel) },

		/* 
		write_relations("Rel", IntDepsRel, TransIntDepsRel, ImplDepsRel,
				IndirectDepsRel, IndirectOptDepsRel),
		*/

		generate_dependencies_write_d_files(DepsList,
			IntDepsRel, ImplDepsRel, IndirectDepsRel,
			IndirectOptDepsRel, TransOptDepsOrdering, DepsMap)
	).

/*
	% Output the various relations into a file which can be
	% processed by the dot package to draw the relations.
:- pred write_relations(string::in, relation(sym_name)::in,
		relation(sym_name)::in, relation(sym_name)::in,
		relation(sym_name)::in, relation(sym_name)::in,
		io__state::di, io__state::uo) is det.

write_relations(FileName, IntDepsRel, TransIntDepsRel,
		ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel) -->
	io__open_output(FileName, Result),
	( { Result = ok(Stream) } ->
		write_relation(Stream, "IntDepsRel", IntDepsRel),
		write_relation(Stream, "TransIntDepsRel", TransIntDepsRel),
		write_relation(Stream, "ImplDepsRel", ImplDepsRel),
		write_relation(Stream, "IndirectDepsRel", IndirectDepsRel),
		write_relation(Stream, "IndirectOptDepsRel",
				IndirectOptDepsRel)
	;
		{ error("unable to open file: " ++ FileName) }
	).

:- pred write_relation(io__output_stream::in,
		string::in, relation(sym_name)::in,
		io__state::di, io__state::uo) is det.

write_relation(Stream, Name, Relation) -->
	io__write_string(Stream, "digraph " ++ Name ++ " {\n"),
	io__write_string(Stream, "label=\"" ++ Name ++ "\";\n"),
	io__write_string(Stream, "center=true;\n"),
	relation__traverse(Relation, write_node(Stream), write_edge(Stream)),
	io__write_string(Stream, "}\n").

:- pred write_node(io__output_stream::in, sym_name::in,
		io__state::di, io__state::uo) is det.

write_node(Stream, Node) -->
	{ sym_name_to_string(Node, "__", NodeStr) },
	io__write_string(Stream, NodeStr),
	io__write_string(Stream, ";\n").

:- pred write_edge(io__output_stream::in, sym_name::in, sym_name::in, 
		io__state::di, io__state::uo) is det.

write_edge(Stream, A, B) -->
	{ sym_name_to_string(A, "__", AStr) },
	{ sym_name_to_string(B, "__", BStr) },
	io__write_string(Stream, AStr),
	io__write_string(Stream, " -> "),
	io__write_string(Stream, BStr),
	io__write_string(Stream, ";\n").
*/

:- pred maybe_output_module_order(module_name::in, list(set(module_name))::in,
	io__state::di, io__state::uo) is det.
maybe_output_module_order(Module, DepsOrdering) -->
	globals__io_lookup_bool_option(generate_module_order, Order),
	globals__io_lookup_bool_option(verbose, Verbose),
	( { Order = yes } ->
		module_name_to_file_name(Module, ".order", yes, OrdFileName),
		maybe_write_string(Verbose, "% Creating module order file `"),
		maybe_write_string(Verbose, OrdFileName),
		maybe_write_string(Verbose, "'..."),
		io__open_output(OrdFileName, OrdResult),
		( { OrdResult = ok(OrdStream) },
			io__write_list(OrdStream, DepsOrdering, "\n\n", 
					write_module_scc(OrdStream)),
			io__close_output(OrdStream),
			maybe_write_string(Verbose, " done.\n")
		; { OrdResult = error(IOError) },
			maybe_write_string(Verbose, " failed.\n"),
			maybe_flush_output(Verbose),
			{ io__error_message(IOError, IOErrorMessage) },
			{ string__append_list(["error opening file `", 
	    			OrdFileName, "' for output: ", IOErrorMessage],
				OrdMessage) },
			report_error(OrdMessage)
		)
	;
		[]
	).

:- pred write_module_scc(io__output_stream::in, set(module_name)::in,
		io__state::di, io__state::uo) is det.
write_module_scc(Stream, SCC0) -->
	{ set__to_sorted_list(SCC0, SCC) },
	io__write_list(Stream, SCC, "\n", prog_out__write_sym_name).


% generate_dependencies_write_d_files(Modules, IntDepsRel, ImplDepsRel,
%	IndirectDepsRel, IndirectOptDepsRel, TransOptOrder,
%	DepsMap, IO0, IO):
%		This predicate writes out the .d files for all the modules
%		in the Modules list.  
%		IntDepsRel gives the interface dependency relation.
%		ImplDepsRel gives the implementation dependency relation
%		IndirectDepsRel gives the indirect dependency relation
%		(this includes dependencies on `*.int2' files).
%		IndirectOptDepsRel gives the indirect optimization
%		dependencies (this includes dependencies via `.opt'
%		and `.trans_opt' files).
%		These are all computed from the DepsMap.
%		TransOptOrder gives the ordering that is used to determine
%		which other modules the .trans_opt files may depend on.
:- pred generate_dependencies_write_d_files(list(deps)::in, 
	deps_rel::in, deps_rel::in, deps_rel::in, deps_rel::in,
	list(module_name)::in, deps_map::in,
	io__state::di, io__state::uo) is det.
generate_dependencies_write_d_files([], _, _, _, _, _, _) --> [].
generate_dependencies_write_d_files([Dep | Deps],
		IntDepsRel, ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel,
		TransOptOrder, DepsMap) --> 
	{ Dep = deps(_, Module0) },

	%
	% Look up the interface/implementation/indirect dependencies
	% for this module from the respective dependency relations,
	% and save them in the module_imports structure.
	%
	{ module_imports_get_module_name(Module0, ModuleName) },
	{ get_dependencies_from_relation(IntDepsRel, ModuleName, IntDeps) },
	{ get_dependencies_from_relation(ImplDepsRel, ModuleName, ImplDeps) },
	{ get_dependencies_from_relation(IndirectDepsRel, ModuleName,
			IndirectDeps) },
	{ get_dependencies_from_relation(IndirectOptDepsRel, ModuleName,
			IndirectOptDeps) },
	{ module_imports_set_int_deps(Module0, IntDeps, Module1) },
	{ module_imports_set_impl_deps(Module1, ImplDeps, Module2) },
	{ module_imports_set_indirect_deps(Module2, IndirectDeps, Module) },

	%
	% Compute the trans-opt dependencies for this module.
	% To avoid the possibility of cycles, each module is
	% only allowed to depend on modules that occur later
	% than it in the TransOptOrder.
	%
	{ FindModule = lambda([OtherModule::in] is semidet, (
		ModuleName \= OtherModule )) },
	{ list__takewhile(FindModule, TransOptOrder, _, TransOptDeps0) },
	( { TransOptDeps0 = [ _ | TransOptDeps1 ] } ->
		% The module was found in the list
		{ TransOptDeps = TransOptDeps1 }
	;
		{ TransOptDeps = [] }
	),

	%
	% Note that even if a fatal error occured for one of the files that
	% the current Module depends on, a .d file is still produced, even
	% though it probably contains incorrect information.
	{ module_imports_get_error(Module, Error) },
	( { Error \= fatal_module_errors } ->
		write_dependency_file(Module,
			set__list_to_set(IndirectOptDeps), yes(TransOptDeps))
	;
		[]
	),
	generate_dependencies_write_d_files(Deps,
		IntDepsRel, ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel,
		TransOptOrder, DepsMap).

:- pred get_dependencies_from_relation(deps_rel, module_name,
		list(module_name)).
:- mode get_dependencies_from_relation(in, in, out) is det.

get_dependencies_from_relation(DepsRel0, ModuleName, Deps) :-
	relation__add_element(DepsRel0, ModuleName, ModuleKey, DepsRel),
	relation__lookup_from(DepsRel, ModuleKey, DepsKeysSet),
	set__to_sorted_list(DepsKeysSet, DepsKeys),
	list__map(relation__lookup_key(DepsRel), DepsKeys, Deps).

% This is the data structure we use to record the dependencies.
% We keep a map from module name to information about the module.

:- type deps_map == map(module_name, deps).
:- type deps
	---> deps(
		bool,			% have we processed this module yet?
		module_imports
	).

	% (Module1 deps_rel Module2) means Module1 is imported by Module2.
:- type deps_rel == relation(module_name).

:- pred generate_deps_map(list(module_name), deps_map, deps_map,
			io__state, io__state).
:- mode generate_deps_map(in, in, out, di, uo) is det.

generate_deps_map([], DepsMap, DepsMap) --> [].
generate_deps_map([Module | Modules], DepsMap0, DepsMap) -->
		% Look up the module's dependencies, and determine whether
		% it has been processed yet.
	lookup_dependencies(Module, DepsMap0, no, Done, ModuleImports,
			DepsMap1),
		% If the module hadn't been processed yet, then add its
		% imports, parents, and public children to the list of
		% dependencies we need to generate, and mark it as
		% having been processed.
	( { Done = no } ->
		{ map__set(DepsMap1, Module, deps(yes, ModuleImports),
			DepsMap2) },
		{ ForeignImportedModules =
		    list__map(
			(func(foreign_import_module(_, ImportedModule, _))
				= ImportedModule),
			ModuleImports ^ foreign_import_module_info) },
		{ list__condense(
			[ModuleImports ^ parent_deps,
			ModuleImports ^ int_deps,
			ModuleImports ^ impl_deps,
			ModuleImports ^ public_children, % a.k.a. incl_deps
			ForeignImportedModules,
			Modules],
			Modules1) }
	;
		{ DepsMap2 = DepsMap1 },
		{ Modules1 = Modules }
	),
		% Recursively process the remaining modules
	generate_deps_map(Modules1, DepsMap2, DepsMap).


	% Construct a pair of dependency relations (the interface dependencies
	% and the implementation dependencies) for all the modules in the
	% program.
:- pred deps_list_to_deps_rel(list(deps), deps_map,
		deps_rel, deps_rel, deps_rel, deps_rel).
:- mode deps_list_to_deps_rel(in, in, in, out, in, out) is det.

deps_list_to_deps_rel([], _, IntRel, IntRel, ImplRel, ImplRel).
deps_list_to_deps_rel([Deps | DepsList], DepsMap,
		IntRel0, IntRel, ImplRel0, ImplRel) :-
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
		relation__add_element(IntRel0, ModuleName, IntModuleKey,
			IntRel1),
		add_int_deps(IntModuleKey, ModuleImports, IntRel1, IntRel2),
		list__foldl(add_parent_impl_deps(DepsMap, IntModuleKey),
				ParentDeps, IntRel2, IntRel3),

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
		relation__add_element(ImplRel0, ModuleName, ImplModuleKey,
			ImplRel1),
		add_impl_deps(ImplModuleKey, ModuleImports, ImplRel1, ImplRel2),
		list__foldl(add_parent_impl_deps(DepsMap, ImplModuleKey),
				ParentDeps, ImplRel2, ImplRel3)
	;
		IntRel3 = IntRel0,
		ImplRel3 = ImplRel0
	),
	deps_list_to_deps_rel(DepsList, DepsMap,
		IntRel3, IntRel, ImplRel3, ImplRel).

% add interface dependencies to the interface deps relation
%
:- pred add_int_deps(relation_key, module_imports, deps_rel, deps_rel).
:- mode add_int_deps(in, in, in, out) is det.

add_int_deps(ModuleKey, ModuleImports, Rel0, Rel) :-
	AddDep = add_dep(ModuleKey),
	list__foldl(AddDep, ModuleImports ^ parent_deps, Rel0, Rel1),
	list__foldl(AddDep, ModuleImports ^ int_deps, Rel1, Rel).

% add direct implementation dependencies for a module to the
% impl. deps relation
%
:- pred add_impl_deps(relation_key, module_imports, deps_rel, deps_rel).
:- mode add_impl_deps(in, in, in, out) is det.

add_impl_deps(ModuleKey, ModuleImports, Rel0, Rel) :-
	% the implementation dependencies are a superset of the
	% interface dependencies, so first we add the interface deps
	add_int_deps(ModuleKey, ModuleImports, Rel0, Rel1),
	% then we add the impl deps
	module_imports_get_impl_deps(ModuleImports, ImplDeps),
	list__foldl(add_dep(ModuleKey), ImplDeps, Rel1, Rel).

% add parent implementation dependencies for the given Parent module
% to the impl. deps relation values for the given ModuleKey.
%
:- pred add_parent_impl_deps(deps_map, relation_key, module_name,
			deps_rel, deps_rel).
:- mode add_parent_impl_deps(in, in, in, in, out) is det.

add_parent_impl_deps(DepsMap, ModuleKey, Parent, Rel0, Rel) :-
	map__lookup(DepsMap, Parent, deps(_, ParentModuleImports)),
	add_impl_deps(ModuleKey, ParentModuleImports, Rel0, Rel).

% add a single dependency to a relation
%
:- pred add_dep(relation_key, T, relation(T), relation(T)).
:- mode add_dep(in, in, in, out) is det.

add_dep(ModuleRelKey, Dep, Relation0, Relation) :-
	relation__add_element(Relation0, Dep, DepRelKey, Relation1),
	relation__add(Relation1, ModuleRelKey, DepRelKey, Relation).

% check if a module is a top-level module, a nested sub-module,
% or a separate sub-module.
%
:- type submodule_kind
	--->	toplevel
	;	nested_submodule
	;	separate_submodule.

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
		deps_map::in, io__state::di, io__state::uo) is det.
generate_dependencies_write_dv_file(SourceFileName, ModuleName, DepsMap) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	module_name_to_file_name(ModuleName, ".dv", yes, DvFileName),
	maybe_write_string(Verbose, "% Creating auto-dependency file `"),
	maybe_write_string(Verbose, DvFileName),
	maybe_write_string(Verbose, "'...\n"),
	io__open_output(DvFileName, DvResult),
	( { DvResult = ok(DvStream) },
		generate_dv_file(SourceFileName, ModuleName, DepsMap,
			DvStream),
		io__close_output(DvStream),
		maybe_write_string(Verbose, "% done.\n")
	; { DvResult = error(IOError) },
		maybe_write_string(Verbose, " failed.\n"),
		maybe_flush_output(Verbose),
		{ io__error_message(IOError, IOErrorMessage) },
		{ string__append_list(["error opening file `", DvFileName,
			"' for output: ", IOErrorMessage], DvMessage) },
		report_error(DvMessage)
	).

	% Write out the `.dep' file, using the information collected in the
	% deps_map data structure.
:- pred generate_dependencies_write_dep_file(file_name::in, module_name::in,
		deps_map::in, io__state::di, io__state::uo) is det.
generate_dependencies_write_dep_file(SourceFileName, ModuleName, DepsMap) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	module_name_to_file_name(ModuleName, ".dep", yes, DepFileName),
	maybe_write_string(Verbose, "% Creating auto-dependency file `"),
	maybe_write_string(Verbose, DepFileName),
	maybe_write_string(Verbose, "'...\n"),
	io__open_output(DepFileName, DepResult),
	( { DepResult = ok(DepStream) },
		generate_dep_file(SourceFileName, ModuleName, DepsMap,
			DepStream),
		io__close_output(DepStream),
		maybe_write_string(Verbose, "% done.\n")
	; { DepResult = error(IOError) },
		maybe_write_string(Verbose, " failed.\n"),
		maybe_flush_output(Verbose),
		{ io__error_message(IOError, IOErrorMessage) },
		{ string__append_list(["error opening file `", DepFileName,
			"' for output: ", IOErrorMessage], DepMessage) },
		report_error(DepMessage)
	).


:- pred generate_dv_file(file_name, module_name, deps_map, io__output_stream,
			io__state, io__state).
:- mode generate_dv_file(in, in, in, in, di, uo) is det.

generate_dv_file(SourceFileName, ModuleName, DepsMap, DepStream) -->
	io__write_string(DepStream,
		"# Automatically generated dependency variables for module `"),
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameString) },
	io__write_string(DepStream, ModuleNameString),
	io__write_string(DepStream, "'\n"),
	io__write_string(DepStream,
		"# generated from source file `"),
	io__write_string(DepStream, SourceFileName),
	io__write_string(DepStream, "'\n"),

	{ library__version(Version) },
	io__write_string(DepStream,
		"# Generated by the Mercury compiler, version "),
	io__write_string(DepStream, Version),
	io__write_string(DepStream, ".\n\n"),

	{ map__keys(DepsMap, Modules0) },
	{ select_ok_modules(Modules0, DepsMap, Modules) },

	{ module_name_to_make_var_name(ModuleName, MakeVarName) },
	{ list__map(get_source_file(DepsMap), Modules, SourceFiles0) },
	{ list__sort_and_remove_dups(SourceFiles0, SourceFiles) },

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".ms ="),
	write_file_dependencies_list(SourceFiles, ".m", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".errs ="),
	write_file_dependencies_list(SourceFiles, ".err", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".mods ="),
	write_dependencies_list(Modules, "", DepStream),
	io__write_string(DepStream, "\n"),

	globals__io_get_target(Target),
	( { Target = il } ->
		{ ForeignModulesAndExts = foreign_modules(Modules, DepsMap) }
	;
		{ ForeignModulesAndExts = [] }
	),
	{ ForeignModules = assoc_list__keys(ForeignModulesAndExts) },
	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".foreign ="),
	write_dependencies_list(ForeignModules, "", DepStream),
	io__write_string(DepStream, "\n\n"),

	globals__io_lookup_bool_option(assume_gmake, Gmake),
	( { Gmake = yes } ->
		{ string__append(MakeVarName, ".mods", ModsVarName) },
		{ Basis = yes(ModsVarName - "") },

		{ string__append(MakeVarName, ".foreign", ForeignVarName) },
		{ ForeignBasis = yes(ForeignVarName - "") }
	;
		{ Basis = no },
		{ ForeignBasis = no }
	),

	{ get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs) },


	{ MakeFileName = (pred(M - E::in, F::out, di, uo) is det -->
		module_name_to_file_name(M, E, yes, F0),
		{ F = "$(os_subdir)" ++ F0 }
	) },

	list__map_foldl(MakeFileName, ForeignModulesAndExts, ForeignFileNames),

		% .foreign_cs are the source files which have had
		% foreign code placed in them.
	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".foreign_cs = "),
	write_file_dependencies_list(ForeignFileNames, "", DepStream),
	io__write_string(DepStream, "\n"),

		% The dlls which contain the foreign_code.
	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".foreign_dlls = "),
	write_compact_dependencies_list(ForeignModules, "$(dlls_subdir)",
					".dll", ForeignBasis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".init_cs = "),
	write_compact_dependencies_list(Modules, "$(cs_subdir)", ".c",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".cs = $("),
	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".init_cs) "),
	write_extra_link_dependencies_list(ExtraLinkObjs, ".c", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".dlls = "),
	write_compact_dependencies_list(Modules, "$(dlls_subdir)", ".dll",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_ss = "),
	write_compact_dependencies_list(Modules, "$(ss_subdir)", ".s",
		Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_pic_ss = "),
	write_compact_dependencies_list(Modules, "$(pic_ss_subdir)", ".pic_s",
		Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_s_dates = "),
	write_compact_dependencies_list(Modules, "$(s_dates_subdir)",
					".s_date", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_pic_s_dates = "),
	write_compact_dependencies_list(Modules, "$(pic_s_dates_subdir)",
					".pic_s_date", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_os = "),
	write_compact_dependencies_list(Modules, "$(os_subdir)", ".$O",
		Basis, DepStream),
	write_extra_link_dependencies_list(ExtraLinkObjs, ".$O", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_pic_os = "),
	write_compact_dependencies_list(Modules, "$(os_subdir)",
		".$(EXT_FOR_PIC_OBJECTS)", Basis, DepStream),
	write_extra_link_dependencies_list(ExtraLinkObjs,
		".$(EXT_FOR_PIC_OBJECTS)", DepStream),
	io__write_string(DepStream, "\n"),

	{ IsNested = (pred(Mod::in) is semidet :-
		get_submodule_kind(Mod, DepsMap) = nested_submodule) },
	(
		% For --target asm, we only generate separate object files
		% for top-level modules and separate sub-modules, not for
		% nested sub-modules.
		{ Target = asm },
		{ list__filter(IsNested, Modules,
			NestedModules, MainModules) },
		{ NestedModules \= [] }
	->
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".ss = "),
		write_dependencies_list(MainModules, ".s", DepStream),
		io__write_string(DepStream, "\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".pic_ss = "),
		write_dependencies_list(MainModules, ".pic_s", DepStream),
		io__write_string(DepStream, "\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".s_dates = "),
		write_dependencies_list(MainModules, ".s_date", DepStream),
		io__write_string(DepStream, "\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".pic_s_dates = "),
		write_dependencies_list(MainModules, ".pic_s_date", DepStream),
		io__write_string(DepStream, "\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".os = "),
		write_dependencies_list(MainModules, ".$O", DepStream),
		write_extra_link_dependencies_list(ExtraLinkObjs, ".$O",
			DepStream),
		io__write_string(DepStream, "\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".pic_os = "),
		write_dependencies_list(MainModules, ".$(EXT_FOR_PIC_OBJECTS)",
			DepStream),
		write_extra_link_dependencies_list(ExtraLinkObjs,
			".$(EXT_FOR_PIC_OBJECTS)", DepStream),
		io__write_string(DepStream, "\n")
	;
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".ss = $("),
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".all_ss)\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".pic_ss = $("),
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".all_pic_ss)\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".s_dates = $("),
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".all_s_dates)\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".pic_s_dates = $("),
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".all_pic_s_dates)\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".os = $("),
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".all_os)\n"),

		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".pic_os = $("),
		io__write_string(DepStream, MakeVarName),
		io__write_string(DepStream, ".all_pic_os)\n")
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
	]),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".rlos = "),
	write_compact_dependencies_list(Modules, "$(rlos_subdir)", ".rlo",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".useds = "),
	write_compact_dependencies_list(Modules, "$(useds_subdir)", ".used",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".ils = "),
	write_compact_dependencies_list(Modules, "$(ils_subdir)", ".il",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".javas = "),
	write_compact_dependencies_list(Modules, "$(javas_subdir)", ".java",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".classes = "),
	write_compact_dependencies_list(Modules, "$(classes_subdir)", ".class",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".dirs = "),
	write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".num_splits = "),
	write_compact_dependencies_list(Modules, "$(num_splits_subdir)",
					".num_splits", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".dir_os = "),
	write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir/*.$O",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".dates = "),
	write_compact_dependencies_list(Modules, "$(dates_subdir)", ".date",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".date0s = "),
	write_compact_dependencies_list(Modules, "$(date0s_subdir)", ".date0",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".date3s = "),
	write_compact_dependencies_list(Modules, "$(date3s_subdir)", ".date3",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".optdates = "),
	write_compact_dependencies_list(Modules, "$(optdates_subdir)",
					".optdate", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".trans_opt_dates = "),
	write_compact_dependencies_list(Modules, "$(trans_opt_dates_subdir)",
					".trans_opt_date", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".c_dates = "),
	write_compact_dependencies_list(Modules, "$(c_dates_subdir)",
					".c_date", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".il_dates = "),
	write_compact_dependencies_list(Modules, "$(il_dates_subdir)",
					".il_date", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".java_dates = "),
	write_compact_dependencies_list(Modules, "$(java_dates_subdir)",
					".java_date", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".ds = "),
	write_compact_dependencies_list(Modules, "$(ds_subdir)", ".d",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".module_deps = "),
	write_compact_dependencies_list(Modules, "$(module_deps_subdir)",
			module_dep_file_extension, Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".hs = "),
	globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
	( { HighLevelCode = yes } ->
		( { Target = asm } ->
			% For the `--target asm' back-end, we only
			% generate `.h' files for modules that
			% contain C code
			write_dependencies_list(
				modules_that_need_headers(Modules, DepsMap),
				".h", DepStream)
		; { Target = c } ->
			% For the `--target c' MLDS back-end, we
			% generate `.h' files for every module
			write_compact_dependencies_list(Modules, "", ".h",
					Basis, DepStream)
		;
			% For the IL and Java targets, currently we don't
			% generate `.h' files at all; although perhaps
			% we should...
			[]
		)
	;
		% For the LLDS back-end, we only generate `.h' files
		% for modules containing `:- pragma export' declarations.
		{ LLDSHeaderModules =
		    list__filter(
			(pred(Module::in) is semidet :-
			    map__lookup(DepsMap, Module,
					deps(_, ModuleImports)),
			    contains_foreign_export =
			    	ModuleImports ^ contains_foreign_export 
		     ), Modules) },
		write_dependencies_list(LLDSHeaderModules, ".h", DepStream)
	),
	io__write_string(DepStream, "\n"),

	% The `<module>.all_hs' variable is like `<module>.hs' except
	% that it contains header files for all the modules, regardless
	% of the grade or --target option.  It is used by the rule for
	% `mmake realclean', which should remove anything that could have
	% been automatically generated, even if the grade or --target option
	% has changed.
	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".all_hs = "),
	write_compact_dependencies_list(Modules, "", ".h", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".ints = "),
	write_compact_dependencies_list(Modules, "$(ints_subdir)", ".int",
					Basis, DepStream),
	write_compact_dependencies_separator(Basis, DepStream),
	write_compact_dependencies_list(Modules, "$(int2s_subdir)", ".int2",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	% The .int0s list should really only include modules that
	% contain sub-modules.  But currently it's only used for
	% the `mmake clean' rule, so it doesn't matter.
	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".int0s = "),
	write_compact_dependencies_list(Modules, "$(int0s_subdir)", ".int0",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".int3s = "),
	write_compact_dependencies_list(Modules, "$(int3s_subdir)", ".int3",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".opts = "),
	write_compact_dependencies_list(Modules, "$(opts_subdir)", ".opt",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".trans_opts = "),
	write_compact_dependencies_list(Modules, "$(trans_opts_subdir)",
					".trans_opt", Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".schemas = "),
	write_compact_dependencies_list(Modules, "", ".base_schema",
					Basis, DepStream),
	io__write_string(DepStream, " "),
	write_compact_dependencies_list(Modules, "", ".derived_schema",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".profs = "),
	write_compact_dependencies_list(Modules, "", ".prof",
					Basis, DepStream),
	io__write_string(DepStream, "\n\n").


:- pred generate_dep_file(file_name, module_name, deps_map, io__output_stream,
			io__state, io__state).
:- mode generate_dep_file(in, in, in, in, di, uo) is det.

generate_dep_file(SourceFileName, ModuleName, DepsMap, DepStream) -->
	io__write_string(DepStream,
		"# Automatically generated dependencies for module `"),
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameString) },
	io__write_string(DepStream, ModuleNameString),
	io__write_string(DepStream, "'\n"),
	io__write_string(DepStream,
		"# generated from source file `"),
	io__write_string(DepStream, SourceFileName),
	io__write_string(DepStream, "'\n"),

	{ library__version(Version) },
	io__write_string(DepStream,
		"# Generated by the Mercury compiler, version "),
	io__write_string(DepStream, Version),
	io__write_string(DepStream, ".\n\n"),

	{ map__keys(DepsMap, Modules0) },
	{ select_ok_modules(Modules0, DepsMap, Modules) },

	{ module_name_to_make_var_name(ModuleName, MakeVarName) },

	module_name_to_file_name(ModuleName, ".init", yes, InitFileName),
	module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName),
	module_name_to_file_name(ModuleName, "_init.s", no, InitAsmFileName),
	module_name_to_file_name(ModuleName, "_init.$O", yes, InitObjFileName),
	module_name_to_file_name(ModuleName, "_init.pic_o", yes,
							InitPicObjFileName),

	% Note we have to do some ``interesting'' hacks to get
	% `$(ALL_MLLIBS_DEP)' to work in the dependency list
	% (and not complain about undefined variables).
	% These hacks rely on features of GNU Make, so should not be used
	% if we cannot assume we are using GNU Make.
	globals__io_lookup_bool_option(assume_gmake, Gmake),
	{ Gmake = yes ->
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
		All_MLLibsDepString = "$(ALL_MLLIBS_DEP)",
		All_MLObjsString = "$(ALL_MLOBJS)",
		All_MLPicObjsString = "$(ALL_MLPICOBJS)"
	},

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

	module_name_to_file_name(ModuleName, "", no, ExeFileName),

	{ If = ["ifeq ($(findstring il,$(GRADE)),il)\n"] },
	{ ILMainRule = [ExeFileName, " : ", ExeFileName, ".exe\n",
			ExeFileName, ".exe : ", "$(", MakeVarName, ".dlls) ",
			"$(", MakeVarName, ".foreign_dlls)\n"] },
	{ Else = ["else\n"] },
	{ MainRule =
		[ExeFileName, " : $(", MakeVarName, ".cs_or_ss) ",
			"$(", MakeVarName, ".os) ",
			InitObjFileName, " ", All_MLObjsString, " ",
			All_MLLibsDepString, "\n",
		"\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) ",
			"-o ", ExeFileName, " ", InitObjFileName, " \\\n",
		"\t	$(", MakeVarName, ".os) ", All_MLObjsString,
			" $(ALL_MLLIBS)\n"]
	},
	{ EndIf = ["endif\n"] },

	globals__io_get_target(Target),
	{ Gmake = yes,
		Rules = If ++ ILMainRule ++ Else ++ MainRule ++ EndIf
	; Gmake = no,
		( Target = il ->
			Rules = ILMainRule
		;
			Rules = MainRule
		)
	},
	io__write_strings(DepStream, Rules),

	module_name_to_file_name(ModuleName, ".split", yes,
				SplitExeFileName),
	module_name_to_file_name(ModuleName, ".split.$A",
			yes, SplitLibFileName),
	io__write_strings(DepStream, [
		SplitExeFileName, " : ", SplitLibFileName, " ",
			InitObjFileName, " ", All_MLObjsString, " ",
			All_MLLibsDepString, "\n",
		"\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) ",
			"-o ", SplitExeFileName, " ", InitObjFileName, " \\\n",
		"\t	", SplitLibFileName, " ", All_MLObjsString,
			" $(ALL_MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		SplitLibFileName, " : $(", MakeVarName, ".dir_os) ",
					All_MLObjsString, "\n",
		"\trm -f ", SplitLibFileName, "\n",
		"\t$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT) ",
		SplitLibFileName, " ", All_MLObjsString, "\n",
		"\tfind $(", MakeVarName, ".dirs) -name ""*.$O"" -print | \\\n",
		"\t	xargs $(AR) q ", SplitLibFileName, "\n",
		"\t$(RANLIB) $(ALL_RANLIBFLAGS) ", SplitLibFileName, "\n\n"
	]),

	globals__io_lookup_bool_option(intermodule_optimization, Intermod),
	{ Intermod = yes ->
		string__append_list(["$(", MakeVarName, ".opts) "],
				MaybeOptsVar)
	;
		MaybeOptsVar = ""
	},
	globals__io_lookup_bool_option(transitive_optimization, TransOpt),
	{ TransOpt = yes ->
		string__append_list(["$(", MakeVarName, ".trans_opts) "],
				MaybeTransOptsVar)
	;
		MaybeTransOptsVar = ""
	},
	globals__io_lookup_bool_option(generate_mmc_make_module_dependencies,
		MmcMakeDeps),
	{ MmcMakeDeps = yes ->
		string__append_list(["$(", MakeVarName, ".module_deps) "],
				MaybeModuleDepsVar)
	;
		MaybeModuleDepsVar = ""
	},

	module_name_to_lib_file_name("lib", ModuleName, "", no, LibTargetName),
	module_name_to_lib_file_name("lib", ModuleName, ".$A",
			yes, LibFileName),
	module_name_to_lib_file_name("lib", ModuleName, ".so", yes,
							SharedLibFileName),
	module_name_to_lib_file_name("lib", ModuleName,
		".$(EXT_FOR_SHARED_LIB)", no, MaybeSharedLibFileName),

	{ ILLibRule = [
		LibTargetName, " : ", "$(", MakeVarName, ".dlls) ",
			"$(", MakeVarName, ".foreign_dlls)\n"
	] },
	{ LibRule = [
		LibTargetName, " : ", LibFileName, " ",
		MaybeSharedLibFileName, " \\\n",
		"\t\t$(", MakeVarName, ".ints) ",
		"$(", MakeVarName, ".int3s) ",
		MaybeOptsVar, MaybeTransOptsVar,
		InitFileName, "\n\n"
	] },
	{ Gmake = yes,
		LibRules = If ++ ILLibRule ++ Else ++ LibRule ++ EndIf
	; Gmake = no,
		( Target = il ->
			LibRules = ILLibRule
		;
			LibRules = LibRule
		)
	},
	io__write_strings(DepStream, [
		".PHONY : ", LibTargetName, "\n" |
		LibRules
	]),

	io__write_strings(DepStream, [
		SharedLibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
			"$(", MakeVarName, ".pic_os) ",
			All_MLPicObjsString, " ", All_MLLibsDepString, "\n",
		"\t$(ML) --make-shared-lib $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) ",
			"-- $(ALL_LD_LIBFLAGS) -o ", SharedLibFileName, " \\\n",
		"\t\t$(", MakeVarName, ".pic_os) ", All_MLPicObjsString,
			" $(ALL_MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		LibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
			"$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
		"\trm -f ", LibFileName, "\n",
		"\t$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)", LibFileName, " ",
			"$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
		"\t$(RANLIB) $(ALL_RANLIBFLAGS) ", LibFileName, "\n\n"
	]),

	module_name_to_file_name(ModuleName, ".dep", no, DepFileName),
	module_name_to_file_name(ModuleName, ".dv", no, DvFileName),
	io__write_strings(DepStream, [
		InitFileName, " : ", DepFileName, "\n",
		"\techo > ", InitFileName, "\n"
	]),
	list__foldl(append_to_init_list(DepStream, InitFileName), Modules),
	io__write_string(DepStream, "\n"),

	% The `force-module_init' dependency forces the commands for
	% the `module_init.c' rule to be run every time the rule
	% is considered.
	{ prog_out__sym_name_to_string(ModuleName, ".", ModuleFileName) },
	{ ForceC2InitTarget = "force-" ++ ModuleFileName ++ "_init" },
	{ TmpInitCFileName = InitCFileName ++ ".tmp" },
	io__write_strings(DepStream, [
		ForceC2InitTarget, " :\n\n",
		InitCFileName, " : ", ForceC2InitTarget, "\n",
		"\t@$(C2INIT) $(ALL_GRADEFLAGS) $(ALL_C2INITFLAGS) ",
			"--init-c-file ", TmpInitCFileName,
			" $(", MakeVarName, ".init_cs) $(ALL_C2INITARGS)\n",
		"\t@mercury_update_interface ", InitCFileName, "\n\n"
	]),

	module_name_to_lib_file_name("lib", ModuleName, ".install_ints", no,
				LibInstallIntsTargetName),
	{ Intermod = yes -> OptStr = " opt" ; OptStr = "" },
	{ TransOpt = yes -> TransOptStr = " trans_opt" ; TransOptStr = "" },
	{ MmcMakeDeps = yes -> DepStr = " module_dep" ; DepStr = "" },
	{ InstallIntsRuleBody = string__append_list([
"		for file in $$files; do \\
			target=""$(INSTALL_INT_DIR)/`basename $$file`""; \\
			if cmp -s ""$$file"" ""$$target""; then \\
				echo \"$$target unchanged\"; \\
			else \\
				echo \"installing $$target\"; \\
				$(INSTALL) ""$$file"" ""$$target""; \\
			fi; \\
		done
		# The following is needed to support the `--use-subdirs' option
		# We try using `ln -s', but if that fails, then we just use
		# `$(INSTALL)'.
		for ext in int int2 int3", OptStr, TransOptStr, DepStr, "; do \\
			dir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\
			rm -f ""$$dir""; \\
			ln -s .. ""$$dir"" || { \\
				{ [ -d ""$$dir"" ] || \\
					$(INSTALL_MKDIR) ""$$dir""; } && \\
				$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext \\
					""$$dir""; \\
			} || exit 1; \\
		done\n\n"]) },

	io__write_strings(DepStream, [
		".PHONY : ", LibInstallIntsTargetName, "\n",
		LibInstallIntsTargetName, " : $(", MakeVarName, ".ints) $(",
			MakeVarName, ".int3s) ", MaybeOptsVar,
			MaybeTransOptsVar, MaybeModuleDepsVar,
			"install_lib_dirs\n",
		"\tfiles=""$(", MakeVarName, ".ints) $(", MakeVarName,
			".int3s) ", MaybeOptsVar, MaybeTransOptsVar,
			MaybeModuleDepsVar, """; \\\n",
		InstallIntsRuleBody
	]),

	module_name_to_lib_file_name("lib", ModuleName, ".install_hdrs", no,
				LibInstallHdrsTargetName),
	globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
	( { HighLevelCode = yes, ( Target = c ; Target = asm ) } ->
		%
		% XXX  Note that we install the header files in two places:
		% in the `inc' directory, so that the C compiler will find
		% them, and also in the `ints' directory, so that Mmake
		% will find them.  That's not ideal, but it works.
		% (A better fix would be to change the VPATH setting
		% in scripts/Mmake.vars.in so that Mmake also searches
		% the `inc' directory, but doing that properly is non-trivial.)
		%
		io__write_strings(DepStream, [
			".PHONY : ", LibInstallHdrsTargetName, "\n",
			LibInstallHdrsTargetName, " : ",
				"$(", MakeVarName, ".hs) ",
				"install_lib_dirs\n",
			"\tfor hdr in $(", MakeVarName, ".hs); do \\\n",
			"\t	$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\\n",
			"\t	$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\\n",
			"\tdone\n\n"
		])
	;
		% for non-MLDS grades, we don't need to install the header
		% files, so this rule does nothing
		io__write_strings(DepStream, [
			".PHONY : ", LibInstallHdrsTargetName, "\n",
			LibInstallHdrsTargetName, " :\n",
			"\t\n\n"
		])
	),

	module_name_to_file_name(ModuleName, ".check", no, CheckTargetName),
	module_name_to_file_name(ModuleName, ".ints", no, IntsTargetName),
	module_name_to_file_name(ModuleName, ".int3s", no, Int3sTargetName),
	module_name_to_file_name(ModuleName, ".opts", no, OptsTargetName),
	module_name_to_file_name(ModuleName, ".trans_opts", no,
						TransOptsTargetName),
	module_name_to_file_name(ModuleName, ".ss", no,
						SsTargetName),
	module_name_to_file_name(ModuleName, ".pic_ss", no,
						PicSsTargetName),
	module_name_to_file_name(ModuleName, ".rlos", no,
						RLOsTargetName),
	module_name_to_file_name(ModuleName, ".ils", no,
						ILsTargetName),
	module_name_to_file_name(ModuleName, ".javas", no,
						JavasTargetName),
	module_name_to_file_name(ModuleName, ".classes", no,
						ClassesTargetName),

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
	]),


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

	module_name_to_file_name(ModuleName, ".clean", no, CleanTargetName),
	io__write_strings(DepStream, [
		"clean_local : ", CleanTargetName, "\n"
	]),
	io__write_strings(DepStream, [
		".PHONY : ", CleanTargetName, "\n",
		CleanTargetName, " :\n",
		"\t-echo $(", MakeVarName, ".dirs) | xargs rm -rf \n",
		"\t-echo $(", MakeVarName, ".num_splits) | xargs rm -rf \n",
		"\t-echo $(", MakeVarName, ".cs) ", InitCFileName,
				" | xargs rm -f\n",
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
	]),

	io__write_string(DepStream, "\n"),

	module_name_to_file_name(ModuleName, ".realclean", no,
			RealCleanTargetName),
	io__write_strings(DepStream, [
		"realclean_local : ", RealCleanTargetName, "\n"
	]),
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
		"\t-echo $(", MakeVarName, ".ds) | xargs rm -f\n",
		"\t-echo $(", MakeVarName, ".module_deps) | xargs rm -f\n",
		"\t-echo $(", MakeVarName, ".all_hs) | xargs rm -f\n",
		"\t-echo $(", MakeVarName, ".dlls) | xargs rm -f\n",
		"\t-echo $(", MakeVarName, ".foreign_dlls) | xargs rm -f\n",
		"\t-echo $(", MakeVarName, ".classes) | xargs rm -f\n",
		"\t-echo $(", MakeVarName, ".rlos) | xargs rm -f\n"
	]),
	io__write_strings(DepStream, [
		"\t-rm -f ",
			ExeFileName, "$(EXT_FOR_EXE) ",
			SplitExeFileName, " ",
			SplitLibFileName, " ",
			InitFileName, " ",
			LibFileName, " ",
			SharedLibFileName, " ",
			DepFileName, " ",
			DvFileName, "\n\n"
	]).

:- pred get_source_file(deps_map, module_name, file_name).
:- mode get_source_file(in, in, out) is det.

get_source_file(DepsMap, ModuleName, FileName) :-
	map__lookup(DepsMap, ModuleName, Deps),
	Deps = deps(_, ModuleImports),
	module_imports_get_source_file_name(ModuleImports, SourceFileName),
	(
		string__remove_suffix(SourceFileName, ".m", SourceFileBase)
	->
		FileName = SourceFileBase
	;
		error("modules.m: source file name doesn't end in `.m'")
	).

:- pred append_to_init_list(io__output_stream, file_name, module_name,
				io__state, io__state).
:- mode append_to_init_list(in, in, in, di, uo) is det.

append_to_init_list(DepStream, InitFileName, Module) -->
	{ llds_out__make_init_name(Module, InitFuncName0) },
	{ string__append(InitFuncName0, "init", InitFuncName) },
	{ llds_out__make_rl_data_name(Module, RLName) },
	io__write_strings(DepStream, [
		"\techo ""INIT ", InitFuncName, """ >> ", InitFileName, "\n"
	]),
	globals__io_lookup_bool_option(aditi, Aditi),
	( { Aditi = yes } ->
		io__write_strings(DepStream, [
			"\techo ""ADITI_DATA ", RLName, """ >> ",
				InitFileName, "\n"
		])
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Find out which modules we need to generate C header files for,
	% assuming we're compiling with `--target asm'.
:- func modules_that_need_headers(list(module_name), deps_map)  =
		list(module_name).

modules_that_need_headers(Modules, DepsMap) =
	list__filter(module_needs_header(DepsMap), Modules).



	% Find out which modules will generate as external foreign
	% language files. 
	% We return the module names and file extensions.
:- func foreign_modules(list(module_name), deps_map) =
		assoc_list(module_name, string).

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
:- pred module_needs_header(deps_map::in, module_name::in) is semidet.

module_needs_header(DepsMap, Module) :-
	map__lookup(DepsMap, Module, deps(_, ModuleImports)),
	ModuleImports ^ foreign_code = contains_foreign_code(Langs),
	set__member(c, Langs).

	% Succeed iff we need to generate a foreign language output file 
	% for the specified module.
:- pred module_has_foreign(deps_map::in, module_name::in,
		list(foreign_language)::out) is semidet.

module_has_foreign(DepsMap, Module, LangList) :-
	map__lookup(DepsMap, Module, deps(_, ModuleImports)),
	ModuleImports ^ foreign_code = contains_foreign_code(Langs),
	LangList = set__to_sorted_list(Langs).

	% get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs) },
	% Find any extra .$O files that should be linked into the executable.
	% These include fact table object files and object files for foreign
	% code that can't be generated inline for this target.
:- pred get_extra_link_objects(list(module_name), deps_map, compilation_target,
		assoc_list(file_name, module_name)).
:- mode get_extra_link_objects(in, in, in, out) is det.

get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs) :-
	get_extra_link_objects_2(Modules, DepsMap, Target, [], ExtraLinkObjs0),
	list__reverse(ExtraLinkObjs0, ExtraLinkObjs).
		
:- pred get_extra_link_objects_2(list(module_name), deps_map,
	compilation_target, assoc_list(file_name, module_name),
	assoc_list(file_name, module_name)).
:- mode get_extra_link_objects_2(in, in, in, in, out) is det.

get_extra_link_objects_2([], _DepsMap, _Target, ExtraLinkObjs, ExtraLinkObjs).
get_extra_link_objects_2([Module | Modules], DepsMap, Target,
		ExtraLinkObjs0, ExtraLinkObjs) :-
	map__lookup(DepsMap, Module, deps(_, ModuleImports)),
	%
	% Handle object files for fact tables
	%
	FactDeps = ModuleImports ^ fact_table_deps,
	list__length(FactDeps, NumFactDeps),
	list__duplicate(NumFactDeps, Module, ModuleList),
	assoc_list__from_corresponding_lists(FactDeps, ModuleList,
		FactTableObjs),
	%
	% Handle object files for foreign code.
	% XXX currently we only support `C' foreign code.
	%
	(
		Target = asm,
		ModuleImports ^ foreign_code
			= contains_foreign_code(Langs),
		set__member(c, Langs)
	->
		prog_out__sym_name_to_string(Module, ".", FileName),
		NewLinkObjs = [(FileName ++ "__c_code") - Module |
			FactTableObjs]
	;
		NewLinkObjs = FactTableObjs
	),
	list__append(NewLinkObjs, ExtraLinkObjs0, ExtraLinkObjs1),
	get_extra_link_objects_2(Modules, DepsMap, Target,
		ExtraLinkObjs1, ExtraLinkObjs).

:- type module_foreign_info
	---> module_foreign_info(
		used_foreign_languages :: set(foreign_language),
		foreign_proc_languages :: map(sym_name, foreign_language),
		all_foreign_import_module_info :: foreign_import_module_info,
		module_contains_foreign_export :: contains_foreign_export
	).

:- pred get_item_list_foreign_code(globals::in, item_list::in,
	set(foreign_language)::out, foreign_import_module_info::out,
	contains_foreign_export::out) is det.

get_item_list_foreign_code(Globals, Items, LangSet, ForeignImports,
		ContainsPragmaExport) :-
	Info0 = module_foreign_info(set__init,
			map__init, [], no_foreign_export),
	list__foldl(get_item_foreign_code(Globals), Items, Info0, Info),
	Info = module_foreign_info(LangSet0, LangMap,
			ForeignImports, ContainsPragmaExport),
	ForeignProcLangs = map__values(LangMap),
	LangSet = set__insert_list(LangSet0, ForeignProcLangs).

:- pred get_item_foreign_code(globals::in, item_and_context::in,
		module_foreign_info::in, module_foreign_info::out) is det.

get_item_foreign_code(Globals, Item, Info0, Info) :-
    ( Item = pragma(Pragma) - Context ->
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
		foreign_language(Attrs, NewLang),
		( OldLang = Info0 ^ foreign_proc_languages ^ elem(Name) ->
			% is it better than an existing one? 
			( 
				yes = prefer_foreign_language(Globals,
					Target, OldLang, NewLang)
			->
				Info = Info0 ^ foreign_proc_languages
			    			^ elem(Name) := NewLang
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
		% XXX handle lang \= c for
		% `:- pragma foreign_import_module'.
		Pragma = foreign_import_module(Lang, Import),
		Lang = c,
		list__member(c, BackendLangs)
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
	)
    ;
	Info = Info0
    ).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(module_name), deps_map, list(module_name)).
:- mode select_ok_modules(in, in, out) is det.

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

:- pred write_dependencies_list(list(module_name), string, io__output_stream,
				io__state, io__state).
:- mode write_dependencies_list(in, in, in, di, uo) is det.

write_dependencies_list([], _, _) --> [].
write_dependencies_list([Module | Modules], Suffix, DepStream) -->
	module_name_to_file_name(Module, Suffix, no, FileName),
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, FileName),
	write_dependencies_list(Modules, Suffix, DepStream).

	% Generate the list of .NET DLLs which could be referred to by this
	% module (including the module itself).
	% If we are compiling a module within the standard library we should
	% reference the runtime DLLs and all other library DLLs.  If we are
	% outside the library we should just reference mercury.dll (which will
	% contain all the DLLs).
	
:- func referenced_dlls(module_name, list(module_name)) = list(module_name).

referenced_dlls(Module, DepModules0) = Modules :-
	DepModules = [Module | DepModules0],

		% If we are not compiling a module in the mercury
		% std library then replace all the std library dlls with
		% one reference to mercury.dll.
	( Module = unqualified(Str), mercury_std_library_module(Str) ->
			% In the standard library we need to add the
			% runtime dlls.
		Modules = list__remove_dups(
			[unqualified("mercury_mcpp"),
				unqualified("mercury_il") | DepModules])
	;
		F = (func(M) =
			( if 
				M = unqualified(S),
				mercury_std_library_module(S)
			then
				unqualified("mercury")
			else
					% A sub module is located in the
					% top level assembly.
				unqualified(outermost_qualifier(M))
			)
		),
		Modules = list__remove_dups(list__map(F, DepModules))
	).

	% submodules(Module, Imports)
	% returns the list of submodules from Imports which are sub-modules of
	% Module, if Module is a top level module and not in the std library.
	% Otherwise it returns the empty list.
:- func submodules(module_name, list(module_name)) = list(module_name).

submodules(Module, Modules0) = Modules :-
	( Module = unqualified(Str), \+ mercury_std_library_module(Str) ->
		P = (pred(M::in) is semidet :-
			Str = outermost_qualifier(M),
			M \= Module
		),
		list__filter(P, Modules0, Modules)
	;
		Modules = []
	).

:- pred write_dll_dependencies_list(list(module_name),
		string, io__output_stream, io__state, io__state).
:- mode write_dll_dependencies_list(in, in, in, di, uo) is det.

write_dll_dependencies_list(Modules, Prefix, DepStream) -->
	list__foldl(write_dll_dependency(DepStream, Prefix), Modules).

:- pred write_dll_dependency(io__output_stream, string, module_name,
				io__state, io__state).
:- mode write_dll_dependency(in, in, in, di, uo) is det.

write_dll_dependency(DepStream, Prefix, Module) -->
	module_name_to_file_name(Module, ".dll", no, FileName),
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, Prefix),
	io__write_string(DepStream, FileName).

:- pred write_fact_table_dependencies_list(module_name, list(file_name),
			string, io__output_stream, io__state, io__state).
:- mode write_fact_table_dependencies_list(in, in, in, in, di, uo) is det.

write_fact_table_dependencies_list(_, [], _, _) --> [].
write_fact_table_dependencies_list(Module, [FactTable | FactTables], Suffix,
			DepStream) -->
	fact_table_file_name(Module, FactTable, Suffix, FileName),
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, FileName),
	write_fact_table_dependencies_list(Module, FactTables, Suffix,
			DepStream).

:- pred write_extra_link_dependencies_list(assoc_list(file_name, module_name),
			string, io__output_stream, io__state, io__state).
:- mode write_extra_link_dependencies_list(in, in, in, di, uo) is det.

write_extra_link_dependencies_list([], _, _) --> [].
write_extra_link_dependencies_list([ExtraLink - Module | ExtraLinks], Suffix,
			DepStream) -->
	extra_link_obj_file_name(Module, ExtraLink, Suffix, FileName),
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, FileName),
	write_extra_link_dependencies_list(ExtraLinks, Suffix, DepStream).

:- pred write_file_dependencies_list(list(string), string, io__output_stream,
				io__state, io__state).
:- mode write_file_dependencies_list(in, in, in, di, uo) is det.

write_file_dependencies_list([], _, _) --> [].
write_file_dependencies_list([FileName | FileNames], Suffix, DepStream) -->
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, FileName),
	io__write_string(DepStream, Suffix),
	write_file_dependencies_list(FileNames, Suffix, DepStream).

%-----------------------------------------------------------------------------%

:- pred write_compact_dependencies_list(list(module_name), string, string,
	maybe(pair(string)), io__output_stream, io__state, io__state).
:- mode write_compact_dependencies_list(in, in, in, in, in, di, uo) is det.

write_compact_dependencies_list(Modules, Prefix, Suffix, Basis, DepStream) -->
	(
		{ Basis = yes(VarName - OldSuffix) },
		% Don't use the compact dependency lists for names of header
		% files for modules in the standard library, because it
		% doesn't take into account the "mercury." prefix
		% that gets added to those header file names in MLDS grades.
		\+ {
			(Suffix = ".h" ; Suffix = ".h.tmp"),
			list__member(unqualified(StdLibModule), Modules),
			mercury_std_library_module(StdLibModule)
		}
	->
		io__write_string(DepStream, "$("),
		io__write_string(DepStream, VarName),
		io__write_string(DepStream, ":%"),
		io__write_string(DepStream, OldSuffix),
		io__write_string(DepStream, "="),
		io__write_string(DepStream, Prefix),
		io__write_string(DepStream, "%"),
		io__write_string(DepStream, Suffix),
		io__write_string(DepStream, ")")
	;
		write_dependencies_list(Modules, Suffix, DepStream)
	).

:- pred write_compact_dependencies_separator(maybe(pair(string)),
	io__output_stream, io__state, io__state).
:- mode write_compact_dependencies_separator(in, in, di, uo) is det.

write_compact_dependencies_separator(no, _DepStream) --> [].
write_compact_dependencies_separator(yes(_), DepStream) -->
	io__write_string(DepStream, " ").

%-----------------------------------------------------------------------------%

	% Look up a module in the dependency map
	% If we don't know its dependencies, read the
	% module and save the dependencies in the dependency map.

:- pred lookup_dependencies(module_name, deps_map, bool, bool,
		module_imports, deps_map, io__state, io__state).
:- mode lookup_dependencies(in, in, in, out, out, out, di, uo) is det.

lookup_dependencies(Module, DepsMap0, Search, Done, ModuleImports, DepsMap) -->
	(
		{ map__search(DepsMap0, Module,
			deps(Done0, ModuleImports0)) }
	->
		{ Done = Done0 },
		{ ModuleImports0 = ModuleImports },
		{ DepsMap = DepsMap0 }
	;
		read_dependencies(Module, Search, ModuleImportsList),
		{ list__foldl(insert_into_deps_map, ModuleImportsList,
			DepsMap0, DepsMap) },
		{ map__lookup(DepsMap, Module, deps(Done, ModuleImports)) }
	).

	%
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
:- pred insert_into_deps_map(module_imports, deps_map, deps_map).
:- mode insert_into_deps_map(in, in, out) is det.
insert_into_deps_map(ModuleImports, DepsMap0, DepsMap) :-
	module_imports_get_module_name(ModuleImports, ModuleName),
	map__set(DepsMap0, ModuleName, deps(no, ModuleImports), DepsMap).

	% Read a module to determine the (direct) dependencies
	% of that module and any nested sub-modules it contains.

:- pred read_dependencies(module_name, bool, list(module_imports),
			io__state, io__state).
:- mode read_dependencies(in, in, out, di, uo) is det.

read_dependencies(ModuleName, Search, ModuleImportsList) -->
	read_mod_ignore_errors(ModuleName, ".m",
		"Getting dependencies for module", Search, no, Items0, Error,
		FileName0, _),
	( { Items0 = [], Error = fatal_module_errors } ->
		read_mod_ignore_errors(ModuleName, ".int", 
		    "Getting dependencies for module interface", Search, no,
		    Items, _Error, FileName, _),
		{ SubModuleList = [ModuleName - Items] }
	;
		{ FileName = FileName0 },
		{ Items = Items0 },
		split_into_submodules(ModuleName, Items, SubModuleList)
	),
	globals__io_get_globals(Globals),
	{ assoc_list__keys(SubModuleList, SubModuleNames) },
	{ list__map(init_dependencies(FileName, ModuleName, SubModuleNames,
		Error, Globals), SubModuleList, ModuleImportsList) }.

init_dependencies(FileName, SourceFileModuleName, NestedModuleNames,
		Error, Globals, ModuleName - Items, ModuleImports) :-
	ParentDeps = get_ancestors(ModuleName),

	get_dependencies(Items, ImplImportDeps0, ImplUseDeps0),
	add_implicit_imports(Items, Globals, ImplImportDeps0, ImplUseDeps0,
		ImplImportDeps, ImplUseDeps),
	list__append(ImplImportDeps, ImplUseDeps, ImplementationDeps),

	get_interface(Items, InterfaceItems),
	get_dependencies(InterfaceItems, InterfaceImportDeps0,
		InterfaceUseDeps0),
	add_implicit_imports(InterfaceItems, Globals,
		InterfaceImportDeps0, InterfaceUseDeps0,
		InterfaceImportDeps, InterfaceUseDeps),
	list__append(InterfaceImportDeps, InterfaceUseDeps, 
		InterfaceDeps),

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
	get_item_list_foreign_code(Globals, Items, LangSet, ForeignImports,
		ContainsPragmaExport),
	ContainsForeignCode =
		(if 
			not set__empty(LangSet)
		then
			contains_foreign_code(LangSet)
		else
			no_foreign_code
		),

	%
	% Work out whether the items contain main/2.
	%
	(
		list__member(Item, Items),
		Item = pred_or_func(_, _, _, predicate, Name,
			[_, _], WithType, _, _, _, _, _) - _,
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

:- pred read_mod(read_modules, module_name, string, string, bool, bool,
		item_list, module_error, file_name, maybe(timestamp),
		io__state, io__state).
:- mode read_mod(in, in, in, in, in, in, out, out, out, out, di, uo) is det.

read_mod(ReadModules, ModuleName, Extension, Descr, Search, ReturnTimestamp,
		Items, Error, FileName, MaybeTimestamp) -->
	(
		{ find_read_module(ReadModules, ModuleName, Extension,
			ReturnTimestamp, Items0, MaybeTimestamp0,
			Error0, FileName0) }
	->
		{ Error = Error0 },
		{ Items = Items0 },
		{ MaybeTimestamp = MaybeTimestamp0 },
		{ FileName = FileName0 }
	;
		read_mod(ModuleName, Extension, Descr, Search, ReturnTimestamp,
			Items, Error, FileName, MaybeTimestamp)
	).

read_mod(ModuleName, Extension, Descr, Search, ReturnTimestamp,
		Items, Error, FileName, MaybeTimestamp) -->
	read_mod_2(no, ModuleName, Extension, Descr, Search,
		no, ReturnTimestamp, Items, Error, FileName, MaybeTimestamp).

read_mod_if_changed(ModuleName, Extension, Descr, Search, OldTimestamp,
		Items, Error, FileName, MaybeTimestamp) -->
	read_mod_2(no, ModuleName, Extension, Descr, Search,
		yes(OldTimestamp), yes, Items, Error,
		FileName, MaybeTimestamp).

read_mod_ignore_errors(ModuleName, Extension, Descr, Search, ReturnTimestamp,
		Items, Error, FileName, MaybeTimestamp) -->
	read_mod_2(yes, ModuleName, Extension, Descr, Search,
		no, ReturnTimestamp, Items, Error, FileName, MaybeTimestamp).

:- pred read_mod_2(bool, module_name, string, string,
		bool, maybe(timestamp), bool, item_list, module_error,
		file_name, maybe(timestamp), io__state, io__state).
:- mode read_mod_2(in, in, in, in, in, in, in, out, out, out, out,
		di, uo) is det.

read_mod_2(IgnoreErrors, ModuleName,
		Extension, Descr, Search, MaybeOldTimestamp,
		ReturnTimestamp, Items, Error, FileName, MaybeTimestamp) -->
	module_name_to_file_name(ModuleName, Extension, no, FileName0),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, FileName0),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),

	( { Search = yes } ->
		globals__io_lookup_accumulating_option(search_directories,
			SearchDirs)
	;
		{ SearchDirs = [dir__this_directory] }
	),
	{ Extension = ".m" ->
		% For `.m' files we need to deal with the case where
		% the module name does not match the file name.
		OpenFile = search_for_module_source(SearchDirs, ModuleName)
	;
		OpenFile = search_for_file(SearchDirs, FileName0)
	},
	( { MaybeOldTimestamp = yes(OldTimestamp) } ->
		prog_io__read_module_if_changed(OpenFile, ModuleName,
			OldTimestamp, Error0, MaybeFileName, ActualModuleName,
			Messages, Items0, MaybeTimestamp0)
	;
		prog_io__read_module(OpenFile, ModuleName,
			ReturnTimestamp, Error0, MaybeFileName,
			ActualModuleName, Messages, Items0, MaybeTimestamp0)
	),

	{
		MaybeFileName = yes(FileName)
	;
		MaybeFileName = no,
		FileName = FileName0
	},
	check_module_has_expected_name(FileName, ModuleName, ActualModuleName),

	check_timestamp(FileName0, MaybeTimestamp0, MaybeTimestamp),
	( { IgnoreErrors = yes } ->
		(
			{ Error0 = fatal_module_errors },
			{ Items0 = [] }
		->
			maybe_write_string(VeryVerbose, "not found.\n")
		;
			maybe_write_string(VeryVerbose, "done.\n")
		)
	;
		(
			{ Error0 = fatal_module_errors },
			maybe_write_string(VeryVerbose,
				"fatal error(s).\n"),
			io__set_exit_status(1)
		;
			{ Error0 = some_module_errors },
			maybe_write_string(VeryVerbose,
				"parse error(s).\n"),
			io__set_exit_status(1)
		;
			{ Error0 = no_module_errors },
			maybe_write_string(VeryVerbose,
				"successful parse.\n")
		),
		prog_out__write_messages(Messages)
	),
	{ Error = Error0 },
	{ Items = Items0 }.

read_mod_from_file(FileName, Extension, Descr, Search, ReturnTimestamp,
		Items, Error, ModuleName, MaybeTimestamp) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, FileName),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	{ string__append(FileName, Extension, FullFileName) },
	{ dir__basename(FileName, BaseFileName) },
	{ file_name_to_module_name(BaseFileName, DefaultModuleName) },
	( { Search = yes } ->
		globals__io_lookup_accumulating_option(search_directories,
			SearchDirs)
	;
		{ SearchDirs = [dir__this_directory] }
	),
	{ OpenFile = search_for_file(SearchDirs, FullFileName) },
	prog_io__read_module(OpenFile, DefaultModuleName,
		ReturnTimestamp, Error, _, ModuleName, Messages, Items,
		MaybeTimestamp0),
	check_timestamp(FullFileName, MaybeTimestamp0, MaybeTimestamp),
	(
		{ Error = fatal_module_errors },
		maybe_write_string(VeryVerbose, "fatal error(s).\n"),
		io__set_exit_status(1)
	;
		{ Error = some_module_errors },
		maybe_write_string(VeryVerbose, "parse error(s).\n"),
		io__set_exit_status(1)
	;
		{ Error = no_module_errors },
		maybe_write_string(VeryVerbose, "successful parse.\n")
	),
	prog_out__write_messages(Messages).

:- pred check_timestamp(file_name, maybe(io__res(timestamp)), maybe(timestamp), 
		io__state, io__state).
:- mode check_timestamp(in, in, out, di, uo) is det.

check_timestamp(FileName, MaybeTimestamp0, MaybeTimestamp) -->
	(
		{ MaybeTimestamp0 = yes(ok(Timestamp)) },
		{ MaybeTimestamp = yes(Timestamp) }
	;
		{ MaybeTimestamp0 = yes(error(IOError)) },
		{ MaybeTimestamp = no },
		report_modification_time_warning(
			FileName, IOError)
	;
		{ MaybeTimestamp0 = no },
		{ MaybeTimestamp = no }
	).

/*
:- pred combine_module_errors(module_error, module_error, module_error).
:- mode combine_module_errors(in, in, out) is det.

combine_module_errors(fatal, _, fatal).
combine_module_errors(yes, fatal, fatal).
combine_module_errors(yes, yes, yes).
combine_module_errors(yes, no, yes).
combine_module_errors(no, Error, Error).
*/

%-----------------------------------------------------------------------------%

process_module_private_interfaces(_, [], DirectImports, DirectImports,
		DirectUses, DirectUses, Module, Module) --> [].
process_module_private_interfaces(ReadModules, [Ancestor | Ancestors],
		DirectImports0, DirectImports, DirectUses0, DirectUses,
		Module0, Module) -->
	{ ModuleName = Module0 ^ module_name },
	{ ModAncestors0 = Module0 ^ parent_deps },
	(
		{ Ancestor = ModuleName }
	->
		{ error("modules.m: module is its own ancestor?") }
	;
		{ list__member(Ancestor, ModAncestors0) }
	->
		% we've already read it
		process_module_private_interfaces(ReadModules, Ancestors,
				DirectImports0, DirectImports,
				DirectUses0, DirectUses,
				Module0, Module)
	;
		{ ModItems0 = Module0 ^ items },
		{ ModError0 = Module0 ^ error },
		{ Module0 ^ maybe_timestamps = yes(_) ->
			ReturnTimestamp = yes
		;
			ReturnTimestamp = no
		},
		read_mod(ReadModules, Ancestor, ".int0",
			"Reading private interface for module", yes,
			ReturnTimestamp, PrivateIntItems, PrivateIntError,
			_AncestorFileName, MaybeTimestamp),

		maybe_record_timestamp(Ancestor, ".int0", may_be_unqualified,
			MaybeTimestamp, Module0, Module1),

		{ strip_off_interface_decl(PrivateIntItems, Items) },
		{ maybe_add_int_error(PrivateIntError, ModError0, ModError) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		( { PrivateIntError = fatal_module_errors } ->
			{ ModAncestors = ModAncestors0 }
		;
			{ ModAncestors = [Ancestor | ModAncestors0] }
		),
		{ get_dependencies(Items, AncDirectImports, AncDirectUses) },
		{ list__append(DirectImports0, AncDirectImports,
				DirectImports1) },
		{ list__append(DirectUses0, AncDirectUses, DirectUses1) },
		{ list__append(ModItems0, Items, ModItems) },
		{ Module2 = ((Module1 ^ items := ModItems)
				      ^ parent_deps := ModAncestors)
				      ^ error := ModError },
		process_module_private_interfaces(ReadModules, Ancestors,
				DirectImports1, DirectImports, DirectUses1,
				DirectUses, Module2, Module)
	).

%-----------------------------------------------------------------------------%

process_module_long_interfaces(_, _, [], _Ext,
		IndirectImports, IndirectImports, Module, Module) --> [].
process_module_long_interfaces(ReadModules, NeedQualifier, [Import | Imports],
		Ext, IndirectImports0, IndirectImports, Module0, Module) -->
	{ ModuleName = Module0 ^ module_name },
	{ ModImplementationImports0 = Module0 ^ impl_deps },
	(
		% have we already read it?
		( { Import = ModuleName }
		; { list__member(Import, Module0 ^ parent_deps) }
		; { list__member(Import, Module0 ^ int_deps) }
		; { list__member(Import, ModImplementationImports0) }
		)
	->
		process_module_long_interfaces(ReadModules, NeedQualifier,
			Imports, Ext, IndirectImports0, IndirectImports,
			Module0, Module)
	;
		{ ModItems0 = Module0 ^ items },
		{ ModError0 = Module0 ^ error },
		{ Module0 ^ maybe_timestamps = yes(_) ->
			ReturnTimestamp = yes
		;
			ReturnTimestamp = no
		},
		read_mod(ReadModules, Import, Ext,
			"Reading interface for module", yes, ReturnTimestamp,
			LongIntItems, LongIntError, _LongIntFileName,
			MaybeTimestamp),

		{ strip_off_interface_decl(LongIntItems, Items) },
		{ maybe_add_int_error(LongIntError, ModError0, ModError) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		( { LongIntError = fatal_module_errors } ->
			{ ModImplementationImports =
				ModImplementationImports0 },
			{ Module1 = Module0 }
		;
			maybe_record_timestamp(Import, Ext, NeedQualifier,
				MaybeTimestamp, Module0, Module1),
			{ ModImplementationImports =
				[Import | ModImplementationImports0] }
		),
		{ get_dependencies(Items, IndirectImports1, IndirectUses1) },
		{ list__append(IndirectImports0, IndirectImports1,
			IndirectImports2) },
		{ list__append(IndirectImports2, IndirectUses1,
			IndirectImports3) },
		{ list__append(ModItems0, Items, ModItems) },
		{ Module2 = ((Module1 ^ impl_deps := ModImplementationImports)
				      ^ items := ModItems)
				      ^ error := ModError },

		process_module_long_interfaces(ReadModules, NeedQualifier,
			Imports, Ext, IndirectImports3, IndirectImports,
			Module2, Module)
	).

:- pred check_imports_accessibility(module_name, list(module_name), item_list,
				io__state, io__state).
:- mode check_imports_accessibility(in, in, in, di, uo) is det.

	%
	% At this point, we've read in all the appropriate interface files,
	% including, for every imported/used module, at least the short
	% interface for that module's parent module, which will contain
	% the `include_module' declarations for any exported sub-modules
	% of the parent.  So the accessible sub-modules can be determined
	% by just calling get_children on the complete item list.
	%
	% We then go through all of the imported/used modules,
	% checking that each one is accessible.
	%
check_imports_accessibility(ModuleName, Imports, Items) -->
	{ get_children(Items, AccessibleSubModules) },
	list__foldl(check_module_accessibility(ModuleName,
		AccessibleSubModules, Items), Imports).

:- pred check_module_accessibility(module_name, list(module_name), item_list,
		module_name, io__state, io__state).
:- mode check_module_accessibility(in, in, in, in, di, uo) is det.

check_module_accessibility(ModuleName, AccessibleSubModules, Items,
		ImportedModule) -->
	( { ImportedModule = qualified(ParentModule, SubModule) } ->
		(
			{ list__member(ImportedModule, AccessibleSubModules) }
		->
			[]
		;
			% The user attempted to import an inaccessible
			% sub-module, so report an error.
			% Unfortunately we didn't get passed the
			% context of the `import_module' or `use_module'
			% declaration(s), so we need to search the item
			% list again to find them.
			{ FindImports = lambda([Item::in] is semidet, (
				Item = module_defn(_, ModuleDefn) - _,
				( ModuleDefn = import(module(Mods))
				; ModuleDefn = use(module(Mods))
				),
				list__member(ImportedModule, Mods)
			  )) },
			{ list__filter(FindImports, Items, ImportItems) },
			{ ImportItems = [] ->
				error("check_parent_module")
			;
				true
			},
			list__foldl(report_inaccessible_module_error(
				ModuleName, ParentModule, SubModule),
				ImportItems)
		)
	;
		[]
	).

:- pred report_inaccessible_module_error(module_name, module_name, string,
			item_and_context, io__state, io__state).
:- mode report_inaccessible_module_error(in, in, in, in, di, uo) is det.

/*
The error message should come out like this
(the second sentence is included only with --verbose-errors):
very_long_name.m:123: In module `very_long_name':
very_long_name.m:123:   error in `import_module' declaration:
very_long_name.m:123:   module `parent_module:sub_module' is inaccessible.
very_long_name.m:123:   Either there was no prior `import_module' or 
very_long_name.m:123:   `use_module' declaration to import module
very_long_name.m:123:   `parent_module', or the interface for module
very_long_name.m:123:   `parent_module' does not contain an `include_module'
very_long_name.m:123:   declaration for module `sub_module'.
*/

report_inaccessible_module_error(ModuleName, ParentModule, SubModule,
		Item - Context) -->
	{ Item = module_defn(_, import(module(_))) ->
		DeclName = "import_module"
	; Item = module_defn(_, use(module(_))) ->
		DeclName = "use_module"
	;
		error("report_inaccessible_parent_error: invalid item")
	},
	prog_out__write_context(Context),
	io__write_string("In module `"),
	prog_out__write_sym_name(ModuleName),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_strings(["  error in `", DeclName, "' declaration:\n"]),
	prog_out__write_context(Context),
	io__write_string("  module `"),
	prog_out__write_sym_name(qualified(ParentModule, SubModule)),
	io__write_string("' is inaccessible.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  Either there was no prior "),
		io__write_string("`import_module' or\n"),
		prog_out__write_context(Context),
		io__write_string("  `use_module' declaration to import "),
		io__write_string("module\n"),
		prog_out__write_context(Context),
		io__write_string("  `"),
		prog_out__write_sym_name(ParentModule),
		io__write_string("', or the interface for module\n"),
		prog_out__write_context(Context),
		io__write_string("  `"),
		prog_out__write_sym_name(ParentModule),
		io__write_string("' does not contain an `include_module'\n"),
		prog_out__write_context(Context),
		io__write_string("  declaration for module `"),
		io__write_string(SubModule),
		io__write_string("'.\n")
	;
		[]
	),
	io__set_exit_status(1).

%-----------------------------------------------------------------------------%

process_module_short_interfaces_transitively(ReadModules, Imports, Ext,
		Module0, Module) -->
	process_module_short_interfaces(ReadModules, Imports, Ext, [],
		IndirectImports, Module0, Module1),
	( { IndirectImports = [] } ->
		{ Module = Module1 }
	;
		process_module_short_interfaces_transitively(ReadModules,
			IndirectImports, Ext, Module1, Module)
	).

process_module_short_interfaces(_, [], _,
		IndirectImports, IndirectImports, Module, Module) --> [].
process_module_short_interfaces(ReadModules, [Import | Imports], Ext, 
		IndirectImports0, IndirectImports, Module0, Module) -->
	{ ModIndirectImports0 = Module0 ^ indirect_deps },
	(
		% check if the imported module has already been imported
		{ Import = Module0 ^ module_name
		; list__member(Import, Module0 ^ parent_deps)
		; list__member(Import, Module0 ^ int_deps)
		; list__member(Import, Module0 ^ impl_deps)
		; list__member(Import, ModIndirectImports0)
		}
	->
		process_module_short_interfaces(ReadModules, Imports, Ext,
			IndirectImports0, IndirectImports, Module0, Module)
	;
		{ ModItems0 = Module0 ^ items },
		{ ModError0 = Module0 ^ error },
		{ Module0 ^ maybe_timestamps = yes(_) ->
			ReturnTimestamp = yes
		;
			ReturnTimestamp = no
		},
		read_mod(ReadModules, Import, Ext,
			"Reading short interface for module", yes,
			ReturnTimestamp, ShortIntItems, ShortIntError,
			_ImportFileName, MaybeTimestamp),
		maybe_record_timestamp(Import, Ext, must_be_qualified,
			MaybeTimestamp, Module0, Module1),

		{ strip_off_interface_decl(ShortIntItems, Items) },
		{ maybe_add_int_error(ShortIntError, ModError0, ModError) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		{ ModIndirectImports = [Import | ModIndirectImports0] },
		{ get_dependencies(Items, Imports1, Uses1) },
		{ list__append(IndirectImports0, Imports1, IndirectImports1) },
		{ list__append(IndirectImports1, Uses1, IndirectImports2) },
		{ list__append(ModItems0, Items, ModItems) },
		{ Module2 = ((Module1 ^ indirect_deps := ModIndirectImports)
				      ^ items := ModItems)
				      ^ error := ModError },
		process_module_short_interfaces(ReadModules, Imports, Ext,
			IndirectImports2, IndirectImports, Module2, Module)
	).

strip_off_interface_decl(Items0, Items) :-
	(
		Items0 = [ FirstItem | Items1 ],
		FirstItem = module_defn(_, interface) - _
	->
		Items = Items1
	;
		Items = Items0
	).

:- pred maybe_add_int_error(module_error, module_error, module_error).
:- mode maybe_add_int_error(in, in, out) is det.

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
	%	IncludeDeps is the list of sub-modules declared with
	% 	`:- include_module' in Items.
	%
:- pred get_children(item_list, list(module_name)).
:- mode get_children(in, out) is det.

get_children(Items, IncludeDeps) :-
	get_children_2(Items, [], IncludeDeps).

:- pred get_children_2(item_list, list(module_name), list(module_name)).
:- mode get_children_2(in, in, out) is det.

get_children_2([], IncludeDeps, IncludeDeps).
get_children_2([Item - _Context | Items], IncludeDeps0, IncludeDeps) :-
	( 
		Item = module_defn(_VarSet, include_module(Modules))
	->
		list__append(IncludeDeps0, Modules, IncludeDeps1)
	;
		IncludeDeps1 = IncludeDeps0
	),
	get_children_2(Items, IncludeDeps1, IncludeDeps).

%-----------------------------------------------------------------------------%

get_dependencies(Items, ImportDeps, UseDeps) :-
	get_dependencies_implementation(Items, [], [] , [], [],
			IntImportDeps, IntUseDeps, ImpImportDeps, ImpUseDeps),
	list__append(IntImportDeps, ImpImportDeps, ImportDeps),
	list__append(IntUseDeps, ImpUseDeps, UseDeps).

	% get_dependencies(Items, IntImportDeps, IntUseDeps,
	% 		ImpImportDeps, ImpUseDeps).
	%	Get the list of modules that a list of items (explicitly)
	%	depends on.
	%	IntImportDeps is the list of modules imported using `:-
	%	import_module' in the interface, and ImpImportDeps those
	%	modules imported in the implementation. IntUseDeps is the
	%	list of modules imported using `:- use_module' in the
	%	interface, and ImpUseDeps those modules imported in the
	%	implementation.
	%	N.B. Typically you also need to consider the module's
	%	implicit dependencies (see get_implicit_dependencies/3),
	%	its parent modules (see get_ancestors/1) and possibly
	%	also the module's child modules (see get_children/2).
	%	You may also need to consider indirect dependencies.
	%
	%	N.B This predicate assumes that any declarations between
	%	the `:- module' and the first `:- interface' or
	%	`:- implementation' are in the implementation.
	%
:- pred get_dependencies(item_list::in,
		list(module_name)::out, list(module_name)::out,
		list(module_name)::out, list(module_name)::out) is det.

get_dependencies(Items, IntImportDeps, IntUseDeps, ImpImportDeps, ImpUseDeps) :-
	get_dependencies_implementation(Items, [], [] , [], [],
			IntImportDeps, IntUseDeps, ImpImportDeps, ImpUseDeps).

:- pred get_dependencies_implementation(item_list::in, list(module_name)::in,
		list(module_name)::in, list(module_name)::in,
		list(module_name)::in, list(module_name)::out,
		list(module_name)::out, list(module_name)::out,
		list(module_name)::out) is det.

get_dependencies_implementation([], IntImportDeps, IntUseDeps,
		ImpImportDeps, ImpUseDeps, IntImportDeps, IntUseDeps,
		ImpImportDeps, ImpUseDeps).
get_dependencies_implementation([Item - _Context | Items],
		IntImportDeps0, IntUseDeps0,
		ImpImportDeps0, ImpUseDeps0,
		IntImportDeps, IntUseDeps,
		ImpImportDeps, ImpUseDeps) :-
	( 
		
		Item = module_defn(_VarSet, interface)
	->
		get_dependencies_interface(Items,
				IntImportDeps0, IntUseDeps0,
				ImpImportDeps0, ImpUseDeps0,
				IntImportDeps, IntUseDeps,
				ImpImportDeps, ImpUseDeps)
	;
		(
		
			Item = module_defn(_VarSet, import(module(Modules)))
		->
			list__append(ImpImportDeps0, Modules, ImpImportDeps1),
			ImpUseDeps1 = ImpUseDeps0
		;
			Item = module_defn(_VarSet, use(module(Modules)))
		->
			list__append(ImpUseDeps0, Modules, ImpUseDeps1),
			ImpImportDeps1 = ImpImportDeps0
		;
			ImpImportDeps1 = ImpImportDeps0,
			ImpUseDeps1 = ImpUseDeps0
		),
		get_dependencies_implementation(Items,
				IntImportDeps0, IntUseDeps0,
				ImpImportDeps1, ImpUseDeps1,
				IntImportDeps, IntUseDeps,
				ImpImportDeps, ImpUseDeps)
	).

:- pred get_dependencies_interface(item_list::in, list(module_name)::in,
		list(module_name)::in, list(module_name)::in,
		list(module_name)::in, list(module_name)::out,
		list(module_name)::out, list(module_name)::out,
		list(module_name)::out) is det.

get_dependencies_interface([], IntImportDeps, IntUseDeps,
		ImpImportDeps, ImpUseDeps, IntImportDeps, IntUseDeps,
		ImpImportDeps, ImpUseDeps).
get_dependencies_interface([Item - _Context | Items],
		IntImportDeps0, IntUseDeps0,
		ImpImportDeps0, ImpUseDeps0,
		IntImportDeps, IntUseDeps,
		ImpImportDeps, ImpUseDeps) :-
	( 
		
		Item = module_defn(_VarSet, implementation)
	->
		get_dependencies_implementation(Items,
				IntImportDeps0, IntUseDeps0,
				ImpImportDeps0, ImpUseDeps0,
				IntImportDeps, IntUseDeps,
				ImpImportDeps, ImpUseDeps)
	;
		(
		
			Item = module_defn(_VarSet, import(module(Modules)))
		->
			list__append(IntImportDeps0, Modules, IntImportDeps1),
			IntUseDeps1 = IntUseDeps0
		;
			Item = module_defn(_VarSet, use(module(Modules)))
		->
			list__append(IntUseDeps0, Modules, IntUseDeps1),
			IntImportDeps1 = IntImportDeps0
		;
			IntImportDeps1 = IntImportDeps0,
			IntUseDeps1 = IntUseDeps0
		),
		get_dependencies_interface(Items,
				IntImportDeps1, IntUseDeps1,
				ImpImportDeps0, ImpUseDeps0,
				IntImportDeps, IntUseDeps,
				ImpImportDeps, ImpUseDeps)
	).

%-----------------------------------------------------------------------------%

	% get the fact table dependencies for a module
:- pred get_fact_table_dependencies(item_list, list(string)).
:- mode get_fact_table_dependencies(in, out) is det.

get_fact_table_dependencies(Items, Deps) :-
	get_fact_table_dependencies_2(Items, [], Deps).

:- pred get_fact_table_dependencies_2(item_list, list(string), list(string)).
:- mode get_fact_table_dependencies_2(in, in, out) is det.

get_fact_table_dependencies_2([], Deps, Deps).
get_fact_table_dependencies_2([Item - _Context | Items], Deps0, Deps) :-
	(
		Item = pragma(fact_table(_SymName, _Arity, FileName))
	->
		Deps1 = [FileName | Deps0]
	;
		Deps1 = Deps0
	),
	get_fact_table_dependencies_2(Items, Deps1, Deps).

%-----------------------------------------------------------------------------%

:- type submodule_map == map(module_name, item_list).

	% Given a module (well, a list of items), split it into
	% its constituent sub-modules, in top-down order.
split_into_submodules(ModuleName, Items0, ModuleList) -->
	{ InParentInterface = no },
	split_into_submodules_2(ModuleName, Items0, InParentInterface,
		Items, ModuleList),
	{ require(unify(Items, []), "modules.m: items after end_module") },
	%
	% check for modules declared as both nested and separate sub-modules
	%
	{ get_children(Items0, NestedSubmodules) },
	{ assoc_list__keys(ModuleList, SeparateSubModules) },
	{ Duplicates = set__intersect(set__list_to_set(NestedSubmodules),
				set__list_to_set(SeparateSubModules)) },
	( { set__empty(Duplicates) } ->
		[]
	;
		report_duplicate_modules(Duplicates, Items0)
	).

:- pred split_into_submodules_2(module_name, item_list, bool, item_list,
				module_list, io__state, io__state).
:- mode split_into_submodules_2(in, in, in, out, out, di, uo) is det.

split_into_submodules_2(ModuleName, Items0, InParentInterface,
		Items, ModuleList) -->
	{ InInterface0 = no },
	split_into_submodules_3(ModuleName, Items0,
		InParentInterface, InInterface0,
		ThisModuleItems, Items, SubModules),
	{ map__to_assoc_list(SubModules, SubModuleList) },
	{ ModuleList = [ModuleName - ThisModuleItems | SubModuleList] }.

:- pred split_into_submodules_3(module_name, item_list, bool, bool,
			item_list, item_list, map(module_name, item_list),
			io__state, io__state).
:- mode split_into_submodules_3(in, in, in, in, out, out, out, di, uo) is det.

split_into_submodules_3(_ModuleName, [], _, _, [], [], SubModules) -->
	{ map__init(SubModules) }.
split_into_submodules_3(ModuleName, [Item | Items1],
		InParentInterface, InInterface0, 
		ThisModuleItems, OtherItems, SubModules) -->
	(
		%
		% check for a `module' declaration, which signals
		% the start of a nested module
		%
		{ Item = module_defn(VarSet, module(SubModuleName)) - Context }
	->
		%
		% parse in the items for the nested submodule
		%
		split_into_submodules_2(SubModuleName, Items1, InInterface0,
			Items2, SubModules0),
		%
		% parse in the remaining items for this module
		%
		split_into_submodules_3(ModuleName, Items2, InParentInterface,
			InInterface0, ThisModuleItems0, Items3, SubModules1),

		%
		% combine the sub-module declarations from the previous two
		% steps
		%
		{ list__foldl(add_submodule, SubModules0, SubModules1,
			SubModules) },
		%
		% replace the nested submodule with an `include_module'
		% declaration
		%
		{ IncludeSubMod = module_defn(VarSet,
			include_module([SubModuleName])) - Context },
		{ ThisModuleItems = [IncludeSubMod | ThisModuleItems0] },
		{ OtherItems = Items3 }
	;
		%
		% check for a matching `end_module' declaration
		%
		{ Item = module_defn(_VarSet, end_module(ModuleName)) - _ }
	->
		%
		% if so, thats the end of this module
		%
		{ ThisModuleItems = [] },
		{ OtherItems = Items1 },
		{ map__init(SubModules) }
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
		(
			{ Item = module_defn(_, interface) - _Context }
		->
			{ InInterface1 = yes }
		;
			{ Item = module_defn(_, implementation) - Context }
		->
			( { InParentInterface = yes } ->
				report_error_implementation_in_interface(
					ModuleName, Context)
			;
				[]
			),
			{ InInterface1 = no }
		;
			{ InInterface1 = InInterface0 }
		),
		%
		% parse the remaining items for this module,
		%
		split_into_submodules_3(ModuleName, Items1,
			InParentInterface, InInterface1,
			ThisModuleItems0, Items2, SubModules),
		%
		% put the current item back onto the
		% front of the item list for this module
		%
		{ ThisModuleItems = [Item | ThisModuleItems0] },
		{ OtherItems = Items2 }
	).

:- pred add_submodule(pair(module_name, item_list),
			submodule_map, submodule_map).
:- mode add_submodule(in, in, out) is det.

add_submodule(ModuleName - ModuleItemList, SubModules0, SubModules) :-
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
	( map__search(SubModules0, ModuleName, ItemList0) ->
		list__append(ModuleItemList, ItemList0, ItemList),
		map__det_update(SubModules0, ModuleName, ItemList, SubModules)
	;
		map__det_insert(SubModules0, ModuleName, ModuleItemList,
								SubModules)
	).

:- pred report_error_implementation_in_interface(module_name, prog_context,
		io__state, io__state).
:- mode report_error_implementation_in_interface(in, in, di, uo) is det.

report_error_implementation_in_interface(ModuleName, Context) -->
	{ ModuleName = qualified(ParentModule0, ChildModule0) ->
		ParentModule = ParentModule0,
		ChildModule = ChildModule0
	;
		error("report_error_implementation_in_interface")
	},
	prog_out__write_context(Context),
	io__write_string("In interface for module `"),
	prog_out__write_sym_name(ParentModule),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  in definition of sub-module `"),
	io__write_string(ChildModule),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string(
		"  error: `:- implementation.' declaration for sub-module\n"),
	prog_out__write_context(Context),
	io__write_string(
		"  occurs in interface section of parent module.\n"),
	io__set_exit_status(1).

:- pred report_duplicate_modules(set(module_name), item_list,
		io__state, io__state).
:- mode report_duplicate_modules(in, in, di, uo) is det.

report_duplicate_modules(Duplicates, Items) -->
	{ IsDuplicateError = (pred(SubModuleName - Context::out) is nondet :-
		list__member(Item, Items),
		Item = module_defn(_VarSet, ModuleDefn) - Context,
		( ModuleDefn = module(SubModuleName)
		; ModuleDefn = include_module(SubModuleNames),
		  list__member(SubModuleName, SubModuleNames)
		),
		set__member(SubModuleName, Duplicates)
	  ) },
	{ solutions(IsDuplicateError, DuplicateErrors) },
	list__foldl(report_error_duplicate_module_decl, DuplicateErrors).

:- pred report_error_duplicate_module_decl(pair(module_name, prog_context),
		io__state, io__state).
:- mode report_error_duplicate_module_decl(in, di, uo) is det.

report_error_duplicate_module_decl(ModuleName - Context) -->
	{ ModuleName = qualified(ParentModule0, ChildModule0) ->
		ParentModule = ParentModule0,
		ChildModule = ChildModule0
	;
		error("report_error_duplicate_module_decl")
	},
	% The error message should look this this:
	% foo.m:123: In module `foo':
	% foo.m:123:   error: sub-module `bar' declared as both
	% foo.m:123:   a separate sub-module and a nested sub-module.
	prog_out__write_context(Context),
	io__write_string("In module `"),
	prog_out__write_sym_name(ParentModule),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  error: sub-module `"),
	io__write_string(ChildModule),
	io__write_string("' declared as both\n"),
	prog_out__write_context(Context),
	io__write_string("  a separate sub-module and a nested sub-module.\n"),
	io__set_exit_status(1).

	% Given a module (well, a list of items), extract the interface
	% part of that module, i.e. all the items between `:- interface'
	% and `:- implementation'.
	% The bodies of instance definitions are removed because
	% the instance methods have not yet been module qualified.
:- pred get_interface(item_list, item_list).
:- mode get_interface(in, out) is det.

get_interface(Items0, Items) :-
	get_interface_2(Items0, no, [], RevItems),
	list__reverse(RevItems, Items).

:- pred get_interface_2(item_list, bool, item_list, item_list).
:- mode get_interface_2(in, in, in, out) is det.

get_interface_2([], _, Items, Items).
get_interface_2([Item - Context | Rest], InInterface0,
				Items0, Items) :-
	( Item = module_defn(_, interface) ->
		Items1 = Items0,
		InInterface1 = yes,
		Continue = yes
	; 
		Item = module_defn(_, Defn),
		( Defn = imported(_)
		; Defn = used(_)
		)
	->
		% Items after here are not part of this module.
		Items1 = Items0,
		InInterface1 = no,
		Continue = no
	;
		Item = module_defn(_, implementation) 
	->
		Items1 = Items0,
		InInterface1 = no,
		Continue = yes
	;
		( InInterface0 = yes ->
			( make_abstract_instance(Item, Item1) ->
				ItemToWrite = Item1
			;
				ItemToWrite = Item
			),
			Items1 = [ItemToWrite - Context | Items0]
		;
			Items1 = Items0
		),
		InInterface1 = InInterface0,
		Continue = yes
	),
	( Continue = yes ->
		get_interface_2(Rest, InInterface1, Items1, Items)
	;
		Items = Items1
	).

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

:- type short_interface_kind
	--->	int2	% the qualified short interface, for the .int2 file
	;	int3.	% the unqualified short interface, for the .int3 file

:- pred get_short_interface(item_list, short_interface_kind, item_list).
:- mode get_short_interface(in, in, out) is det.

get_short_interface(Items0, Kind, Items) :-
	get_short_interface_2(Items0, [], [], no, Kind,
			RevItems, RevImports, NeedsImports),
	list__reverse(RevItems, Items1),
	( NeedsImports = yes ->
		list__reverse(RevImports, Imports1),
		list__append(Imports1, Items1, Items)
	;
		Items = Items1
	).

:- pred get_short_interface_2(item_list, item_list, item_list, bool,
		short_interface_kind, item_list, item_list, bool).
:- mode get_short_interface_2(in, in, in, in, in, out, out, out) is det.

get_short_interface_2([], Items, Imports, NeedsImports, _Kind,
			Items, Imports, NeedsImports).
get_short_interface_2([ItemAndContext | Rest], Items0, Imports0, NeedsImports0,
			Kind, Items, Imports, NeedsImports) :-
	ItemAndContext = Item0 - Context,
	( Item0 = module_defn(_, import(_)) ->
		Items1 = Items0,
		Imports1 = [ItemAndContext | Imports0],
		NeedsImports1 = NeedsImports0
	; Item0 = module_defn(_, use(_)) ->
		Items1 = Items0,
		Imports1 = [ItemAndContext | Imports0],
		NeedsImports1 = NeedsImports0
	; make_abstract_defn(Item0, Kind, Item1) ->
		Imports1 = Imports0,
		Items1 = [Item1 - Context | Items0],
		NeedsImports1 = NeedsImports0
	; include_in_short_interface(Item0) ->
		Imports1 = Imports0,
		Items1 = [ItemAndContext | Items0],
		NeedsImports1 = yes
	;
		Items1 = Items0,
		Imports1 = Imports0,
		NeedsImports1 = NeedsImports0
	),
	get_short_interface_2(Rest, Items1, Imports1, NeedsImports1, Kind,
				Items, Imports, NeedsImports).

:- pred include_in_short_interface(item).
:- mode include_in_short_interface(in) is semidet.

include_in_short_interface(type_defn(_, _, _, _, _)).
include_in_short_interface(inst_defn(_, _, _, _, _)).
include_in_short_interface(mode_defn(_, _, _, _, _)).
include_in_short_interface(module_defn(_, _)).

:- pred make_abstract_defn(item, short_interface_kind, item).
:- mode make_abstract_defn(in, in, out) is semidet.

make_abstract_defn(type_defn(VarSet, Name, Args, TypeDefn, Cond),
		ShortInterfaceKind,
		type_defn(VarSet, Name, Args, abstract_type, Cond)) :-
	(
		TypeDefn = du_type(_, _)
	;
		TypeDefn = abstract_type
	;
		TypeDefn = eqv_type(_),
		% For the `.int2' files, we need the full definitions of
		% equivalence types. They are needed to ensure that
		% non-abstract equivalence types always get fully expanded
		% before code generation, even in modules that only indirectly
		% import the definition of the equivalence type.
		% But the full definitions are not needed for the `.int3'
		% files. So we convert equivalence types into abstract
		% types only for the `.int3' files.
		ShortInterfaceKind = int3
	).
make_abstract_defn(typeclass(A, B, C, _, E), _,
		typeclass(A, B, C, abstract, E)).


	% All instance declarations must be written
	% to `.int' files as abstract instance
	% declarations because the method names
	% have not yet been module qualified.
	% This could cause the wrong predicate to be
	% used if calls to the method are specialized.
:- pred make_abstract_instance(item, item).
:- mode make_abstract_instance(in, out) is semidet.

make_abstract_instance(Item0, Item) :-
	Item0 = instance(Constraints, Class, ClassTypes, Body0, TVarSet,
		ModName),
	Body0 = concrete(_),
	Body = abstract,
	Item = instance(Constraints, Class, ClassTypes, Body, TVarSet,
		ModName).

%-----------------------------------------------------------------------------%

:- pred maybe_record_timestamp(module_name, string, need_qualifier,
	maybe(timestamp), module_imports, module_imports,
	io__state, io__state).
:- mode maybe_record_timestamp(in, in, in, in, in, out, di, uo) is det.

maybe_record_timestamp(ModuleName, Suffix, NeedQualifier,
		MaybeTimestamp, Module0, Module) -->
	(
		{ Module0 ^ maybe_timestamps = yes(Timestamps0) }
	->
		(
			{ MaybeTimestamp = yes(Timestamp) },
			{ TimestampInfo = module_timestamp(Suffix,
						Timestamp, NeedQualifier) },
			{ map__set(Timestamps0, ModuleName,
					TimestampInfo, Timestamps) },
			{ Module = Module0 ^ maybe_timestamps :=
					yes(Timestamps) }
		;
			{ MaybeTimestamp = no },
			{ Module = Module0 }
		)
	;
		{ Module = Module0 }
	).

:- pred report_modification_time_warning(file_name, io__error,
		io__state, io__state).
:- mode report_modification_time_warning(in, in, di, uo) is det.

report_modification_time_warning(SourceFileName, Error) -->
	globals__io_set_option(smart_recompilation, bool(no)),
	globals__io_set_option(generate_item_version_numbers, bool(no)),
	globals__io_lookup_bool_option(warn_smart_recompilation, Warn),
	( { Warn = yes } ->
		io__write_string(
			"Warning: cannot find modification time for "),
		io__write_string(SourceFileName),
		io__write_string(":\n"),
		{ io__error_message(Error, Msg) },
		io__write_string("  "),
		io__write_string(Msg),
		io__write_string(".\n"),
		io__write_string("  Smart recompilation will not work.\n"),
		globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
		( { HaltAtWarn = yes } ->
			io__set_exit_status(1)	
		;
			[]
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%
