%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
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

:- module modules.

:- interface.

:- import_module prog_data, prog_io.
:- import_module std_util, bool, list, io.

%-----------------------------------------------------------------------------%

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
	%	Used for creating library names, e.g. `lib<foo>.a'
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
	%	This predicate is used for the names of .c and .o files
	%	for --split-c-files.
	%
:- pred module_name_to_split_c_file_name(module_name, int, string, file_name,
				io__state, io__state).
:- mode module_name_to_split_c_file_name(in, in, in, out, di, uo) is det.

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

	% read_mod(ModuleName, Extension, Descr, Search, Items, Error,
	%		SourceFileName):
	%	Given a module name and a file extension (e.g. `.m',
	%	`.int', or `int2'), read in the list of items in that file.
	%	If Extension is ".m", and ModuleName is a nested module,
	%	then try searching for different filenames:
	%	for modules such as `foo.bar.baz.m' search first for
	%	`foo.bar.baz.m', then `bar.baz.m', then `baz.m'.
	%	If Search is yes, search all directories given by the option
	%	search_directories for the module.
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
:- pred read_mod(module_name, string, string, bool,
		item_list, module_error, file_name, io__state, io__state).
:- mode read_mod(in, in, in, in, out, out, out, di, uo) is det.

	% Similar to read_mod, but doesn't return error messages.
:- pred read_mod_ignore_errors(module_name, string, string, bool,
		item_list, module_error, file_name, io__state, io__state).
:- mode read_mod_ignore_errors(in, in, in, in, out, out, out, di, uo) is det.

	% read_mod_from_file(SourceFileName, Extension, Descr, Search, Items,
	%		Error, ModuleName):
	%	Given a file name and a file extension (e.g. `.m',
	%	`.int', or `int2'), read in the list of items in that file.
	%	If Search is yes, search all directories given by the option
	%	search_directories for the module.
	%	Return the module name (as determined by the
	%	`:- module' declaration, if any).
	%
	%	N.B.  This reads a module given the file name.
	%	If you want to read a module given the module name,
	%	use `read_mod'.
	%
:- pred read_mod_from_file(file_name, string, string, bool,
		item_list, module_error, module_name, io__state, io__state).
:- mode read_mod_from_file(in, in, in, in, out, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%

	% make_private_interface(SourceFileName, ModuleName, Items):
	%	Given a source file name and module name,
	%	and the list of items in that module,
	%	output the private (`.int0') interface file for the module.
	%	(The private interface contains all the declarations in
	%	the module, including those in the `implementation'
	%	section; it is used when compiling sub-modules.)
	%
:- pred make_private_interface(file_name, module_name, item_list,
				io__state, io__state).
:- mode make_private_interface(in, in, in, di, uo) is det.

	% make_interface(SourceFileName, ModuleName, Items):
	%	Given a source file name and module name,
	%	and the list of items in that module,
	%	output the long (`.int') and short (`.int2') interface files
	%	for the module.
	%
:- pred make_interface(file_name, module_name, item_list, io__state, io__state).
:- mode make_interface(in, in, in, di, uo) is det.

	% 	Output the unqualified short interface file to <module>.int3.
	%
:- pred make_short_interface(module_name, item_list, io__state, io__state).
:- mode make_short_interface(in, in, di, uo) is det.

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
		file_name,	    % The source file
		module_name,	    % The module (or sub-module) that we
				    % are compiling.
		list(module_name),  % The list of ancestor modules it inherits
		list(module_name),  % The list of modules it directly imports
				    % in the interface
				    % (imports via ancestors count as direct)
		list(module_name),  % The list of modules it directly imports
				    % in the implementation.
		list(module_name),  % The list of modules it indirectly imports
		list(module_name),  % The list of its public children,
				    % i.e. child modules that it includes
				    % in the interface section.
		list(string),  	    % The list of filenames for fact tables
				    % in this module.
		item_list,	% The contents of the module and its imports
		module_error	% Whether an error has been encountered
				% when reading in this module.
	).

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

%-----------------------------------------------------------------------------%

	% Given a module (well, a list of items), split it into
	% its constituent sub-modules, in top-down order.
	% Report an error if the `implementation' section of a sub-module
	% is contained inside the `interface' section of its parent module.

:- type module_list == list(pair(module_name, item_list)).

:- pred split_into_submodules(module_name, item_list, module_list,
					io__state, io__state).
:- mode split_into_submodules(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

	% grab_imported_modules(SourceFileName, ModuleName,
	%		Items, Module, Error)
	%	Given a source file name and module name,
	%	and the list of items in that module,
	%	read in the private interface files for all the parent modules,
	%	the long interface files for all the imported modules,
	%	and the short interface files for all the indirectly imported
	%	modules, and return a `module_imports' structure containing the
	%	relevant information.
	%
:- pred grab_imported_modules(file_name, module_name, item_list, module_imports,
			module_error, io__state, io__state).
:- mode grab_imported_modules(in, in, in, out, out, di, uo) is det.

	% grab_unqual_imported_modules(SourceFileName, ModuleName,
	%		Items, Module, Error):
	%	Similar to grab_imported_modules, but only reads in
	%	the unqualified short interfaces (.int3s),
	%	and the .int0 files for parent modules,
	%	instead of reading the long interfaces and
	%	qualified short interfaces (.int and int2s).
	%	Does not set the `PublicChildren' or `FactDeps'
	%	fields of the module_imports structure.

:- pred grab_unqual_imported_modules(file_name, module_name, item_list,
			module_imports, module_error, io__state, io__state).
:- mode grab_unqual_imported_modules(in, in, in, out, out, di, uo) is det.

	% process_module_long_interfaces(Imports, Ext, IndirectImports0,
	%			IndirectImports, Module0, Module):
	%  	Read the long interfaces for modules in Imports
	%	(unless they've already been read in)
	%	from files with filename extension Ext,
	%	and append any imports/uses in those modules to the
	%	IndirectImports list.
	%
:- pred process_module_long_interfaces(list(module_name), string,
		list(module_name), list(module_name),
		module_imports, module_imports,
		io__state, io__state).
:- mode process_module_long_interfaces(in, in, in, out, in, out, di, uo) is det.

	% process_module_indirect_imports(IndirectImports, Ext,
	%			Module0, Module):
	%  	Read the short interfaces for modules in IndirectImports
	%	(unless they've already been read in) and any
	%	modules that those modules import (transitively),
	%	from files with filename extension Ext.
	%	Put them all in a `:- used.' section.
	%
:- pred process_module_indirect_imports(list(module_name), string,
		module_imports, module_imports, io__state, io__state).
:- mode process_module_indirect_imports(in, in, in, out, di, uo)
		is det.

	% process_module_short_interfaces_transitively(IndirectImports, Ext,
	%			Module0, Module):
	%  	Read the short interfaces for modules in IndirectImports
	%	(unless they've already been read in) and any
	%	modules that those modules import (transitively).
	%
:- pred process_module_short_interfaces_transitively(list(module_name),
		string, module_imports, module_imports, io__state, io__state).
:- mode process_module_short_interfaces_transitively(in, in, in, out, di, uo)
		is det.

	% process_module_short_interfaces(Modules, Ext,
	%		IndirectImports0, IndirectImports, Module0, Module):
	%  	Read the short interfaces for modules in Modules
	%	(unless they've already been read in).
	%	Append the modules imported by Modules to
	%	IndirectImports0 to give IndirectImports.
	%
:- pred process_module_short_interfaces(list(module_name), string,
		list(module_name), list(module_name),
		module_imports, module_imports, io__state, io__state).
:- mode process_module_short_interfaces(in, in, in, out, in, out, di, uo)
		is det.

%-----------------------------------------------------------------------------%

	% write_dependency_file(Module, MaybeTransOptDeps):
	%	Write out the per-module makefile dependencies (`.d') file
	%	for the specified module.
	%	MaybeTransOptDeps is a list of module names which the
	%	`.trans_opt' file may depend on.  This is set to `no' if the
	%	dependency list is not available.
	%
:- pred write_dependency_file(module_imports, maybe(list(module_name)),
				io__state, io__state).
:- mode write_dependency_file(in, in, di, uo) is det.

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
	%	Get the list of modules that a list of items depends on.
	%	ImportDeps is the list of modules imported using
	% 	`:- import_module', UseDeps is the list of modules imported
	%	using `:- use_module'.
	%	N.B. Typically you also need to consider the module's
	%	parent modules (see get_ancestors/2) and possibly
	%	also the module's child modules (see get_children/2).
	%
:- pred get_dependencies(item_list, list(module_name), list(module_name)).
:- mode get_dependencies(in, out, out) is det.

	% get_ancestors(ModuleName, ParentDeps):
	%	ParentDeps is the list of ancestor modules for this
	%	module, oldest first; e.g. if the ModuleName is 
	%	`foo:bar:baz', then ParentDeps would be [`foo', `foo:bar'].
	%
:- pred get_ancestors(module_name, list(module_name)).
:- mode get_ancestors(in, out) is det.

	% get_partial_qualifiers(ModuleName, PartialQualifiers):
	%	PartialQualifiers is the list of partial module
	%	qualifiers for ModuleName; e.g. if the ModuleName is 
	%	`foo:bar:baz', then ParentDeps would be [`bar:baz', `baz']).
	%
:- pred get_partial_qualifiers(module_name, list(module_name)).
:- mode get_partial_qualifiers(in, out) is det.

%-----------------------------------------------------------------------------%

	% touch_interface_datestamp(ModuleName, Ext).
	%
	% Touch the datestamp file `ModuleName.Ext'. Datestamp files
	% are used to record when each of the interface files was last
	% updated.

:- pred touch_interface_datestamp(module_name, string, io__state, io__state).
:- mode touch_interface_datestamp(in, in, di, uo) is det.

	% update_interface(FileName)
	%
	% Call the shell script mercury_update_interface to update the
	% interface file FileName if it has changed.

:- pred update_interface(string, io__state, io__state).
:- mode update_interface(in, di, uo) is det.

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
:- import_module llds_out, passes_aux, prog_out, prog_util, mercury_to_mercury.
:- import_module prog_io_util, globals, options, intermod, module_qual.

:- import_module string, set, map, term, varset, dir, library.
:- import_module assoc_list, relation, char, require.

%-----------------------------------------------------------------------------%

	% It is not really clear what the naming convention
	% should be.  Currently we assume that the module
	% `foo:bar:baz' will be in files `foo.bar.baz.{m,int,etc.}'.
	% It would be nice to allow a more flexible mapping.

module_name_to_file_name(ModuleName, Ext, MkDir, FileName) -->
	{ prog_out__sym_name_to_string(ModuleName, ".", BaseFileName) },
	{ string__append_list([BaseFileName, Ext], BaseName) },
	choose_file_name(ModuleName, BaseName, Ext, MkDir, FileName).

module_name_to_lib_file_name(Prefix, ModuleName, Ext, MkDir, FileName) -->
	{ prog_out__sym_name_to_string(ModuleName, ".", BaseFileName) },
	{ string__append_list([Prefix, BaseFileName, Ext], BaseName) },
	choose_file_name(ModuleName, BaseName, Ext, MkDir, FileName).

fact_table_file_name(ModuleName, FactTableFileName, Ext, FileName) -->
	{ string__append(FactTableFileName, Ext, BaseName) },
	choose_file_name(ModuleName, BaseName, Ext, no, FileName).

:- pred choose_file_name(module_name, string, string, bool, file_name,
			io__state, io__state).
:- mode choose_file_name(in, in, in, in, out, di, uo) is det.

choose_file_name(_ModuleName, BaseName, Ext, MkDir, FileName) -->
	globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
	( { UseSubdirs = no } ->
		{ FileName = BaseName }
	;
		%
		% the source files, the final executables,
		% library files (including .init files)
		% output files intended for use by the user,
		% and phony Mmake targets names go in the current directory
		%
		{ Ext = ".m"
		% executable files
		; Ext = ""
		; Ext = ".split"
		; Ext = ".nu"
		; Ext = ".nu.debug"
		; Ext = ".sicstus"
		; Ext = ".sicstus.debug"
		% library files
		; Ext = ".a"
		; Ext = ".so"
		; Ext = ".$(EXT_FOR_SHARED_LIB)"
		; Ext = ".split.a"
		; Ext = ".split.so"
		; Ext = ".split.$(EXT_FOR_SHARED_LIB)"
		; Ext = ".init"
		% output files intended for use by the user
		; Ext = ".h"
		; Ext = ".err"
		; Ext = ".ugly"
		; Ext = ".hlds_dump"
		; Ext = ".dependency_graph"
		; Ext = ".order"
		% Mmake targets
		; Ext = ".clean"
		; Ext = ".clean_nu"
		; Ext = ".clean_sicstus"
		; Ext = ".change_clean"
		; Ext = ".realclean"
		; Ext = ".depend"
		; Ext = ".check"
		; Ext = ".ints"
		; Ext = ".int3s"
		; Ext = ".opts"
		; Ext = ".trans_opts"
		}
	->
		{ FileName = BaseName }
	;
		%
		% we need to handle a few cases specially
		%
		{
			Ext = ".dir/*.o"
		->
			SubDirName = "dirs"
		;
			% .o and .pic_o files need to go in the
			% same directory, so that using
			% .$(EXT_FOR_PIC_OBJECTS) will work.
			( Ext = ".o"
			; Ext = ".pic_o"
			; Ext = "$(EXT_FOR_PIC_OBJECTS)"
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
		{ string__append_list([DirName, Slash, BaseName], FileName) }
	).

module_name_to_split_c_file_name(ModuleName, Num, Ext, FileName) -->
	module_name_to_file_name(ModuleName, ".dir", no, DirName),
	{ unqualify_name(ModuleName, BaseFileName) },
	{ dir__directory_separator(Slash) },
	{ string__format("%s%c%s_%03d%s",
		[s(DirName), c(Slash), s(BaseFileName), i(Num), s(Ext)],
		FileName) }.

file_name_to_module_name(FileName, ModuleName) :-
	string_to_sym_name(FileName, ".", ModuleName).

module_name_to_make_var_name(ModuleName, MakeVarName) :-
	prog_out__sym_name_to_string(ModuleName, ".", MakeVarName).

:- pred make_directory(string, io__state, io__state).
:- mode make_directory(in, di, uo) is det.

make_directory(DirName) -->
	( { dir__this_directory(DirName) } ->
		[]
	;
		{ string__format("[ -d %s ] || mkdir -p %s",
			[s(DirName), s(DirName)], Command) },
		io__call_system(Command, _Result)
	).

%-----------------------------------------------------------------------------%

	% Read in the .int3 files that the current module depends on,
	% and use these to qualify all the declarations
	% as much as possible. Then write out the .int0 file.
make_private_interface(SourceFileName, ModuleName, Items0) -->
	grab_unqual_imported_modules(SourceFileName, ModuleName, Items0,
		Module, Error),
		%
		% Check whether we succeeded
		%
	( { Error = yes } ->
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
			{ strip_clauses_from_interface(Items3, Items) },
			write_interface_file(ModuleName, ".int0", Items),
			touch_interface_datestamp(ModuleName, ".date0")
		)
	).

	% Read in the .int3 files that the current module depends on,
	% and use these to qualify all items in the interface as much as
	% possible. Then write out the .int and .int2 files.
make_interface(SourceFileName, ModuleName, Items0) -->
	{ get_interface(Items0, no, InterfaceItems0) },
		% 
		% Get the .int3 files for imported modules
		%
	grab_unqual_imported_modules(SourceFileName, ModuleName,
		InterfaceItems0, Module0, Error),

		%
		% Check whether we succeeded
		%
	{ module_imports_get_items(Module0, InterfaceItems1) },
	( { Error = yes } ->
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
			% check for some warnings, and then 
			% write out the `.int' and `int2' files
			% and touch the `.date' file.
			%
			{ strip_imported_items(InterfaceItems2, [],
							InterfaceItems3) },
			check_for_clauses_in_interface(InterfaceItems3,
							InterfaceItems),
			check_int_for_no_exports(InterfaceItems, ModuleName),
			write_interface_file(ModuleName, ".int",
							InterfaceItems),
			{ get_short_interface(InterfaceItems,
						ShortInterfaceItems) },
			write_interface_file(ModuleName, ".int2",
						ShortInterfaceItems),
			touch_interface_datestamp(ModuleName, ".date")
		)
	).

	% This qualifies everything as much as it can given the
	% information in the current module and writes out the .int3 file.
make_short_interface(ModuleName, Items0) -->
	{ get_interface(Items0, no, InterfaceItems0) },
	check_for_clauses_in_interface(InterfaceItems0, InterfaceItems),
	{ get_short_interface(InterfaceItems, ShortInterfaceItems0) },
	module_qual__module_qualify_items(ShortInterfaceItems0,
			ShortInterfaceItems, ModuleName, no, _, _, _, _),
	write_interface_file(ModuleName, ".int3", ShortInterfaceItems),
	touch_interface_datestamp(ModuleName, ".date3").

%-----------------------------------------------------------------------------%

:- pred strip_imported_items(item_list::in, item_list::in,
						item_list::out) is det.

strip_imported_items([], Items0, Items) :-
	list__reverse(Items0, Items). 
strip_imported_items([Item - Context | Rest], Items0, Items) :-
	( Item = module_defn(_, imported) ->
		list__reverse(Items0, Items)
	; Item = module_defn(_, used) ->
		list__reverse(Items0, Items)
	;
		strip_imported_items(Rest, [Item - Context | Items0], Items)
	).

:- pred check_for_clauses_in_interface(item_list, item_list,
					io__state, io__state).
:- mode check_for_clauses_in_interface(in, out, di, uo) is det.

check_for_clauses_in_interface([], []) --> [].
check_for_clauses_in_interface([ItemAndContext0 | Items0], Items) -->
	{ ItemAndContext0 = Item0 - Context },
	(
		( { Item0 = pred_clause(_,_,_,_) }
		; { Item0 = func_clause(_,_,_,_,_) }
		)
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
		( Item0 = pred_clause(_,_,_,_)
		; Item0 = func_clause(_,_,_,_,_)
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
% `termination_info' and `check_termination' declarations
% are supposed to go in the interface,
% but all other pragma declarations are implementation
% details only, and should go in the implementation.

% XXX we should allow c_header_code;
% but if we do allow it, we should put it in the generated
% header file, which currently we don't.

pragma_allowed_in_interface(c_header_code(_), no).
pragma_allowed_in_interface(c_code(_), no).
pragma_allowed_in_interface(c_code(_, _, _, _, _, _), no).
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
pragma_allowed_in_interface(unused_args(_, _, _, _, _), no).
pragma_allowed_in_interface(termination_info(_, _, _, _, _), yes).
pragma_allowed_in_interface(terminates(_, _), yes).
pragma_allowed_in_interface(does_not_terminate(_, _), yes).
pragma_allowed_in_interface(check_termination(_, _), yes).

check_for_no_exports(Items, ModuleName) -->
	globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning),
	( { ExportWarning = no } ->
		[]
	;
		{ get_interface(Items, no, InterfaceItems) },
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
		{ Item = nothing
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
			io__stderr_stream(StdErr),
			io__write_strings(StdErr, [ "\t\t",
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

:- pred write_interface_file(module_name, string, item_list, io__state, io__state).
:- mode write_interface_file(in, in, in, di, uo) is det.

write_interface_file(ModuleName, Suffix, InterfaceItems) -->

		% create (e.g.) `foo.int.tmp'

	{ string__append(Suffix, ".tmp", TmpSuffix) },
	module_name_to_file_name(ModuleName, Suffix, yes, OutputFileName),
	module_name_to_file_name(ModuleName, TmpSuffix, no, TmpOutputFileName),

		% we need to add a `:- interface' declaration at the start
		% of the item list
	{ varset__init(VarSet) },
	{ term__context_init(Context) },
	{ InterfaceDeclaration = module_defn(VarSet, interface) - Context },
	{ InterfaceItems1 = [InterfaceDeclaration | InterfaceItems] },

	convert_to_mercury(ModuleName, TmpOutputFileName, InterfaceItems1),
	update_interface(OutputFileName).

		% invoke the shell script `mercury_update_interface'
		% to update <Module>.int from <Module>.int.tmp if
		% necessary

update_interface(OutputFileName) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Updating interface:\n"),
	( { Verbose = yes } ->
		{ Command = "mercury_update_interface -v " }
	;
		{ Command = "mercury_update_interface " }
	),
	{ string__append(Command, OutputFileName, ShellCommand) },
	invoke_system_command(ShellCommand, Succeeded),
	( { Succeeded = no } ->
		report_error("problem updating interface files.")
	;
		[]
	).

%-----------------------------------------------------------------------------%

touch_interface_datestamp(ModuleName, Ext) -->
	module_name_to_file_name(ModuleName, Ext, yes, OutputFileName),

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

grab_imported_modules(SourceFileName, ModuleName, Items0, Module, Error) -->
		%
		% Find out which modules this one depends on
		%
	{ get_ancestors(ModuleName, AncestorModules) },
	{ get_dependencies(Items0, ImportedModules0, UsedModules0) },

	warn_if_import_self_or_ancestor(ModuleName, AncestorModules,
		ImportedModules0, UsedModules0),

	warn_if_duplicate_use_import_decls(ModuleName,
		ImportedModules0, ImportedModules1,
		UsedModules0, UsedModules1),

	{ get_fact_table_dependencies(Items0, FactDeps) },
	{ get_interface(Items0, no, InterfaceItems) },
	{ get_children(InterfaceItems, PublicChildren) },
	{ init_module_imports(SourceFileName, ModuleName, Items0,
		PublicChildren, FactDeps, Module0) },

		% If this module has any seperately-compiled sub-modules,
		% then we need to make everything in this module exported.
	{ get_children(Items0, Children) },
	{ Children = [] ->
		Module1 = Module0
	;
		split_clauses_and_decls(Items0, Clauses, Decls),
		make_pseudo_decl(interface, InterfaceDecl),
		make_pseudo_decl(implementation, ImplementationDecl),
		list__append([InterfaceDecl | Decls],
			[ImplementationDecl | Clauses], Items1),
		module_imports_set_items(Module0, Items1, Module1)
	},

		% We add a pseudo-declarations `:- imported' at the end
		% of the item list. Uses of the items with declarations 
		% following this do not need module qualifiers.
	{ append_pseudo_decl(Module1, imported, Module2) },

		% Add `builtin' and `private_builtin' to the
		% list of imported modules
	{ add_implicit_imports(ImportedModules1, UsedModules1,
			ImportedModules2, UsedModules2) },

		% Process the ancestor modules
	process_module_private_interfaces(AncestorModules,
		ImportedModules2, ImportedModules, UsedModules2, UsedModules,
		Module2, Module3),

		% Process the modules imported using `import_module'.
	{ IndirectImports0 = [] },
	process_module_long_interfaces(ImportedModules, ".int",
		IndirectImports0, IndirectImports1, Module3, Module4),

		% Process the modules imported using `use_module' 
		% and the short interfaces for indirectly imported
		% modules. The short interfaces are treated as if
		% they are imported using `use_module'.
	{ append_pseudo_decl(Module4, used, Module5) },
	process_module_long_interfaces(UsedModules, ".int",
		IndirectImports1, IndirectImports, Module5, Module6),
	process_module_short_interfaces_transitively(IndirectImports, ".int2",
		Module6, Module),

	{ module_imports_get_error(Module, Error) }.

% grab_unqual_imported_modules:
%	like grab_imported_modules, but gets the `.int3' files
%	instead of the `.int' and `.int2' files.

grab_unqual_imported_modules(SourceFileName, ModuleName, Items0,
		Module, Error) -->
		%
		% Find out which modules this one depends on
		%
	{ get_ancestors(ModuleName, ParentDeps) },
	{ get_dependencies(Items0, ImportDeps0, UseDeps0) },

		%
		% Construct the initial module import structure,
		% and append a `:- imported' decl to the items.
		%
	{ init_module_imports(SourceFileName, ModuleName, Items0, [], [],
		Module0) },
	{ append_pseudo_decl(Module0, imported, Module1) },

		% Add `builtin' and `private_builtin' to the imported modules.
	{ add_implicit_imports(ImportDeps0, UseDeps0, ImportDeps1, UseDeps1) },

		%
		% Get the .int3s and .int0s that the current module depends on.
		%

		% first the .int0s for parent modules
	process_module_private_interfaces(ParentDeps,
			ImportDeps1, ImportDeps, UseDeps1, UseDeps,
			Module1, Module2),

		% then the .int3s for `:- import'-ed modules
	process_module_long_interfaces(ImportDeps, ".int3",
			[], IndirectImportDeps0, Module2, Module3),

		% then (after a `:- used' decl)
		% the .int3s for `:- use'-ed modules
		% and indirectly imported modules
	{ append_pseudo_decl(Module3, used, Module4) },
	process_module_long_interfaces(UseDeps, ".int3",
			IndirectImportDeps0, IndirectImportDeps,
			Module4, Module5),
	process_module_short_interfaces_transitively(
			IndirectImportDeps, ".int3", Module5, Module),

	{ module_imports_get_error(Module, Error) }.

%-----------------------------------------------------------------------------%

:- pred init_module_imports(file_name, module_name, item_list,
			list(module_name), list(string), module_imports).
:- mode init_module_imports(in, in, in, in, in, out) is det.

init_module_imports(SourceFileName, ModuleName, Items, PublicChildren,
			FactDeps, Module) :-
	Module = module_imports(SourceFileName, ModuleName, [], [], [], [],
			PublicChildren, FactDeps, Items, no).

module_imports_get_source_file_name(Module, SourceFileName) :-
	Module = module_imports(SourceFileName, _, _, _, _, _, _, _, _, _).

module_imports_get_module_name(Module, ModuleName) :-
	Module = module_imports(_, ModuleName, _, _, _, _, _, _, _, _).

module_imports_get_impl_deps(Module, ImplDeps) :-
	Module = module_imports(_, _, _, _, ImplDeps, _, _, _, _, _).

module_imports_get_items(Module, Items) :-
	Module = module_imports(_, _, _, _, _, _, _, _, Items, _).

module_imports_set_items(Module0, Items, Module) :-
	Module0 = module_imports(A, B, C, D, E, F, G, H, _, J),
	Module = module_imports(A, B, C, D, E, F, G, H, Items, J).

module_imports_get_error(Module, Error) :-
	Module = module_imports(_, _, _, _, _, _, _, _, _, Error).

module_imports_set_error(Module0, Error, Module) :-
	Module0 = module_imports(A, B, C, D, E, F, G, H, I, _),
	Module = module_imports(A, B, C, D, E, F, G, H, I, Error).

module_imports_set_int_deps(Module0, IntDeps, Module) :-
	Module0 = module_imports(A, B, C, _, E, F, G, H, I, J),
	Module = module_imports(A, B, C, IntDeps, E, F, G, H, I, J).

module_imports_set_impl_deps(Module0, ImplDeps, Module) :-
	Module0 = module_imports(A, B, C, D, _, F, G, H, I, J),
	Module = module_imports(A, B, C, D, ImplDeps, F, G, H, I, J).

module_imports_set_indirect_deps(Module0, IndirectDeps, Module) :-
	Module0 = module_imports(A, B, C, D, E, _, G, H, I, J),
	Module = module_imports(A, B, C, D, E, IndirectDeps, G, H, I, J).

append_pseudo_decl(Module0, PseudoDecl, Module) :-
	Module0 = module_imports(FileName, ModuleName, Ancestors, IntDeps,
			ImplDeps, IndirectDeps, PublicChildren, FactDeps,
			Items0, Error),
	make_pseudo_decl(PseudoDecl, Item),
	list__append(Items0, [Item], Items),
	Module = module_imports(FileName, ModuleName, Ancestors, IntDeps,
			ImplDeps, IndirectDeps, PublicChildren, FactDeps,
			Items, Error).

make_pseudo_decl(PseudoDecl, Item) :-
	term__context_init(Context),
	varset__init(Varset),
	Item = module_defn(Varset, PseudoDecl) - Context.

%-----------------------------------------------------------------------------%

:- pred add_implicit_imports(list(module_name), list(module_name),
			list(module_name), list(module_name)).
:- mode add_implicit_imports(in, in, out, out) is det.

add_implicit_imports(ImportDeps0, UseDeps0, ImportDeps, UseDeps) :-
	mercury_public_builtin_module(MercuryPublicBuiltin),
	mercury_private_builtin_module(MercuryPrivateBuiltin),
	ImportDeps = [MercuryPublicBuiltin | ImportDeps0],
	( MercuryPrivateBuiltin = MercuryPublicBuiltin ->
		UseDeps = UseDeps0
	;
		UseDeps = [MercuryPrivateBuiltin | UseDeps0]
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

write_dependency_file(Module, MaybeTransOptDeps) -->
	{ Module = module_imports(SourceFileName, ModuleName, ParentDeps,
			IntDeps, ImplDeps, IndirectDeps, _InclDeps, FactDeps0,
			_Items, _Error) },
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
					".fact_tables:%=$(os_subdir)%.o)\n\n",
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
					FactDeps, ".o", DepStream),
				io__nl(DepStream)
			)
		;
			[]
		),

		{
			string__remove_suffix(SourceFileName, ".m",
				SourceFileBase)
		->
			string__append(SourceFileBase, ".err", ErrFileName)
		;
			error("modules.m: source file doesn't end in `.m'")
		},
		module_name_to_file_name(ModuleName, ".optdate", no,
					OptDateFileName),
		module_name_to_file_name(ModuleName, ".c", no, CFileName),
		module_name_to_file_name(ModuleName, ".o", no, ObjFileName),
		module_name_to_file_name(ModuleName, ".pic_o", no,
							PicObjFileName),
		io__write_strings(DepStream, ["\n\n",
			OptDateFileName, " ",
			TransOptDateFileName, " ",
			CFileName, " ",
			ErrFileName, " ",
			PicObjFileName, " ",
			ObjFileName, " : ",
			SourceFileName
		] ),
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
		( { Intermod = yes; UseOptFiles = yes } ->
			io__write_strings(DepStream, [
				"\n\n", 
				CFileName, " ",
				TransOptDateFileName, " ",
				ErrFileName, " ", 
				PicObjFileName, " ",
				ObjFileName, " :"
			]),

			% The .c file only depends on the .opt files from 
			% the current directory, so that inter-module
			% optimization works when the .opt files for the 
			% library are unavailable. This is only necessary 
			% because make doesn't allow conditional dependencies.
			% The dependency on the current module's .opt file
			% is to make sure the module gets type-checked without
			% having the definitions of abstract types from other
			% modules.
			globals__io_lookup_bool_option(transitive_optimization,
				TransOpt),
			globals__io_lookup_bool_option(use_trans_opt_files,
				UseTransOpt),

			( { TransOpt = yes ; UseTransOpt = yes } ->
				{ bool__not(UseTransOpt, BuildOptFiles) },
				get_both_opt_deps(BuildOptFiles,
					[ModuleName | LongDeps], IntermodDirs,
					OptDeps, TransOptDeps),
				write_dependencies_list(OptDeps,
					".opt", DepStream),
				io__write_strings(DepStream, [
					"\n\n", 
					CFileName, " ",
					ErrFileName, " ", 
					PicObjFileName, " ", 
					ObjFileName, " :"
				]),
				write_dependencies_list(TransOptDeps,
					".trans_opt", DepStream)
			;
				{ bool__not(UseOptFiles, BuildOptFiles) },
				get_opt_deps(BuildOptFiles,
					[ModuleName | LongDeps],
					IntermodDirs, ".opt", OptDeps),
				write_dependencies_list(OptDeps,
					".opt", DepStream)
			)
		;
			[]
		),

		module_name_to_file_name(ModuleName, ".date", no,
						DateFileName),
		module_name_to_file_name(ModuleName, ".date0", no,
						Date0FileName),
		io__write_strings(DepStream, [
				"\n\n", DateFileName, " ",
				Date0FileName, " : ",
				SourceFileName
		]),
		write_dependencies_list(ParentDeps, ".int0", DepStream),
		write_dependencies_list(LongDeps, ".int3", DepStream),
		write_dependencies_list(ShortDeps, ".int3", DepStream),
			
		module_name_to_file_name(ModuleName, ".dir", no, DirFileName),
		module_name_to_split_c_file_name(ModuleName, 0, ".o",
			SplitCObj0FileName),
		io__write_strings(DepStream, [
			"\n\n",
			SplitCObj0FileName, " : ",
				SourceFileName, "\n",
			"\trm -rf ", DirFileName, "\n",
			"\t$(MCS) $(ALL_GRADEFLAGS) $(ALL_MCSFLAGS) ",
				SourceFileName, "\n"
		]),

		module_name_to_file_name(ModuleName, ".m", no,
			ExpectedSourceFileName),
		( { SourceFileName \= ExpectedSourceFileName } ->
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
			module_name_to_file_name(ModuleName, ".date3", no,
							Date3FileName),
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
				CFileName, " : ", SourceFileName, "\n",
				"\trm -f ", CFileName, "\n",
				"\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
					"$< > ", ErrFileName, " 2>&1\n",
				"ifneq ($(RM_C),:)\n",
				ObjFileName, " : ", SourceFileName, "\n",
				"\t$(MMAKE_MAKE_CMD) $(MFLAGS) ",
					"MC=""$(MC)"" ",
					"ALL_MCFLAGS=""$(ALL_MCFLAGS)"" ",
					"ALL_GRADEFLAGS=""$(ALL_GRADEFLAGS)"" ",
					CFileName, "\n",
				"\t$(MGNUC) $(ALL_GRADEFLAGS) ",
					"$(ALL_MGNUCFLAGS) -c ", CFileName,
					" -o $@\n",
				"\t$(RM_C) ", CFileName, "\n",
				PicObjFileName, " : ", SourceFileName, "\n",
				"\t$(MMAKE_MAKE_CMD) $(MFLAGS) ",
					"MC=""$(MC)"" ",
					"ALL_MCFLAGS=""$(ALL_MCFLAGS)"" ",
					"ALL_GRADEFLAGS=""$(ALL_GRADEFLAGS)"" ",
					CFileName, "\n",
				"\t$(MGNUC) $(ALL_GRADEFLAGS) ",
					"$(ALL_MGNUCFLAGS) $(CFLAGS_FOR_PIC) ",
					"\\\n",
				"\t\t-c ", CFileName, " -o $@\n",
				"endif # RM_C != :\n"
			])
		;
			[]
		),

		io__close_output(DepStream),
		io__rename_file(TmpDependencyFileName, DependencyFileName,
			Result3),
		( { Result3 = error(Error) } ->
			maybe_write_string(Verbose, " failed.\n"),
			maybe_flush_output(Verbose),
			{ io__error_message(Error, ErrorMsg) },
			{ string__append_list(["can't rename file `",
				TmpDependencyFileName, "' as `",
				DependencyFileName, "': ", ErrorMsg],
				Message) },
			report_error(Message)
		;
			maybe_write_string(Verbose, " done.\n")
		)
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
:- pred get_both_opt_deps(bool::in, list(module_name)::in, list(string)::in, 
	list(module_name)::out, list(module_name)::out, 
	io__state::di, io__state::uo) is det.
get_both_opt_deps(_, [], _, [], []) --> [].
get_both_opt_deps(BuildOptFiles, [Dep | Deps], IntermodDirs,
		OptDeps, TransOptDeps) -->
	get_both_opt_deps(BuildOptFiles, Deps, IntermodDirs,
		OptDeps0, TransOptDeps0),
	( { BuildOptFiles = yes } ->
		module_name_to_file_name(Dep, ".m", no, DepName), 
		search_for_file(IntermodDirs, DepName, Result1),
		( { Result1 = yes } ->
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
		( { Result2 = yes } ->
			{ OptDeps = [Dep | OptDeps1] },
			io__seen
		;
			{ OptDeps = OptDeps1 }
		),
		module_name_to_file_name(Dep, ".trans_opt", no, TransOptName), 
		search_for_file(IntermodDirs, TransOptName, Result3),
		( { Result3 = yes } ->
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
:- pred get_opt_deps(bool::in, list(module_name)::in, list(string)::in,
	string::in, list(module_name)::out,
	io__state::di, io__state::uo) is det.
get_opt_deps(_, [], _, _, []) --> [].
get_opt_deps(BuildOptFiles, [Dep | Deps], IntermodDirs, Suffix, OptDeps) -->
	get_opt_deps(BuildOptFiles, Deps, IntermodDirs, Suffix, OptDeps0),
	( { BuildOptFiles = yes } ->
		module_name_to_file_name(Dep, ".m", no, DepName),
		search_for_file(IntermodDirs, DepName, Result1),
		( { Result1 = yes } ->
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
		( { Result2 = yes } ->
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
	read_mod_from_file(FileName, ".m", "Reading file", no,
		Items, Error, ModuleName),
	{ string__append(FileName, ".m", SourceFileName) },
	split_into_submodules(ModuleName, Items, SubModuleList),
	{ list__map(init_dependencies(SourceFileName, Error), SubModuleList,
		ModuleImportsList) },
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
	( { Error = fatal } ->
		{ prog_out__sym_name_to_string(ModuleName, ModuleString) },
		{ string__append_list(["can't read source file for module `",
			ModuleString, "'."], Message) },
		report_error(Message)
	;
		{ module_imports_get_source_file_name(ModuleImports,
			SourceFileName) },
		generate_dependencies_write_dep_file(SourceFileName, ModuleName,
			DepsMap),

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

		generate_dependencies_write_d_files(DepsList,
			IntDepsRel, ImplDepsRel, IndirectDepsRel,
			TransOptDepsOrdering, DepsMap)
	).

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


% generate_dependencies_write_d_files(Modules, IntDepsRel, TransOptOrder,
%	DepsMap, IO0, IO):
%		This predicate writes out the .d files for all the modules
%		in the Modules list.  
%		IntDepsRel gives the interface dependency relation
%		(computed from the DepsMap).
%		TransOptOrder gives the ordering that is used to determine
%		which other modules the .trans_opt files may depend on.
:- pred generate_dependencies_write_d_files(list(deps)::in, 
	deps_rel::in, deps_rel::in, deps_rel::in, list(module_name)::in,
	deps_map::in, io__state::di, io__state::uo) is det.
generate_dependencies_write_d_files([], _, _, _, _, _) --> [].
generate_dependencies_write_d_files([Dep | Deps],
		IntDepsRel, ImplDepsRel, IndirectDepsRel,
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
	( { Error \= fatal } ->
		write_dependency_file(Module, yes(TransOptDeps))
	;
		[]
	),
	generate_dependencies_write_d_files(Deps,
		IntDepsRel, ImplDepsRel, IndirectDepsRel,
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
		{ ModuleImports = module_imports(_, _,
			ParentDeps, IntDeps, ImplDeps, _, InclDeps, _, _, _) },
		{ list__condense(
			[ParentDeps, IntDeps, ImplDeps, InclDeps, Modules],
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
	ModuleImports = module_imports(_FileName, ModuleName,
		ParentDeps, _IntDeps, _ImplDeps,
		_IndirectDeps, _PublicChildren, _FactDeps, _Items, ModuleError),
	( ModuleError \= fatal ->
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
	ModuleImports = module_imports(_FileName, _ModuleName,
		ParentDeps, IntDeps, _ImplDeps, _IndirectDeps, PublicChildren,
		_FactDeps, _Items, _ModuleError),
	AddDep = add_dep(ModuleKey),
	list__foldl(AddDep, ParentDeps, Rel0, Rel1),
	list__foldl(AddDep, IntDeps, Rel1, Rel2),
	list__foldl(AddDep, PublicChildren, Rel2, Rel).

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

%-----------------------------------------------------------------------------%

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


:- pred generate_dep_file(file_name, module_name, deps_map, io__output_stream,
			io__state, io__state).
:- mode generate_dep_file(in, in, in, in, di, uo) is det.

generate_dep_file(SourceFileName, ModuleName, DepsMap, DepStream) -->
	%
	% Some of the targets are based on the source file name
	% rather than on the module name.
	%
	{
		string__remove_suffix(SourceFileName, ".m", SourceFileBase)
	->
		file_name_to_module_name(SourceFileBase, SourceModuleName)
	;
		error("modules.m: source file name doesn't end in `.m'")
	},
	
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
	io__write_string(DepStream, "\n\n"),

	globals__io_lookup_bool_option(assume_gmake, Gmake),
	( { Gmake = yes } ->
		{ string__append(MakeVarName, ".mods", ModsVarName) },
		{ Basis = yes(ModsVarName - "") }
	;
		{ Basis = no }
	),

	{ get_extra_link_objects(Modules, DepsMap, ExtraLinkObjs) },

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".nos = "),
	write_compact_dependencies_list(Modules, "$(nos_subdir)", ".no",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".qls = "),
	write_compact_dependencies_list(Modules, "$(qls_subdir)", ".ql",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".cs = "),
	write_compact_dependencies_list(Modules, "$(cs_subdir)", ".c",
					Basis, DepStream),
	write_file_dependencies_list(ExtraLinkObjs, ".c", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".os = "),
	write_compact_dependencies_list(Modules, "$(os_subdir)", ".o",
					Basis, DepStream),
	write_file_dependencies_list(ExtraLinkObjs, ".o", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".pic_os = "),
	write_compact_dependencies_list(Modules, "$(os_subdir)",
					".$(EXT_FOR_PIC_OBJECTS)",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".dirs = "),
	write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".dir_os = "),
	write_compact_dependencies_list(Modules, "$(dirs_subdir)", ".dir/*.o",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".ss = "),
	write_compact_dependencies_list(Modules, "", ".s", Basis, DepStream),
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
	io__write_string(DepStream, ".ds = "),
	write_compact_dependencies_list(Modules, "$(ds_subdir)", ".d",
					Basis, DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, MakeVarName),
	io__write_string(DepStream, ".hs = "),
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
	io__write_string(DepStream, ".profs = "),
	write_compact_dependencies_list(Modules, "", ".prof",
					Basis, DepStream),
	io__write_string(DepStream, "\n\n"),

	module_name_to_file_name(ModuleName, ".init", yes, InitFileName),
	module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName),
	module_name_to_file_name(ModuleName, "_init.s", no, InitAsmFileName),
	module_name_to_file_name(ModuleName, "_init.o", yes, InitObjFileName),
	module_name_to_file_name(ModuleName, "_init.pic_o", yes,
							InitPicObjFileName),

	module_name_to_file_name(SourceModuleName, "", no, ExeFileName),
	io__write_strings(DepStream, [
		"MLOBJS_DEPS += ", ExeFileName, "\n",
		ExeFileName, " : $(", MakeVarName, ".os) ",
			InitObjFileName, "\n",
		"\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -o ",
			ExeFileName, " ", InitObjFileName, " \\\n",
		"\t	$(", MakeVarName, ".os) $(MLOBJS) $(ALL_MLLIBS)\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".split", yes,
				SplitExeFileName),
	module_name_to_file_name(ModuleName, ".split.a", yes, SplitLibFileName),
	io__write_strings(DepStream, [
		SplitExeFileName, " : ", SplitLibFileName, " ",
			InitObjFileName, "\n",
		"\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -o ",
			SplitExeFileName, " ", InitObjFileName, " \\\n",
		"\t	", SplitLibFileName, " $(ALL_MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		"MLOBJS_DEPS += ", SplitLibFileName, "\n",
		SplitLibFileName, " : $(", MakeVarName, ".dir_os)\n",
		"\trm -f ", SplitLibFileName, "\n",
		"\t$(AR) $(ALL_ARFLAGS) ", SplitLibFileName, " $(MLOBJS)\n",
		"\tfor dir in $(", MakeVarName, ".dirs); do \\\n",
		"\t	$(AR) q ", SplitLibFileName, " $$dir/*.o; \\\n",
		"\tdone\n",
		"\t$(RANLIB) $(ALL_RANLIBFLAGS) ", SplitLibFileName, "\n\n"
	]),

	module_name_to_lib_file_name("lib", ModuleName, "", no, LibTargetName),
	module_name_to_lib_file_name("lib", ModuleName, ".a", yes, LibFileName),
	module_name_to_lib_file_name("lib", ModuleName, ".so", yes,
							SharedLibFileName),
	module_name_to_lib_file_name("lib", ModuleName,
		".$(EXT_FOR_SHARED_LIB)", no, MaybeSharedLibFileName),
	io__write_strings(DepStream, [
		".PHONY : ", LibTargetName, "\n",
		LibTargetName, " : ",
		LibFileName, " ",
		MaybeSharedLibFileName, " \\\n",
		"\t\t$(", MakeVarName, ".ints) ",
		"$(", MakeVarName, ".int3s) ",
		"$(", MakeVarName, ".opts) ",
		InitFileName, "\n\n"
	]),

	io__write_strings(DepStream, [
		"MLPICOBJS_DEPS += ", SharedLibFileName, "\n",
		SharedLibFileName, " : $(", MakeVarName, ".pic_os)\n",
		"\t$(ML) --make-shared-lib $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) ",
			"-o ", SharedLibFileName, " \\\n",
		"\t\t$(", MakeVarName, ".pic_os) $(MLPICOBJS) ",
			"$(ALL_MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		"MLOBJS_DEPS += ", LibFileName, "\n",
		LibFileName, " : $(", MakeVarName, ".os)\n",
		"\trm -f ", LibFileName, "\n",
		"\t$(AR) $(ALL_ARFLAGS) ", LibFileName, " ",
			"$(", MakeVarName, ".os) $(MLOBJS)\n",
		"\t$(RANLIB) $(ALL_RANLIBFLAGS) ", LibFileName, "\n\n"
	]),

	module_name_to_file_name(ModuleName, ".dep", no, DepFileName),
	io__write_strings(DepStream, [
		InitFileName, " : ", DepFileName, "\n",
		"\techo > ", InitFileName, "\n"
	]),
	list__foldl(append_to_init_list(DepStream, InitFileName), Modules),
	io__write_string(DepStream, "\n"),

	io__write_strings(DepStream, [
		InitCFileName, " : ", DepFileName, "\n",
		"\t$(C2INIT) $(ALL_GRADEFLAGS) $(ALL_C2INITFLAGS) $(",
			MakeVarName, ".cs) > ", InitCFileName, "\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".nu", yes, NU_ExeFileName),
	module_name_to_file_name(SourceModuleName, ".nu.debug", yes,
						NU_DebugExeFileName),
	io__write_strings(DepStream, [
		NU_ExeFileName, " : $(", MakeVarName, ".nos)\n",
		"\t$(MNL) $(ALL_MNLFLAGS) -o ", NU_ExeFileName, " ",
			"$(", MakeVarName, ".nos)\n\n",

		NU_DebugExeFileName, " : $(", MakeVarName, ".nos)\n",
		"\t$(MNL) --debug $(ALL_MNLFLAGS) -o ", NU_DebugExeFileName,
			" $(", MakeVarName, ".nos)\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".sicstus", yes,
						SicstusExeFileName),
	module_name_to_file_name(SourceModuleName, ".sicstus.debug", yes,
						SicstusDebugExeFileName),
	io__write_strings(DepStream, [
		SicstusExeFileName, " : $(", MakeVarName, ".qls)\n",
		"\t$(MSL) $(ALL_MSLFLAGS) -o ", SicstusExeFileName, " ",
			"$(", MakeVarName, ".qls)\n\n",

		SicstusDebugExeFileName, " : $(", MakeVarName, ".qls)\n",
		"\t$(MSL) --debug $(ALL_MSLFLAGS) -o ", SicstusDebugExeFileName,
			" $(", MakeVarName, ".qls)\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".check", no,
				CheckTargetName),
	module_name_to_file_name(ModuleName, ".ints", no, IntsTargetName),
	module_name_to_file_name(ModuleName, ".int3s", no, Int3sTargetName),
	module_name_to_file_name(ModuleName, ".opts", no, OptsTargetName),
	module_name_to_file_name(ModuleName, ".trans_opts", no,
						TransOptsTargetName),
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
						".trans_opt_dates)\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".clean", no,
				CleanTargetName),
	io__write_strings(DepStream, [
		"clean : ", CleanTargetName, "\n"
	]),
	io__write_strings(DepStream, [
		".PHONY : ", CleanTargetName, "\n",
		CleanTargetName, " :\n",
		"\t-rm -rf ", MakeVarName, ".dir\n",
		"\t-rm -f $(", MakeVarName, ".cs) ", InitCFileName, "\n",
		"\t-rm -f $(", MakeVarName, ".ss) ", InitAsmFileName, "\n",
		"\t-rm -f $(", MakeVarName, ".os) ", InitObjFileName, "\n",
		"\t-rm -f $(", MakeVarName, ".pic_os) ", InitPicObjFileName,
									"\n",
		"\t-rm -f $(", MakeVarName, ".trans_opt_dates)\n",
		"\t-rm -f $(", MakeVarName, ".trans_opts)\n",
		"\t-rm -f $(", MakeVarName, ".profs)\n",
		"\t-rm -f $(", MakeVarName, ".nos)\n",
		"\t-rm -f $(", MakeVarName, ".qls)\n",
		"\t-rm -f $(", MakeVarName, ".errs)\n"
	]),

	io__write_string(DepStream, "\n"),

	module_name_to_file_name(SourceModuleName, ".change_clean", no,
			ChangeCleanTargetName),
	io__write_strings(DepStream, [
		".PHONY : ", ChangeCleanTargetName, "\n",
		ChangeCleanTargetName, " :\n",
		"\t-rm -f $(", MakeVarName, ".cs) ", InitCFileName, "\n",
		"\t-rm -f $(", MakeVarName, ".ss) ", InitAsmFileName, "\n",
		"\t-rm -f $(", MakeVarName, ".os) ", InitObjFileName, "\n",
		"\t-rm -f $(", MakeVarName, ".pic_os) ", InitPicObjFileName,
									"\n",
		"\t-rm -f $(", MakeVarName, ".hs)\n",
		"\t-rm -f $(", MakeVarName, ".ds)\n",
		"\t-rm -f ",
			ExeFileName, " ",
			SplitExeFileName, " ",
			SplitLibFileName, " ",
			InitFileName, " ",
			LibFileName, " ",
			SharedLibFileName, " ",
			DepFileName, "\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".realclean", no,
			RealCleanTargetName),
	io__write_strings(DepStream, [
		"realclean : ", RealCleanTargetName, "\n"
	]),
	io__write_strings(DepStream, [
		".PHONY : ", RealCleanTargetName, "\n",
		RealCleanTargetName, " : ", CleanTargetName, "\n",
		"\t-rm -f $(", MakeVarName, ".dates)\n",
		"\t-rm -f $(", MakeVarName, ".date0s)\n",
		"\t-rm -f $(", MakeVarName, ".date3s)\n",
		"\t-rm -f $(", MakeVarName, ".optdates)\n",
		"\t-rm -f $(", MakeVarName, ".ints)\n",
		"\t-rm -f $(", MakeVarName, ".int0s)\n",
		"\t-rm -f $(", MakeVarName, ".int3s)\n",
		"\t-rm -f $(", MakeVarName, ".opts)\n",
		"\t-rm -f $(", MakeVarName, ".ds)\n",
		"\t-rm -f $(", MakeVarName, ".hs)\n"
	]),
	module_name_to_file_name(SourceModuleName, ".nu.save", no,
						NU_SaveExeFileName),
	module_name_to_file_name(SourceModuleName, ".nu.debug.save", no,
						NU_DebugSaveExeFileName),
	io__write_strings(DepStream, [
		"\t-rm -f ",
			ExeFileName, " ",
			SplitExeFileName, " ",
			SplitLibFileName, " ",
			InitFileName, " ",
			LibFileName, " ",
			SharedLibFileName, " ",
			NU_ExeFileName, " ",
			NU_SaveExeFileName, " ",
			NU_DebugExeFileName, " ",
			NU_DebugSaveExeFileName, " ",
			SicstusExeFileName, " ",
			SicstusDebugExeFileName, " ",
			DepFileName, "\n\n"
	]),

	module_name_to_file_name(SourceModuleName, ".clean_nu", no,
						CleanNU_TargetName),
	module_name_to_file_name(SourceModuleName, ".clean_sicstus", no,
						CleanSicstusTargetName),
	io__write_strings(DepStream, [
		"clean_nu : ", CleanNU_TargetName, "\n",
		".PHONY : ", CleanNU_TargetName, "\n",
		CleanNU_TargetName, " :\n",
		"\t-rm -f $(", MakeVarName, ".nos)\n\n",

		"clean_sicstus : ", CleanSicstusTargetName, "\n",
		".PHONY : ", CleanSicstusTargetName, "\n",
		CleanSicstusTargetName, " :\n",
		"\t-rm -f $(", MakeVarName, ".qls)\n\n"
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
	{ llds_out__make_init_name(Module, InitFuncName) },
	io__write_strings(DepStream, [
		"\techo ""INIT ", InitFuncName, """ >> ", InitFileName, "\n"
	]).

%-----------------------------------------------------------------------------%
	% get_extra_link_objects(Modules, DepsMap, ExtraLinkObjs) },
	% Find any extra .o files that should be linked into the executable.
	% Currently only looks for fact table object files.
:- pred get_extra_link_objects(list(module_name), deps_map, list(string)).
:- mode get_extra_link_objects(in, in, out) is det.

get_extra_link_objects(Modules, DepsMap, ExtraLinkObjs) :-
	get_extra_link_objects_2(Modules, DepsMap, [], ExtraLinkObjs).

:- pred get_extra_link_objects_2(list(module_name), deps_map, 
		list(string), list(string)).
:- mode get_extra_link_objects_2(in, in, in, out) is det.

get_extra_link_objects_2([], _DepsMap, ExtraLinkObjs, ExtraLinkObjs).
get_extra_link_objects_2([Module | Modules], DepsMap, 
		ExtraLinkObjs0, ExtraLinkObjs) :-
	map__lookup(DepsMap, Module, deps(_, ModuleImports)),
	ModuleImports = module_imports(_, _, _, _, _, _, _, FactDeps, _, _),
	list__append(FactDeps, ExtraLinkObjs0, ExtraLinkObjs1),
	get_extra_link_objects_2(Modules, DepsMap, ExtraLinkObjs1, 
		ExtraLinkObjs).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(module_name), deps_map, list(module_name)).
:- mode select_ok_modules(in, in, out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
	map__lookup(DepsMap, Module, deps(_, ModuleImports)),
	module_imports_get_error(ModuleImports, Error),
	( Error = fatal ->
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

write_compact_dependencies_list(Modules, _Prefix, Suffix, no, DepStream) -->
	write_dependencies_list(Modules, Suffix, DepStream).
write_compact_dependencies_list(_Modules, Prefix, Suffix,
		yes(VarName - OldSuffix), DepStream) -->
	io__write_string(DepStream, "$("),
	io__write_string(DepStream, VarName),
	io__write_string(DepStream, ":%"),
	io__write_string(DepStream, OldSuffix),
	io__write_string(DepStream, "="),
	io__write_string(DepStream, Prefix),
	io__write_string(DepStream, "%"),
	io__write_string(DepStream, Suffix),
	io__write_string(DepStream, ")").

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
	% XXX We could make some effort here to catch the case where a
	% module is defined as both a separate sub-module and also
	% as a nested sub-module.  However, that doesn't seem worthwhile,
	% since not all such cases would arrive here anyway --
	% it would be nice to catch that case but this is not the
	% place to catch it.
	% (Currently for that case we just ignore the file containing the
	% separate sub-module.  Since we don't consider the
	% file containing the separate sub-module to be part of the
	% program's source, there's no duplicate definition, and thus
	% no requirement to report any error message.)
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
		"Getting dependencies for module", Search, Items0, Error,
		FileName0),
	( { Items0 = [], Error = fatal } ->
		read_mod_ignore_errors(ModuleName, ".int", 
		    "Getting dependencies for module interface", Search, 
		    Items, _Error, FileName),
		{ SubModuleList = [ModuleName - Items] }
	;
		{ FileName = FileName0 },
		{ Items = Items0 },
		split_into_submodules(ModuleName, Items, SubModuleList)
	),
	{ list__map(init_dependencies(FileName, Error), SubModuleList,
		ModuleImportsList) }.

:- pred init_dependencies(file_name, module_error,
		pair(module_name, item_list), module_imports).
:- mode init_dependencies(in, in, in, out) is det.

init_dependencies(FileName, Error, ModuleName - Items, ModuleImports) :-
	get_ancestors(ModuleName, ParentDeps),

	get_dependencies(Items, ImplImportDeps0, ImplUseDeps0),
	add_implicit_imports(ImplImportDeps0, ImplUseDeps0,
		ImplImportDeps, ImplUseDeps),
	list__append(ImplImportDeps, ImplUseDeps, ImplementationDeps),

	get_interface(Items, no, InterfaceItems),
	get_dependencies(InterfaceItems, InterfaceImportDeps0,
		InterfaceUseDeps0),
	add_implicit_imports(InterfaceImportDeps0, InterfaceUseDeps0,
		InterfaceImportDeps, InterfaceUseDeps),
	list__append(InterfaceImportDeps, InterfaceUseDeps, 
		InterfaceDeps),

	% we don't fill in the indirect dependencies yet
	IndirectDeps = [],

	get_children(InterfaceItems, IncludeDeps),

	get_fact_table_dependencies(Items, FactTableDeps),

	ModuleImports = module_imports(FileName, ModuleName, ParentDeps,
		InterfaceDeps, ImplementationDeps, IndirectDeps, IncludeDeps,
		FactTableDeps, [], Error).

%-----------------------------------------------------------------------------%

read_mod(ModuleName, Extension, Descr, Search, Items, Error, FileName) -->
	read_mod_2(no, ModuleName, ModuleName, Extension, Descr, Search,
		Items, Error, FileName).

read_mod_ignore_errors(ModuleName, Extension, Descr, Search,
		Items, Error, FileName) -->
	read_mod_2(yes, ModuleName, ModuleName, Extension, Descr, Search,
		Items, Error, FileName).

:- pred read_mod_2(bool, module_name, module_name, string, string,
		bool, item_list, module_error, file_name,
		io__state, io__state).
:- mode read_mod_2(in, in, in, in, in, in, out, out, out, di, uo)
		is det.

read_mod_2(IgnoreErrors, ModuleName, PartialModuleName,
		Extension, Descr, Search, Items, Error, FileName) -->
	module_name_to_file_name(PartialModuleName, Extension, no, FileName0),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, FileName0),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	prog_io__read_module(FileName0, ModuleName, Search,
		Error0, ActualModuleName, Messages, Items0),
	check_module_has_expected_name(FileName0,
		ModuleName, ActualModuleName),
	( { IgnoreErrors = yes } ->
		(
			{ Error0 = fatal },
			{ Items0 = [] }
		->
			maybe_write_string(VeryVerbose, "not found.\n")
		;
			maybe_write_string(VeryVerbose, "done.\n")
		)
	;
		( { Error0 = fatal } ->
			maybe_write_string(VeryVerbose, "fatal error(s).\n"),
			io__set_exit_status(1)
		; { Error0 = yes } ->
			maybe_write_string(VeryVerbose, "parse error(s).\n"),
			io__set_exit_status(1)
		;
			maybe_write_string(VeryVerbose, "successful parse.\n")
		),
		prog_out__write_messages(Messages)
	),
	%
	% if that didn't work, and we're reading in the source (.m)
	% file for a nested module, try again after dropping one 
	% level of module qualifiers.
	%
	(
		{ Error0 = fatal },
		{ Items0 = [] },
		{ Extension = ".m" },
		{ PartialModuleName = qualified(Parent, Child) }
	->
		{ drop_one_qualifier(Parent, Child, PartialModuleName2) },
		read_mod_2(IgnoreErrors, ModuleName, PartialModuleName2,
			Extension, Descr, Search, Items, Error, FileName)
	;
		{ Error = Error0 },
		{ Items = Items0 },
		{ FileName = FileName0 }
	).

read_mod_from_file(FileName, Extension, Descr, Search,
		Items, Error, ModuleName) -->
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
	prog_io__read_module(FullFileName, DefaultModuleName, Search,
		Error, ModuleName, Messages, Items),
	( { Error = fatal } ->
		maybe_write_string(VeryVerbose, "fatal error(s).\n"),
		io__set_exit_status(1)
	; { Error = yes } ->
		maybe_write_string(VeryVerbose, "parse error(s).\n"),
		io__set_exit_status(1)
	;
		maybe_write_string(VeryVerbose, "successful parse.\n")
	),
	prog_out__write_messages(Messages).

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

	% process_module_private_interfaces(Ancestors, DirectImports0,
	%			DirectImports, DirectUses0, DirectUses,
	%			Module0, Module):
	%  	Read the complete private interfaces for modules in Ancestors,
	%	and append any imports/uses in the ancestors to the
	%	corresponding previous lists.

:- pred process_module_private_interfaces(list(module_name),
		list(module_name), list(module_name),
		list(module_name), list(module_name),
		module_imports, module_imports, io__state, io__state).
:- mode process_module_private_interfaces(in, in, out, in, out, in, out,
		di, uo) is det.

process_module_private_interfaces([], DirectImports, DirectImports,
		DirectUses, DirectUses, Module, Module) --> [].
process_module_private_interfaces([Ancestor | Ancestors],
		DirectImports0, DirectImports, DirectUses0, DirectUses,
		Module0, Module) -->
	{ Module0 = module_imports(FileName, ModuleName, ModAncestors0,
				ModInterfaceDeps, ModImplementationDeps,
				ModIndirectDeps, ModPublicChildren,
				ModFactDeps, ModItems0, ModError0) },
	(
		{ Ancestor = ModuleName }
	->
		{ error("modules.m: module is its own ancestor?") }
	;
		{ list__member(Ancestor, ModAncestors0) }
	->
		% we've already read it
		process_module_private_interfaces(Ancestors,
				DirectImports0, DirectImports,
				DirectUses0, DirectUses,
				Module0, Module)
	;
		read_mod(Ancestor, ".int0",
			"Reading private interface for module", yes, 
			PrivateIntItems, PrivateIntError, _AncestorFileName),
		{ strip_off_interface_decl(PrivateIntItems, Items) },
		{ maybe_add_int_error(PrivateIntError, ModError0, ModError) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		( { PrivateIntError = fatal } ->
			{ ModAncestors = ModAncestors0 }
		;
			{ ModAncestors = [Ancestor | ModAncestors0] }
		),
		{ get_dependencies(Items, AncDirectImports, AncDirectUses) },
		{ list__append(DirectImports0, AncDirectImports,
				DirectImports1) },
		{ list__append(DirectUses0, AncDirectUses, DirectUses1) },
		{ list__append(ModItems0, Items, ModItems) },
		{ Module1 = module_imports(FileName, ModuleName, ModAncestors,
				ModInterfaceDeps, ModImplementationDeps,
				ModIndirectDeps, ModPublicChildren,
				ModFactDeps, ModItems, ModError) },
		process_module_private_interfaces(Ancestors, DirectImports1,
				DirectImports, DirectUses1, DirectUses,
				Module1, Module)
	).

%-----------------------------------------------------------------------------%

process_module_long_interfaces([], _Ext, IndirectImports, IndirectImports, 
		Module, Module) --> [].
process_module_long_interfaces([Import | Imports], Ext, IndirectImports0,
		IndirectImports, Module0, Module) -->
	{ Module0 = module_imports(FileName, ModuleName, ModAncestors,
				ModInterfaceImports, ModImplementationImports0,
				ModIndirectImports, ModPublicChildren,
				ModFactDeps, ModItems0, ModError0) },
	(
		% have we already read it?
		( { Import = ModuleName }
		; { list__member(Import, ModAncestors) }
		; { list__member(Import, ModInterfaceImports) }
		; { list__member(Import, ModImplementationImports0) }
		)
	->
		process_module_long_interfaces(Imports, Ext,
				IndirectImports0, IndirectImports,
				Module0, Module)
	;
		read_mod(Import, Ext,
			"Reading interface for module", yes, 
			LongIntItems, LongIntError, _LongIntFileName),
		{ strip_off_interface_decl(LongIntItems, Items) },
		{ maybe_add_int_error(LongIntError, ModError0, ModError) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		( { LongIntError = fatal } ->
			{ ModImplementationImports = ModImplementationImports0 }
		;
			{ ModImplementationImports =
				[Import | ModImplementationImports0] },
			check_module_accessibility(ModuleName, Import,
				ModItems0)
		),
		{ get_dependencies(Items, IndirectImports1, IndirectUses1) },
		{ list__append(IndirectImports0, IndirectImports1,
			IndirectImports2) },
		{ list__append(IndirectImports2, IndirectUses1,
			IndirectImports3) },
		{ list__append(ModItems0, Items, ModItems) },
		{ Module1 = module_imports(FileName, ModuleName, ModAncestors,
				ModInterfaceImports, ModImplementationImports,
				ModIndirectImports, ModPublicChildren,
				ModFactDeps, ModItems, ModError) },

		process_module_long_interfaces(Imports, Ext,
			IndirectImports3, IndirectImports, Module1, Module)
	).

:- pred check_module_accessibility(module_name, module_name, item_list,
				io__state, io__state).
:- mode check_module_accessibility(in, in, in, di, uo) is det.

check_module_accessibility(ModuleName, ImportedModule, Items) -->
	( { ImportedModule = qualified(ParentModule, SubModule) } ->
		%
		% Check that the imported/used module is accessible,
		% by searching through the current item list (we should
		% have already read in the imported module's parent module
		% at this point, so the item list should include the items
		% in the parent's interface) looking for an `include_module'
		% declaration that names it.
		%
		(
			{ get_children(Items, AccessibleSubModules) },
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
very_long_name.m:123:  `use_module' declaration to import module
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

process_module_indirect_imports(IndirectImports, Ext, Module0, Module) -->
		% Treat indirectly imported items as if they were imported 
		% using `:- use_module', since all uses of them in the `.int'
		% files must be module qualified.
	{ append_pseudo_decl(Module0, used, Module1) },
	process_module_short_interfaces_transitively(IndirectImports,
		Ext, Module1, Module).

process_module_short_interfaces_transitively(Imports, Ext, Module0, Module) -->
	process_module_short_interfaces(Imports, Ext, [], IndirectImports,
		Module0, Module1),
	( { IndirectImports = [] } ->
		{ Module = Module1 }
	;
		process_module_short_interfaces_transitively(IndirectImports,
			Ext, Module1, Module)
	).

process_module_short_interfaces([], _,
		IndirectImports, IndirectImports, Module, Module) --> [].
process_module_short_interfaces([Import | Imports], Ext, 
		IndirectImports0, IndirectImports, Module0, Module) -->
	{ Module0 = module_imports(FileName, ModuleName, ModAncestors,
			ModInterfaceDeps, ModImplementationDeps,
			ModIndirectImports0, ModPublicChildren, ModFactDeps,
			ModItems0, ModError0) },
	(
		% check if the imported module has already been imported
		{ Import = ModuleName
		; list__member(Import, ModAncestors)
		; list__member(Import, ModInterfaceDeps)
		; list__member(Import, ModImplementationDeps)
		; list__member(Import, ModIndirectImports0)
		}
	->
		process_module_short_interfaces(Imports, Ext,
			IndirectImports0, IndirectImports, Module0, Module)
	;
		read_mod(Import, Ext,
				"Reading short interface for module", yes,
				ShortIntItems, ShortIntError, _ImportFileName),
		{ strip_off_interface_decl(ShortIntItems, Items) },
		{ maybe_add_int_error(ShortIntError, ModError0, ModError) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		{ ModIndirectImports = [Import | ModIndirectImports0] },
		{ get_dependencies(Items, Imports1, Uses1) },
		{ list__append(IndirectImports0, Imports1, IndirectImports1) },
		{ list__append(IndirectImports1, Uses1, IndirectImports2) },
		{ list__append(ModItems0, Items, ModItems) },
		{ Module1 = module_imports(FileName, ModuleName, ModAncestors,
			ModInterfaceDeps, ModImplementationDeps,
			ModIndirectImports, ModPublicChildren, ModFactDeps,
			ModItems, ModError) },
		process_module_short_interfaces(Imports, Ext,
			IndirectImports2, IndirectImports, Module1, Module)
	).

:- pred strip_off_interface_decl(item_list, item_list).
:- mode strip_off_interface_decl(in, out) is det.

% strip off the `:- interface' declaration at the start, if any

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
	( InterfaceError \= no ->
		ModError = yes
	;
		ModError = ModError0
	).

%-----------------------------------------------------------------------------%

get_ancestors(ModuleName, Ancestors) :-
	get_ancestors_2(ModuleName, [], Ancestors).
	
:- pred get_ancestors_2(module_name, list(module_name), list(module_name)).
:- mode get_ancestors_2(in, in, out) is det.

get_ancestors_2(unqualified(_), Ancestors, Ancestors).
get_ancestors_2(qualified(Parent, _), Ancestors0, Ancestors) :-
	Ancestors1 = [Parent | Ancestors0],
	get_ancestors_2(Parent, Ancestors1, Ancestors).

%-----------------------------------------------------------------------------%

get_partial_qualifiers(unqualified(_), []).
get_partial_qualifiers(qualified(ParentQual, ChildName),
			[PartialQual | PartialQuals]) :-
	drop_one_qualifier(ParentQual, ChildName, PartialQual),
	get_partial_qualifiers(PartialQual, PartialQuals).
	
:- pred drop_one_qualifier(module_name, string, module_name).
:- mode drop_one_qualifier(in, in, out) is det.

drop_one_qualifier(ParentQual, ChildName, PartialQual) :-
	(
		ParentQual = unqualified(_ParentName),
		PartialQual = unqualified(ChildName)
	;
		ParentQual = qualified(GrandParentQual, ParentName), 
		drop_one_qualifier(GrandParentQual, ParentName,
				PartialGrantParentQual),
		PartialQual = qualified(PartialGrantParentQual, ChildName)
	).

%-----------------------------------------------------------------------------%

	% get_children(Items, IncludeDeps):
	%	IncludeDeps is the list of sub-modules declared with
	% 	`:- import_module' in Items.
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
	get_dependencies_2(Items, [], ImportDeps, [], UseDeps).

:- pred get_dependencies_2(item_list, list(module_name), list(module_name), 
		list(module_name), list(module_name)).
:- mode get_dependencies_2(in, in, out, in, out) is det.

get_dependencies_2([], ImportDeps, ImportDeps, UseDeps, UseDeps).
get_dependencies_2([Item - _Context | Items], ImportDeps0, ImportDeps,
		UseDeps0, UseDeps) :-
	( 
		Item = module_defn(_VarSet, import(module(Modules)))
	->
		list__append(ImportDeps0, Modules, ImportDeps1),
		UseDeps1 = UseDeps0
	;
		Item = module_defn(_VarSet, use(module(Modules)))
	->
		list__append(UseDeps0, Modules, UseDeps1),
		ImportDeps1 = ImportDeps0
	;
		ImportDeps1 = ImportDeps0,
		UseDeps1 = UseDeps0
	),
	get_dependencies_2(Items, ImportDeps1, ImportDeps, UseDeps1, UseDeps).

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
	{ require(unify(Items, []), "modules.m: items after end_module") }.

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
		% combine the sub-module declarations from the prevous two
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
	%
	( map__search(SubModules0, ModuleName, ItemList0) ->
		list__append(ModuleItemList, ItemList0, ItemList),
		map__det_update(SubModules0, ModuleName, ItemList, SubModules)
	;
		map__det_insert(SubModules0, ModuleName, ModuleItemList,
								SubModules)
	).

:- pred report_error_implementation_in_interface(module_name, term__context,
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

	% Given a module (well, a list of items), extract the interface
	% part of that module, i.e. all the items between `:- interface'
	% and `:- implementation'. If IncludeImported is yes, also
	% include all items after a `:- imported'. This is useful for
	% making the .int file.

:- pred get_interface(item_list, bool, item_list).
:- mode get_interface(in, in, out) is det.

get_interface(Items0, IncludeImported, Items) :-
	get_interface_2(Items0, no, IncludeImported, [], RevItems),
	list__reverse(RevItems, Items).

:- pred get_interface_2(item_list, bool, bool, item_list, item_list).
:- mode get_interface_2(in, in, in, in, out) is det.

get_interface_2([], _, _, Items, Items).
get_interface_2([Item - Context | Rest], InInterface0,
				IncludeImported, Items0, Items) :-
	( Item = module_defn(_, interface) ->
		Items1 = Items0,
		InInterface1 = yes
	; 
		Item = module_defn(_, Defn),
		( Defn = imported
		; Defn = used
		)
	->
		% module_qual.m needs the :- imported declaration.
		( IncludeImported = yes, InInterface0 = yes ->
			Items1 = [Item - Context | Items0]
		;
			Items1 = Items0
		),
		InInterface1 = InInterface0
	;
		Item = module_defn(_, implementation) 
	->
		Items1 = Items0,
		InInterface1 = no
	;
		( InInterface0 = yes ->
			Items1 = [Item - Context | Items0]
		;
			Items1 = Items0
		),
		InInterface1 = InInterface0
	),
	get_interface_2(Rest, InInterface1, IncludeImported, Items1, Items).

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

:- pred get_short_interface(item_list, item_list).
:- mode get_short_interface(in, out) is det.

get_short_interface(Items0, Items) :-
	get_short_interface_2(Items0, [], [], no,
			RevItems, RevImports, NeedsImports),
	list__reverse(RevItems, Items1),
	( NeedsImports = yes ->
		list__reverse(RevImports, Imports1),
		list__append(Imports1, Items1, Items)
	;
		Items = Items1
	).

:- pred get_short_interface_2(item_list, item_list, item_list, bool,
				item_list, item_list, bool).
:- mode get_short_interface_2(in, in, in, in, out, out, out) is det.

get_short_interface_2([], Items, Imports, NeedsImports,
			Items, Imports, NeedsImports).
get_short_interface_2([ItemAndContext | Rest], Items0, Imports0, NeedsImports0,
			Items, Imports, NeedsImports) :-
	ItemAndContext = Item0 - Context,
	( Item0 = module_defn(_, import(_)) ->
		Items1 = Items0,
		Imports1 = [ItemAndContext | Imports0],
		NeedsImports1 = NeedsImports0
	; Item0 = module_defn(_, use(_)) ->
		Items1 = Items0,
		Imports1 = [ItemAndContext | Imports0],
		NeedsImports1 = NeedsImports0
	; make_abstract_type_defn(Item0, Item1) ->
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
	get_short_interface_2(Rest, Items1, Imports1, NeedsImports1,
				Items, Imports, NeedsImports).

:- pred include_in_short_interface(item).
:- mode include_in_short_interface(in) is semidet.

include_in_short_interface(type_defn(_, _, _)).
include_in_short_interface(inst_defn(_, _, _)).
include_in_short_interface(mode_defn(_, _, _)).
include_in_short_interface(module_defn(_, _)).
include_in_short_interface(typeclass(_, _, _, _, _)).

:- pred make_abstract_type_defn(item, item).
:- mode make_abstract_type_defn(in, out) is semidet.

make_abstract_type_defn(type_defn(VarSet, du_type(Name, Args, _, _), Cond),
			type_defn(VarSet, abstract_type(Name, Args), Cond)).
make_abstract_type_defn(type_defn(VarSet, abstract_type(Name, Args), Cond),
			type_defn(VarSet, abstract_type(Name, Args), Cond)).

	% Given a module (well, a list of items), extract the interface
	% part of that module, i.e. all the items between `:- interface'
	% and `:- implementation'. If IncludeImported is yes, also
	% include all items after a `:- imported'. This is useful for
	% making the .int file.

%-----------------------------------------------------------------------------%
