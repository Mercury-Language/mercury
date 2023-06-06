%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: file_name.m.
%
% This module deals with the connections between
% module names and file names.
%
%---------------------------------------------------------------------------%

:- module parse_tree.file_names.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.

%---------------------------------------------------------------------------%
%
% XXX This interface should be improved in two ways.
%
% - First, the implementation of this predicate effectively divides
%   the set of possible values of Ext into classes of extensions,
%   treating every extension in a given class the same way.
%
%   We should replace the simple string Ext argument with a more
%   structured specification of the extension, one that puts a wrapper
%   around the actual suffix indicating what class the extension falls in,
%   as in e.g. ec_library(".a"). For some classes, maybe the majority,
%   the list of member extensions may be fixed; for these, it would
%   make sense to specify each member of the class by a value in an
%   extension-class-specific enum type, not by string.
%
%   While the code handling some of classes accesses the filesystem,
%   the code handling some other classes does not. If we put the wrappers
%   for these two kinds of classes into two separate types, we could
%   have a version of this predicate for each type, one with and one
%   without an I/O state pair.
%
%   Work along these lines has begun.
%
% - Second, calls which search for a source file for reading (that may not
%   exist) should be separated from calls that construct a file name
%   to write the file.
%
% XXX Given the wide variety of uses cases that choose_file_name has
% to handle for its callers, the only way to ensure that a diff implementing
% the above ideas handles *all* of those uses cases correctly is probably to
%
% - Gather a list of all the extensions choose_file_name is called with.
%
% - Set up a test environment with distinctively named directories in
%   all the relevant directory search options.
%
% - Invoke choose_file_name and module_name_to_file_name_ext
%   with all possible combinations of 
%
%       --use-grade-subdirs, --use-subdirs or neither
%       empty and distinctive nonempty base parent dirs
%       unqualified and qualified module names
%       extension
%       maybe_search
%       maybe_create_dirs
%
%   and record the results as the baseline.
%
% - Repeat the exercise with the proposed replacement code, and
%   compare the results to the baseline.

    % If the proper place for a file is in a subdirectory (e.g. Mercury/css),
    % but the subdirectory does not exist, which in this case may mean either
    %
    % - that Mercury exists but Mercury/css does not, or
    % - that Mercury does not exist, which of course means that Mercury/css
    %   does not exist either,
    %
    % values of this type tell the predicate they are given to whether it
    % should create any such missing directories.
:- type maybe_create_dirs
    --->    do_create_dirs
    ;       do_not_create_dirs.

    % For some kinds of files, we know exactly in which directory
    % they should be; for other kinds, we may have to search several
    % directories. For the latter, our clients will need to call the
    % module_name_to_search_file_name predicate, which internally sets
    % MaybeSearch to do_search.
    %
    % Note that do_create_dirs is not compatible with do_search; if you
    % know what one of several directories should contain a file, but don't
    % know which one, you have by definition no basis you can use to choose
    % which one to create. This invariant is enforced by the fact that
    % module_name_to_search_file_name always passes do_not_create_dirs
    % alongside do_search.
    %
    % This type is not used in the interface of this module, but it is used
    % by the *clients* of this module.
    %
    % Note that module_name_to_search_file_name constructs just the
    % *filename to search for*, and leaves the actual searching to be done
    % by some other system component. Usually, that component is
    % either module_name_to_search_file_name's caller, or *its* caller, etc.
    % However, in some cases (such as .mh/.mih files), we just output
    % the search file name, and leave it to the target language compiler
    % to do the searching.
    %
    % XXX This setup makes it hard to build a database of
    %
    % - which extensions are ever subject to search, and
    % - what the search path of each such extension is.
:- type maybe_search
    --->    do_search
    ;       do_not_search.

    % This type is intended to represent an extension at the end of a filename.
    % However, we currently also use it to represent an extension at the end
    % of the name of an mmake target. Some of those uses include references
    % to the values of mmake variables.
    %
    % We can partition the set of extensions we handle, along the lines
    % described above, by adding more function symbols to this type,
    % and specifying what suffixes correspond to each function symbol.
:- type ext
    --->    ext_src
            % The extension string is ".m".
    ;       ext_other(other_ext).
            % The general case. The extension string must not be covered
            % by any of the other cases above.

:- type other_ext
    --->    other_ext(string).

:- func extension_to_string(ext) = string.
:- func other_extension_to_string(other_ext) = string.

:- func make_module_dep_file_extension = other_ext is det.

%---------------------%

% XXX Most of the predicates below take a "from" string argument,
% for which the caller is expected pass $pred or some other identification
% of the call site. This is a temporary measure, intended to help debug
% any problems that may arise during the process of slicing the space
% of extensions into smaller and smaller pieces.

    % Return the file name of the Mercury source for the given module.
    %
:- pred module_name_to_source_file_name(module_name::in, file_name::out,
    io::di, io::uo) is det.

    % module_name_to_file_name(Globals, Mkdir, Ext, Module, FileName, !IO):
    %
    % Convert a module name and file extension to the corresponding file name.
    % If `MkDir' is do_create_dirs, then create any directories needed.
    %
    % Currently we use the convention that the module `foo.bar.baz' should be
    % named `foo.bar.baz.m', and allow other naming conventions with the
    % `-f' option.
    %
    % Note that this predicate is also used to create some "phony" Makefile
    % targets that do not have corresponding files, e.g. `<foo>.clean'.
    %
:- pred module_name_to_file_name(globals::in, string::in,
    maybe_create_dirs::in, ext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % module_name_to_search_file_name(Globals, Ext, Module, FileName, !IO):
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
:- pred module_name_to_search_file_name(globals::in, string::in, ext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % module_name_to_lib_file_name(Globals, MkDir, Prefix, Ext,
    %   Module, FileName, !IO):
    %
    % Like module_name_to_file_name, but also allows a prefix.
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name(globals::in, string::in,
    maybe_create_dirs::in, string::in, other_ext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name(Globals, MkDir, Ext, FactTableFileName,
    %   FileName, !IO):
    %
    % Returns the filename to use when compiling fact table files.
    % If `MkDir' is do_create_dirs, then create any directories needed.
    %
:- pred fact_table_file_name(globals::in, string::in, maybe_create_dirs::in,
    other_ext::in, file_name::in, file_name::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Succeeds iff the module referred to by the module name is one
    % of the modules in the standard library.
    %
:- pred mercury_std_library_module_name(module_name::in) is semidet.

    % qualify_mercury_std_library_module_name(ModuleName) = QualModuleName:
    %
    % If ModuleName is a standard library module then return the module with an
    % extra `mercury' prefix. Otherwise, return the module name unchanged.
    %
:- func qualify_mercury_std_library_module_name(module_name) = module_name.

%---------------------------------------------------------------------------%

    % Convert a file name (excluding the trailing `.m') to the corresponding
    % module name.
    %
:- pred file_name_to_module_name(file_name::in, module_name::out) is det.

    % Convert a module name to a file name stem (e.g. foo.bar.baz).
    %
:- pred module_name_to_file_name_stem(module_name::in, file_name::out) is det.

    % Convert a module name to something that is suitable
    % for use as a variable name in makefiles.
    %
:- pred module_name_to_make_var_name(module_name::in, string::out) is det.

%---------------------------------------------------------------------------%

    % Return the name of the directory containing Java `.class' files.
    %
:- pred get_class_dir_name(globals::in, string::out) is det.

%---------------------------------------------------------------------------%

    % Convert an include_file reference to a filesystem path.
    %
:- pred make_include_file_path(string::in, string::in, string::out) is det.

%---------------------------------------------------------------------------%

    % Test whether a proposed replacement for module_name_to_file_name_ext
    % generates the same outputs as the original.
    %
:- pred test_file_name_extensions(globals::in, io.text_output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % This predicate is intended to output profiling data that can later
    % be used to improve the operation of this module. It appends to
    % /tmp/TRANSLATIONS_RECORD information about the frequency with which
    % this module is asked to translate file names with various suffixes,
    % provided that the gathering of this information has been enabled by
    % both the right trace flag at compile time and the right environment
    % variable at runtime.
    %
:- pred write_translations_record_if_any(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.options.
:- import_module parse_tree.java_names.
:- import_module parse_tree.source_file_map.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

extension_to_string(Ext) = ExtStr :-
    (
        Ext = ext_src,
        ExtStr = ".m"
    ;
        Ext = ext_other(OtherExt),
        ExtStr = other_extension_to_string(OtherExt)
    ).

other_extension_to_string(OtherExt) = ExtStr :-
    OtherExt = other_ext(ExtStr).

%---------------------------------------------------------------------------%

:- pred valid_other_ext(other_ext::in) is semidet.

valid_other_ext(other_ext(ExtStr)) :-
    % We define what string is valid as an argument of ext/1 negatively:
    % any extension string is valid as an argument of ext/1 *unless*
    % it has some other representation.
    not (
        ExtStr = ".m"       % ext_src
    ).

%---------------------------------------------------------------------------%

make_module_dep_file_extension = other_ext(".module_dep").

%---------------------------------------------------------------------------%

module_name_to_source_file_name(ModuleName, SourceFileName, !IO) :-
    % Look up the module in the module->file mapping.
    source_file_map.lookup_module_source_file(ModuleName, MaybeFileName, !IO),
    (
        MaybeFileName = yes(SourceFileName)
    ;
        MaybeFileName = no,
        % We get here only if
        %
        % - the source file map says that the default source file name
        %   for ModuleName is actually being used to store a module
        %   *other than* ModuleName, *and*
        % - the source file map itself does not know in which file ModuleName
        %   is stored.
        %
        % This can happen only if either the source file map was built from
        % an incomplete list of source files, or if that list became incomplete
        % later, as new source files were added.
        %
        % XXX What we do here is *seriously* suboptimal. Any programmer
        % who is clueless enough to screw up the naming of source files
        % this badly will be even more confused by the mess resulting
        % from the code below.
        %
        % The old XXX suggested that we should propagate the fact that
        % no source file name is available for the given module back to
        % our caller, but I think it would be simpler for the code here
        % to print an error message
        %
        % - describing the problem, and
        % - suggest ways to fix it (put modules into files with non-colliding
        %   names, or running mmc -f *.m),
        %
        % and then exit with an error status, *without* returning to our
        % caller.
        SourceFileName =
            "Mercury/.missing." ++ default_source_file_name(ModuleName)
    ).

%---------------------------------------------------------------------------%

module_name_to_file_name(Globals, From, MkDir, Ext,
        ModuleName, FileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, do_not_search, MkDir,
        Ext, ModuleName, FileName, !IO).

module_name_to_search_file_name(Globals, From, Ext,
        ModuleName, FileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, do_search, do_not_create_dirs,
        Ext, ModuleName, FileName, !IO).

module_name_to_lib_file_name(Globals, From, MkDir, Prefix, Ext,
        ModuleName, FileName, !IO) :-
    BaseFileName = sym_name_to_string(ModuleName),
    BaseNameNoExt = Prefix ++ BaseFileName,
    % Library files do not need the preprocessing that
    % module_name_to_file_name_ext does before calling choose_file_name.
    choose_file_name(Globals, From, do_not_search, Ext,
        [], BaseNameNoExt, DirComponents, FileName),
    maybe_create_dirs_on_path(MkDir, DirComponents, !IO).

fact_table_file_name(Globals, From, MkDir, Ext,
        FactTableFileName, FileName, !IO) :-
    % Fact table files do not need the preprocessing that
    % module_name_to_file_name_ext does before calling choose_file_name.
    choose_file_name(Globals, From, do_not_search, Ext,
        [], FactTableFileName, DirComponents, FileName),
    maybe_create_dirs_on_path(MkDir, DirComponents, !IO).

%---------------------------------------------------------------------------%

% XXX The implementations of the following predicates, namely
% module_name_to_file_name_ext and its subcontractors choose_file_name
% and make_file_name, effectively divide the set of possible values
% of Ext into classes of extensions, treating every extension
% in a given class the same way.
%
% We should replace the simple Ext argument, which currently represents
% all extensions using the same function symbol, with a more structured
% specification of the extension, one which has several function symbols,
% each indicating which class the extension falls in. For example, ext_src
% would indicate source files (without need for storing the suffix), while
% ext_obj(...) would indicate an object file, with its string argument could
% be e.g. .o, .pic_o, .lpic_o etc to indicate what *kind* of object file.
% Such function symbols could have other arguments to indicate e.g. the
% presence or absence of "_init" between the base name of the file
% and the extension itself. For some classes, maybe the majority, the list of
% member extensions may be fixed. For these, it would make sense to specify
% each member of the class by a value in an extension-class-specific enum type,
% not by a string. And for a class containing only one extension, such as
% source files, even that should not be needed. A possible difficulty is that
% the strings of some extensions are not fixed, but are taken from compiler
% options. However, for most, if not all of these options, the class that
% the extension they represent falls into should be clear.
%
% The extension class should specify whether
%
% - the extensions in the class are (architecture or) grade dependent,
% - if yes, what the name of the grade and non-grade directories are,
% - if not, what the name of the non-grade directory is, and
% - whether it is worth caching the translation (a translation that is
%   either trivially cheap or usually only done once per compiler invocation
%   is not worth caching).
%
% While the code handling some of classes accesses the filesystem,
% the code handling some other classes does not. If we put the wrappers
% for these two kinds of classes into two separate types, we could
% have two versions of these predicates for each type, one with and one
% without an I/O state pair.
%
% It may also be a good idea to also separate the representations of extensions
% of filenames (which can also be mmake targets) vs strings whose *only* use
% is as mmake targets (since they are not also filenames). This is because
% it makes sense to search in the filesystem for filenames, but not for
% mmake targets, and because mmake targets (should) never need directories
% created for them.
%
% The classification does not need to include .tmp suffixes on extensions,
% since (due to the behavior of the mercury_update_interface script, which
% the .tmp suffixes are for) the .tmp suffix *always* goes directly after
% the end of the corresponding non-.tmp filename, and can never be e.g.
% found in a different directory in a search. Calls to the exported predicates
% of this module should never specify .tmp as part of the extension; instead,
% they should add the .tmp suffix to the filename they get back from those
% predicates instead.

:- pred module_name_to_file_name_ext(globals::in, string::in,
    maybe_search::in, maybe_create_dirs::in, ext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name_ext(Globals, From, Search, MkDir, Ext,
        ModuleName, FileName, !IO) :-
    (
        Ext = ext_src,
        module_name_to_source_file_name(ModuleName, FileName, !IO)
    ;
        Ext = ext_other(OtherExt),
        decide_base_name_parent_dirs_other(OtherExt, ModuleName,
            BaseParentDirs, BaseNameNoExt),
        choose_file_name(Globals, From, Search, OtherExt,
            BaseParentDirs, BaseNameNoExt, DirComponents, FileName),
        maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
    ),
    trace [compile_time(flag("file_name_translations")),
        runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
    (
        record_translation(Search, MkDir, Ext, ModuleName, FileName, !TIO)
    ).

%---------------------%

:- pred decide_base_name_parent_dirs_other(other_ext::in, module_name::in,
    list(string)::out, file_name::out) is det.

decide_base_name_parent_dirs_other(OtherExt, ModuleName,
        BaseParentDirs, BaseNameNoExt) :-
    OtherExt = other_ext(ExtStr),
    expect(valid_other_ext(OtherExt), $pred,
        ExtStr ++ " is a not valid argument of ext/1"),
    ( if
        % Java files need to be placed into a package subdirectory
        % and may need mangling.
        ( string.suffix(ExtStr, ".java")
        ; string.suffix(ExtStr, ".class")
        )
    then
        BaseParentDirs = ["jmercury"],
        mangle_sym_name_for_java(ModuleName, module_qual, "__", BaseNameNoExt)
    else
        BaseParentDirs = [],
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, ".")
    ).

%---------------------%

    % choose_file_name(Globals, Search, OtherExt, BaseParentDirs, BaseName,
    %   FileName, !IO)
    %
    % BaseParentDirs is usually empty. For Java files, BaseParentDirs are the
    % package directories that the file needs to be placed in.
    %
:- pred choose_file_name(globals::in, string::in, maybe_search::in,
    other_ext::in, list(string)::in, string::in,
    list(string)::out, file_name::out) is det.

choose_file_name(Globals, _From, Search, OtherExt,
        BaseParentDirs, BaseNameNoExt, DirComponents, FileName) :-
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    OtherExt = other_ext(ExtStr),
    ( if
        % If we are searching for (rather than writing) a `.mih' file,
        % use the plain file name. This is so that searches for files
        % in installed libraries will work. `--c-include-directory' is set
        % so that searches for files in the current directory will work.
        Search = do_search,
        ExtStr = ".mih"
    then
        DirComponents = [],
        FileName = BaseNameNoExt ++ ExtStr
    else
        (
            UseSubdirs = no,
            % Even if not putting files in a `Mercury' directory,
            % Java files will have non-empty BaseParentDirs (the package)
            % which may need to be created.
            % XXX We can never target Java while UseSubdirs = no. However,
            % while making dependencies, generate_d_file in write_deps_file.m
            % does call module_name_to_file_name with a .java suffix
            % without --use-subdirs being enabled, so insisting on
            % BaseParentDirs = [] here would cause an abort when making
            % dependencies.
            % XXX This indicates a deeper problem, which is that while
            % different backends use different settings of --use-subdirs
            % and --use-grade-subdirs, we are generating dependencies
            % for *all* backends with the *same* settings of these options.
            FileName = glue_dir_names_file_name(BaseParentDirs,
                BaseNameNoExt, ExtStr),
            DirComponents = BaseParentDirs
        ;
            UseSubdirs = yes,
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            ( if
                % The source files, the final executables, library files
                % (including .init files) output files intended for use
                % by the user, and phony Mmake targets names go in the
                % current directory.
                not (
                    UseGradeSubdirs = yes,
                    file_is_arch_or_grade_dependent(Globals, OtherExt)
                ),
                is_current_dir_extension(ExtStr)
            then
                DirComponents = [],
                FileName = BaseNameNoExt ++ ExtStr
            else
                choose_subdir_name(Globals, ExtStr, SubDirName),
                make_file_name(Globals, [SubDirName | BaseParentDirs], Search,
                    BaseNameNoExt, OtherExt, DirComponents, FileName)
            )
        )
    ).

:- pred is_current_dir_extension(string::in) is semidet.

is_current_dir_extension(ExtStr) :-
    % Executable files.
    % XXX The Ext = "" here is wrong. While an empty extension
    % *can* mean we are building the name of an executable,
    % it can also mean we are building the name of a phony Mmakefile
    % target for a library, such as libmer_std in the library
    % directory.
    ( ExtStr = ""
    ; ExtStr = ".bat"
    ; ExtStr = ".exe"

    % Library files.
    ; ExtStr = ".a"
    ; ExtStr = ".$A"
    ; ExtStr = ".lib"
    ; ExtStr = ".so"
    ; ExtStr = ".dll"
    ; ExtStr = ".dylib"     % references can be generated only via options
    ; ExtStr = ".$(EXT_FOR_SHARED_LIB)"
    ; ExtStr = ".jar"
    ; ExtStr = ".init"

    % XXX Describe me.
    ; ExtStr = ".mh"

    % Output files intended for use by the user.
    % The MLDS dump files with extensions .c_dump* and .mih_dump*
    % also fit into this category, but their filenames are constructed
    % by getting the filenames for the .c and .mih extensions
    % and adding a suffix to that.
    ; ExtStr = ".err"
    ; ExtStr = ".ugly"
    ; ExtStr = ".hlds_dump"
    ; ExtStr = ".mlds_dump"
    ; ExtStr = ".dependency_graph"
    ; ExtStr = ".order"

    % Mmake targets.
    ; ExtStr = ".clean"
    ; ExtStr = ".realclean"
    ; ExtStr = ".depend"
    ; ExtStr = ".install_ints"
    ; ExtStr = ".install_opts"
    ; ExtStr = ".install_hdrs"
    ; ExtStr = ".install_grade_hdrs"
    ; ExtStr = ".check"
    ; ExtStr = ".ints"
    ; ExtStr = ".int3s"
    ; ExtStr = ".javas"
    ; ExtStr = ".classes"
    ; ExtStr = ".opts"
    ; ExtStr = ".trans_opts"
    ; ExtStr = ".all_ints"
    ; ExtStr = ".all_int3s"
    ; ExtStr = ".all_opts"
    ; ExtStr = ".all_trans_opts"
    ).

    % Decide which ext_other extensions go in which directories.
    %
:- pred choose_subdir_name(globals::in, string::in, string::out) is det.

choose_subdir_name(Globals, ExtStr, SubDirName) :-
    ( if
        (
            ( ExtStr = ".dir/*.o"
            ; ExtStr = ".dir/*.$O"
            ),
            SubDirNamePrime = "dirs"
        ;
            % .$O, .pic_o and .lpic_o files need to go in the same directory,
            % so that using .$(EXT_FOR_PIC_OBJECTS) will work.
            ( ExtStr = ".o"
            ; ExtStr = ".$O"
            ; ExtStr = ".lpic_o"
            ; ExtStr = ".pic_o"
            ; ExtStr = "$(EXT_FOR_PIC_OBJECTS)"
            ; ExtStr = "_init.o"
            ; ExtStr = "_init.$O"
            ; ExtStr = "_init.lpic_o"
            ; ExtStr = "_init.pic_o"
            ; ExtStr = "_init.$(EXT_FOR_PIC_OBJECTS)"
            ),
            SubDirNamePrime = "os"
        ;
            % `.dv' files go in the `deps' subdirectory,
            % along with the `.dep' files.
            ExtStr = ".dv",
            SubDirNamePrime = "deps"
        ;
            % Launcher scripts go in the `bin' subdirectory.
            ExtStr = "",
            SubDirNamePrime = "bin"
        )
    then
        SubDirName = SubDirNamePrime
    else if
        % _init.c, _init.cs, _init.o etc. files go in the cs, css, os etc
        % subdirectories.
        string.remove_prefix("_init.", ExtStr, ExtName)
    then
        SubDirName = ExtName ++ "s"
    else if
        globals.lookup_string_option(Globals, library_extension, LibExt),
        globals.lookup_string_option(Globals, shared_library_extension,
            SharedLibExt),
        % Static and shared libraries go in the `lib' subdirectory.
        ( ExtStr = LibExt
        ; ExtStr = SharedLibExt
        )
    then
        SubDirName = "lib"
    else if
        % The usual case: `*.foo' files go in the `foos' subdirectory.
        string.remove_prefix(".", ExtStr, ExtName)
    then
        SubDirName = ExtName ++ "s"
    else
        unexpected($pred, "unknown extension `" ++ ExtStr ++ "'")
    ).

:- pred make_file_name(globals::in, list(dir_name)::in, maybe_search::in,
    file_name::in, other_ext::in, list(string)::out, file_name::out) is det.

make_file_name(Globals, SubDirNames, Search, BaseNameNoExt, OtherExt,
        DirComponents, FileName) :-
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    OtherExt = other_ext(ExtStr),
    ( if
        UseGradeSubdirs = yes,
        file_is_arch_or_grade_dependent(Globals, OtherExt),

        % If we are searching for (rather than writing) the file, just search
        % in Mercury/<ext>s. This is so that searches for files in installed
        % libraries work. `--intermod-directories' is set so this will work.

        not (
            Search = do_search,
            ( ExtStr= ".opt"
            ; ExtStr= ".trans_opt"
            ; ExtStr= ".analysis"
            ; ExtStr= ".imdg"
            ; ExtStr= ".request"
            )
        )
    then
        grade_directory_component(Globals, Grade),
        globals.lookup_string_option(Globals, target_arch, TargetArch),

        % The extra "Mercury" is needed so we can use `--intermod-directory
        % Mercury/<grade>/<target_arch>' and `--c-include
        % Mercury/<grade>/<target_arch>' to find the local `.opt' and `.mih'
        % files without messing up the search for the files for installed
        % libraries.
        DirComponents = ["Mercury", Grade, TargetArch, "Mercury" | SubDirNames]
    else if
        UseSubdirs = yes
    then
        DirComponents = ["Mercury" | SubDirNames]
    else
        DirComponents = SubDirNames
    ),
    FileName = glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr).

:- func glue_dir_names_file_name(list(string), string, string) = string.

glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr) = FileName :-
    (
        DirComponents = [],
        FileName = BaseNameNoExt ++ ExtStr
    ;
        DirComponents = [_ | _],
        Components = DirComponents ++ [BaseNameNoExt ++ ExtStr],
        FileName = dir.relative_path_name_from_components(Components)
    ).

%---------------------%

:- pred file_is_arch_or_grade_dependent(globals::in, other_ext::in) is semidet.

file_is_arch_or_grade_dependent(Globals, OtherExt) :-
    OtherExt = other_ext(ExtStr),
    (
        file_is_arch_or_grade_dependent_2(ExtStr)
    ;
        globals.lookup_string_option(Globals, executable_file_extension,
            ExtStr)
    ;
        globals.lookup_string_option(Globals, library_extension, ExtStr)
    ;
        globals.lookup_string_option(Globals, shared_library_extension, ExtStr)
    ;
        some [ObjExtStr] (
            (
                globals.lookup_string_option(Globals,
                    object_file_extension, ObjExtStr)
            ;
                globals.lookup_string_option(Globals,
                    pic_object_file_extension, ObjExtStr)
            ),
            (
                ExtStr = ObjExtStr
            ;
                ExtStr = "_init" ++ ObjExtStr
            )
        )
    ).

:- pred file_is_arch_or_grade_dependent_2(string::in) is semidet.

    % The `.used' file isn't grade dependent itself, but it contains
    % information collected while compiling a grade-dependent `.c', `.cs',
    % etc file.
file_is_arch_or_grade_dependent_2("").
file_is_arch_or_grade_dependent_2(".$A").
file_is_arch_or_grade_dependent_2(".a").
file_is_arch_or_grade_dependent_2(".analysis").
file_is_arch_or_grade_dependent_2(".analysis_date").
file_is_arch_or_grade_dependent_2(".analysis_status").
file_is_arch_or_grade_dependent_2(".bat").
file_is_arch_or_grade_dependent_2(".c").
file_is_arch_or_grade_dependent_2(".c_date").
file_is_arch_or_grade_dependent_2(".class").
file_is_arch_or_grade_dependent_2(".cs").
file_is_arch_or_grade_dependent_2(".cs_date").
file_is_arch_or_grade_dependent_2(".dir").
file_is_arch_or_grade_dependent_2(".dll").
file_is_arch_or_grade_dependent_2(".imdg").
file_is_arch_or_grade_dependent_2(".init").
file_is_arch_or_grade_dependent_2(".jar").
file_is_arch_or_grade_dependent_2(".java").
file_is_arch_or_grade_dependent_2(".java_date").
file_is_arch_or_grade_dependent_2(".mih").
file_is_arch_or_grade_dependent_2(".opt").
file_is_arch_or_grade_dependent_2(".optdate").
file_is_arch_or_grade_dependent_2(".request").
file_is_arch_or_grade_dependent_2(".track_flags").
file_is_arch_or_grade_dependent_2(".trans_opt").
file_is_arch_or_grade_dependent_2(".trans_opt_date").
file_is_arch_or_grade_dependent_2(".used").
file_is_arch_or_grade_dependent_2("_init.$O").
file_is_arch_or_grade_dependent_2("_init.c").

%---------------------------------------------------------------------------%

mercury_std_library_module_name(ModuleName) :-
    (
        ModuleName = unqualified(Name),
        mercury_std_library_module(Name)
    ;
        ModuleName = qualified(_ParentModule, _Name),
        (
            module_name_to_file_name_stem(ModuleName, ModuleNameStr),
            mercury_std_library_module(ModuleNameStr)
        ;
            strip_outermost_qualifier(ModuleName, "mercury",
                StrippedModuleName),
            module_name_to_file_name_stem(StrippedModuleName,
                StrippedModuleNameStr),
            mercury_std_library_module(StrippedModuleNameStr)
        )
    ).

qualify_mercury_std_library_module_name(ModuleName) = QualModuleName :-
    ( if mercury_std_library_module_name(ModuleName) then
        QualModuleName = add_outermost_qualifier("mercury", ModuleName)
    else
        QualModuleName = ModuleName
    ).

%---------------------------------------------------------------------------%

file_name_to_module_name(FileName, ModuleName) :-
    ModuleName = string_to_sym_name(FileName).

module_name_to_file_name_stem(ModuleName, FileName) :-
    FileName = sym_name_to_string(ModuleName).

module_name_to_make_var_name(ModuleName, MakeVarName) :-
    MakeVarName = sym_name_to_string(ModuleName).

%---------------------------------------------------------------------------%

:- pred maybe_create_dirs_on_path(maybe_create_dirs::in, list(string)::in,
    io::di, io::uo) is det.

maybe_create_dirs_on_path(MkDir, DirComponents, !IO) :-
    (
        DirComponents = []
    ;
        DirComponents = [_ | _],
        (
            MkDir = do_create_dirs,
            DirName = dir.relative_path_name_from_components(DirComponents),
            % We avoid trying to create a directory if we have created it
            % before, because a set membership check here should be *much*
            % cheaper than a system call.
            %
            % We could try to check not just whether we have created
            % DirName before, but also any directory that corresponds
            % to a prefix of DirComponents. However, library/dir.m has
            % no mechanism we could use to tell it that a given prefix
            % of directories in DirComponents has already been created,
            % and that therefore checking whether they already exist
            % is unnecessary. (If any agent other than the Mercury system
            % is deleting some of these directories after their creation,
            % then things will be screwed up beyond Mercury's ability to
            % recover, *regardless* of what we do here.)
            %
            % This is not too much of a loss, since the crude test here
            % will still eliminate most of the eliminable system calls
            % involved in this task.
            get_made_dirs(MadeDirs0, !IO),
            ( if set_tree234.contains(MadeDirs0, DirName) then
                Made = yes
            else
                Made = no,
                make_directory(DirName, _, !IO),
                set_tree234.insert(DirName, MadeDirs0, MadeDirs),
                set_made_dirs(MadeDirs, !IO)
            ),
            trace [compile_time(flag("file_name_translations")),
                runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
            (
                Made = no,
                record_no_mkdir(DirName, !TIO)
            ;
                Made = yes,
                record_mkdir(DirName, !TIO)
            )
        ;
            MkDir = do_not_create_dirs
        )
    ).

:- mutable(made_dirs, set_tree234(string), set_tree234.init, ground,
    [untrailed, attach_to_io_state]).

%---------------------------------------------------------------------------%

get_class_dir_name(Globals, ClassDirName) :-
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    (
        UseGradeSubdirs = yes,
        grade_directory_component(Globals, Grade),
        globals.lookup_string_option(Globals, target_arch, TargetArch),
        ClassDirName = "Mercury" / Grade / TargetArch / "Mercury" / "classs"
    ;
        UseGradeSubdirs = no,
        (
            UseSubdirs = yes,
            ClassDirName = "Mercury" / "classs"
        ;
            UseSubdirs = no,
            ClassDirName = "."
        )
    ).

%---------------------------------------------------------------------------%

make_include_file_path(ModuleSourceFileName, OrigFileName, Path) :-
    ( if path_name_is_absolute(OrigFileName) then
        Path = OrigFileName
    else
        % XXX This will throw an exception on Windows if OrigFileName is a path
        % "X:foo", i.e. relative to the current directory on the X: drive.
        % That seems a silly thing to write in a source file.
        Path = dirname(ModuleSourceFileName) / OrigFileName
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % NOTE All of the string arguments below, with the possible exception
    % of the argument of newext_mmake_target, should be replaced by
    % category-specific enums.
:- type newext
    --->    newext_src
            % The extension string is ".m".

    ;       newext_executable(string)
    ;       newext_library(string)
            % Executables and library files, which are always put into
            % the current directory.

    ;       newext_executable_gs(string)
    ;       newext_library_gs(string)
            % Executables and library files, which are
            %
            % - put into the current directory with --no-use-grade-subdirs,
            % - put into a grade subdir with --use-grade-subdirs.
            %
            % Note that the documention of --use-grade-subdirs says
            % "Executables and libraries will be symlinked or copied into the
            % current directory", but if this is actually done, it is done
            % outside file_names.m.

    ;       newext_mh(string)
            % Information about Mercury code exported to outside C code.
            % The extension string is ".mh".

    ;       newext_mih(string)
            % Machine-independent header files for generated C code.
            % XXX Yet we consider them architecture-or-grade-dependent.
            % The extension string is ".mih".

    ;       newext_user(string)
            % Compiler-generated files that are intended to be read
            % by the programmer, such as .err files, which are always put
            % into the current directory.

    ;       newext_user_ngs(string)
            % Compiler-generated files that are intended to be read
            % by the programmer, such as .defns files, which will be put
            % into the current directory with --no-use-subdirs, but which
            % will be put into a non-grade-specific subdirectory with
            % --use-subdirs.

    ;       newext_mmake_target(string)
            % These suffixes are used not to create filenames, but to
            % create mmake target names. Some do refer to real files,
            % but they can (and some do) refer to these using extension
            % strings that can contain references to make variables.
            % Some of the other generated make targets are phony targets,
            % meaning that they never correspond to real files at all.

    ;       newext_mmake_var_ngs(string)
    ;       newext_mmake_var_gs(string)
            % These suffixes are used not to create filenames, but to
            % create mmake variable names.

    ;       newext_track_flags(string)

    ;       newext_mmakefile_fragment(string)
            % Compiler-generated files that are designed to be bodily included
            % in Mmakefiles.

    ;       newext_int(string)
    ;       newext_int_date(string)
    ;       newext_opt(string)
    ;       newext_opt_date(string)
            % Compiler-generated interface files. and optimization files,
            % and the timestamp files showing when they were last checked.

    ;       newext_target_c(string)
    ;       newext_target_c_date(string)
    ;       newext_target_cs(string)
    ;       newext_target_cs_date(string)
    ;       newext_target_java(string)
    ;       newext_target_java_date(string)

    ;       newext_target_init_c(string)

    ;       newext_target_init_obj(string)
    ;       newext_target_obj(string)

    ;       newext_bytecode(string)

    ;       newext_analysis(string)

    ;       newext_other(other_newext).
            % The general case. The extension string must not be covered
            % by any of the other cases above.

:- type other_newext
    --->    other_newext(string).

:- func make_new_extension(string) = newext.

make_new_extension(Str) = NewExt :-
    % The classifications marked "DODGY" below select a path in
    % module_name_to_file_name_ext_new that computes the right filename
    % (the filename computed by old code), but the name of their category
    % may not be applicable.
    ( if Str = ".m" then
        NewExt = newext_src
    else if is_current_dir_extension_new(Str, NewExtPrime) then
        NewExt = NewExtPrime
    else if Str = ".mih" then
        NewExt = newext_mih(Str)
    else if ( Str = ".d" ; Str = ".dv" ; Str = ".dep" ) then
        NewExt = newext_mmakefile_fragment(Str)
    else if ( Str = ".int0" ; Str = ".int" ) then
        NewExt = newext_int(Str)
    else if ( Str = ".int2" ; Str = ".int3" ) then
        NewExt = newext_int(Str)
    else if ( Str = ".date0" ; Str = ".date" ; Str = ".date3" ) then
        NewExt = newext_int_date(Str)
    else if ( Str = ".opt" ; Str = ".trans_opt" ) then
        NewExt = newext_opt(Str)
    else if ( Str = ".optdate" ; Str = ".trans_opt_date" ) then
        NewExt = newext_opt_date(Str)
    else if ( Str = ".class" ; Str = ".java" ) then
        NewExt = newext_target_java(Str)
    else if Str = ".java_date" then
        NewExt = newext_target_java_date(Str)
    else if Str = ".cs" then
        NewExt = newext_target_cs(Str)
    else if Str = ".cs_date" then
        NewExt = newext_target_cs_date(Str)
    else if Str = ".c" then
        NewExt = newext_target_c(Str)
    else if Str = ".c_date" then
        NewExt = newext_target_c_date(Str)
    else if ( Str = ".$O" ; Str = ".o" ; Str = ".pic_o" ) then
        NewExt = newext_target_obj(Str)
    else if Str = "_init.c" then
        NewExt = newext_target_init_c(Str)
    else if ( Str = "_init.$O" ; Str = "_init.o" ; Str = "_init.pic_o" ) then
        NewExt = newext_target_init_obj(Str)
    else if ( Str = ".mbc"; Str = ".bytedebug" ) then
        NewExt = newext_bytecode(Str)
    else if ( Str = ".analysis" ; Str = ".request" ; Str = ".imdg" ) then
        NewExt = newext_analysis(Str)
    else if ( Str = ".analysis_date"; Str = ".analysis_status" ) then
        NewExt = newext_analysis(Str)
    else if Str = ".module_dep" then
        NewExt = newext_mmake_var_ngs(Str)  % DODGY
    else if Str = ".prof" then
        NewExt = newext_mmake_var_ngs(Str)  % DODGY
    else if Str = ".err_date" then
        NewExt = newext_mmake_var_ngs(Str)  % DODGY
    else if Str = ".used" then
        NewExt = newext_mmake_var_gs(Str)
    else if Str = ".track_flags" then
        NewExt = newext_track_flags(Str)
    else if Str = ".dir/*.$O" then
        NewExt = newext_mmakefile_fragment(Str) % DODGY
    else
        NewExt = newext_other(other_newext(Str)),
        unexpected($pred, "ext_other")
    ).

%---------------------------------------------------------------------------%
%
% This code is near-duplicate of the call-tree of module_name_to_file_name_ext
% above, with the name of each predicate extended with the "_new" suffix.
%
% The intension is that we can modify this copy, and then compare its operation
% with the operation of the original.
%

% XXX START OF CODE DUPLICATION

:- pred module_name_to_file_name_ext_new(globals::in, string::in,
    maybe_search::in, maybe_create_dirs::in, newext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name_ext_new(Globals, From, Search, MkDir, Ext,
        ModuleName, FileName, !IO) :-
    (
        Ext = newext_src,
        module_name_to_source_file_name(ModuleName, FileName, !IO)
    ;
        ( Ext = newext_executable(ExtStr)
        ; Ext = newext_library(ExtStr)
        ; Ext = newext_mh(ExtStr)
        ; Ext = newext_user(ExtStr)
        ; Ext = newext_mmake_target(ExtStr)
        ),
        % Output files intended for use by the user, and phony Mmake target
        % names go in the current directory. So do .mh files, and *some*,
        % but not all, kinds of executable and library files.
        % XXX Why is that?
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        FileName = BaseNameNoExt ++ ExtStr
    ;
        ( Ext = newext_executable_gs(ExtStr)
        ; Ext = newext_library_gs(ExtStr)
        ),
        % Some kinds of executables and library files go in the current
        % directory only with --no-use-grade-subdirs; with --use-grade-subdirs,
        % they go in a grade subdir.
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_grade_subdirs,
            UseGradeSubdirs),
        (
            UseGradeSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseGradeSubdirs = yes,
            % This implies --use-subdirs as well.
            ( if ExtStr = "" then
                % Launcher scripts go in the `bin' subdirectory.
                SubDirName = "bin"
            else
                SubDirName = dot_extension_dir_name(ExtStr)
            ),
            make_grade_subdir_file_name_new(Globals, [SubDirName],
                BaseNameNoExt, ExtStr, DirComponents, FileName),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_mih(ExtStr),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        (
            Search = do_search,
            % If we are searching for (rather than writing) a `.mih' file,
            % use the plain file name. This is so that searches for files
            % in installed libraries will work. `--c-include-directory' is set
            % so that searches for files in the current directory will work.
            FileName = BaseNameNoExt ++ ExtStr
        ;
            Search = do_not_search,
            globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
            (
                UseSubdirs = no,
                FileName = BaseNameNoExt ++ ExtStr
            ;
                UseSubdirs = yes,
                SubDirName = dot_extension_dir_name(ExtStr),
                globals.lookup_bool_option(Globals, use_grade_subdirs,
                    UseGradeSubdirs),
                (
                    UseGradeSubdirs = no,
                    DirComponents = ["Mercury", SubDirName],
                    FileName = glue_dir_names_file_name(DirComponents,
                        BaseNameNoExt, ExtStr)
                ;
                    UseGradeSubdirs = yes,
                    make_grade_subdir_file_name_new(Globals, [SubDirName],
                        BaseNameNoExt, ExtStr, DirComponents, FileName)
                ),
                maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
            )
        )
    ;
        ( Ext = newext_user_ngs(ExtStr)
        ; Ext = newext_int(ExtStr)
        ; Ext = newext_int_date(ExtStr)
        ; Ext = newext_bytecode(ExtStr)
        ; Ext = newext_mmake_var_ngs(ExtStr) % XXX Probably never used.
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            SubDirName = dot_extension_dir_name(ExtStr),
            DirComponents = ["Mercury", SubDirName],
            FileName =
                glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_mmakefile_fragment(ExtStr),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            ( if ExtStr = ".dv" then
                SubDirName = "deps"
            else if ExtStr = ".dir/*.$O" then
                SubDirName = "dirs"
            else
                SubDirName = dot_extension_dir_name(ExtStr)
            ),
            DirComponents = ["Mercury", SubDirName],
            FileName =
                glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        ( Ext = newext_opt(ExtStr)
        ; Ext = newext_opt_date(ExtStr)
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            SubDirName = dot_extension_dir_name(ExtStr),
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            ( if
                UseGradeSubdirs = yes,
                % XXX The code from which this code is derived had this
                % comment, which I (zs) don't think adequately describes
                % the logic behind this confusing code:
                %
                % "If we are searching for (rather than writing) the file,
                % just search in Mercury/<ext>s. This is so that searches
                % for files in installed libraries work.
                % `--intermod-directories' is set so this will work."
                not (
                    Search = do_search,
                    Ext = newext_opt(_)
                )
            then
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            else
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_target_java(ExtStr),
        BaseParentDirs = ["jmercury"],
        mangle_sym_name_for_java(ModuleName, module_qual, "__", BaseNameNoExt),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            DirComponents = BaseParentDirs,
            FileName = glue_dir_names_file_name(DirComponents,
                BaseNameNoExt, ExtStr)
        ;
            UseSubdirs = yes,
            SubDirName = dot_extension_dir_name(ExtStr),
            SubDirNames = [SubDirName | BaseParentDirs],
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury" |  SubDirNames],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name_new(Globals, SubDirNames,
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            )
        ),
        maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
    ;
        ( Ext = newext_target_c(ExtStr)
        ; Ext = newext_target_c_date(ExtStr)
        ; Ext = newext_target_cs(ExtStr)
        ; Ext = newext_target_cs_date(ExtStr)
        ; Ext = newext_target_java_date(ExtStr)
        ; Ext = newext_mmake_var_gs(ExtStr) % XXX Probably never used.
        ; Ext = newext_track_flags(ExtStr) % XXX Probably never used.
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            SubDirName = dot_extension_dir_name(ExtStr),
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_analysis(ExtStr),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            SubDirName = dot_extension_dir_name(ExtStr),
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            ( if
                UseGradeSubdirs = yes,
                % XXX The code from which this code is derived had this
                % comment, which I (zs) don't think adequately describes
                % the logic behind this confusing code:
                %
                % If we are searching for (rather than writing) the file,
                % just search % in Mercury/<ext>s. This is so that searches
                % for files in installed libraries work.
                % `--intermod-directories' is set so this will work.
                not (
                    Search = do_search,
                    ( ExtStr= ".analysis"
                    ; ExtStr= ".imdg"
                    ; ExtStr= ".request"
                    )
                )
            then
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            else
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_target_init_c(ExtStr),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            expect(unify(ExtStr, "_init.c"), $pred, "ExtStr != _init.c"),
            SubDirName = "cs",
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        ( Ext = newext_target_obj(ExtStr)
        ; Ext = newext_target_init_obj(ExtStr)
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            % The original code that this code is derived from has
            % this comment:
            %
            %   .$O, .pic_o and .lpic_o files need to go in the same directory,
            %   so that using .$(EXT_FOR_PIC_OBJECTS) will work.
            %
            % XXX We stopped supporting lpic (linked-with-pic) files
            % years ago, and we don't ever invoke filename translations
            % with ".$(EXT_FOR_PIC_OBJECTS)" as the extension.
            SubDirName = "os",

            % XXX EXT Why aren't object files for grades that differ in e.g.
            % whether debugging is enabled stored in grade-specific directories
            % if --use-grade-subdirs is enabled? The .c files that they are
            % derived from *are* stored in grade-specific directories.
            % I (zs) expect that the reason why this hasn't been a problem
            % is that we don't target C with --use-grade-subdirs.
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            ( if
                UseGradeSubdirs = yes,
                Ext = newext_target_init_obj("_init.$O")
            then
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            else
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_other(OtherExt),
        decide_base_name_parent_dirs_other_new(OtherExt, ModuleName,
            BaseParentDirs, BaseNameNoExt),
        choose_file_name_new(Globals, From, Search, OtherExt,
            BaseParentDirs, BaseNameNoExt, DirComponents, FileName),
        maybe_create_dirs_on_path(MkDir, DirComponents, !IO),
        unexpected($pred, "newext_other")
    ).
% XXX NOT YET UPDATED FOR NEWEXT
%   trace [compile_time(flag("file_name_translations")),
%       runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
%   (
%       record_translation(Search, MkDir, Ext, ModuleName, FileName, !TIO)
%   ).

    % Given an extension of the form .xyz, return the name of the directory
    % into which files with that etension should be put, i.e. xyzs.
    %
:- func dot_extension_dir_name(string) = string.

dot_extension_dir_name(ExtStr) = SubDirName :-
    ( if string.remove_prefix(".", ExtStr, ExtName) then
        % The usual case: `*.foo' files go in the `foos' subdirectory.
        SubDirName = ExtName ++ "s"
    else
        string.format("ExtStr <%s> does not start with dot",
            [s(ExtStr)], Msg),
        unexpected($pred, Msg)
    ).

%---------------------%

:- pred decide_base_name_parent_dirs_other_new(other_newext::in,
    module_name::in, list(string)::out, file_name::out) is det.

decide_base_name_parent_dirs_other_new(OtherExt, ModuleName,
        BaseParentDirs, BaseNameNoExt) :-
    OtherExt = other_newext(ExtStr),
    expect(valid_other_newext(OtherExt), $pred,
        ExtStr ++ " is a not valid argument of ext/1"),
    BaseParentDirs = [],
    BaseNameNoExt = sym_name_to_string_sep(ModuleName, ".").

%---------------------%

    % choose_file_name_new(Globals, Search, OtherExt, BaseParentDirs, BaseName,
    %   FileName, !IO)
    %
    % BaseParentDirs is usually empty. For Java files, BaseParentDirs are the
    % package directories that the file needs to be placed in.
    %
:- pred choose_file_name_new(globals::in, string::in, maybe_search::in,
    other_newext::in, list(string)::in, string::in,
    list(string)::out, file_name::out) is det.

choose_file_name_new(Globals, _From, Search, OtherExt,
        BaseParentDirs, BaseNameNoExt, DirComponents, FileName) :-
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    OtherExt = other_newext(ExtStr),
    (
        UseSubdirs = no,
        % Even if not putting files in a `Mercury' directory,
        % Java files will have non-empty BaseParentDirs (the package)
        % which may need to be created.
        % XXX We can never target Java while UseSubdirs = no. However,
        % while making dependencies, generate_d_file in write_deps_file.m
        % does call module_name_to_file_name with a .java suffix
        % without --use-subdirs being enabled, so insisting on
        % BaseParentDirs = [] here would cause an abort when making
        % dependencies.
        % XXX This indicates a deeper problem, which is that while
        % different backends use different settings of --use-subdirs
        % and --use-grade-subdirs, we are generating dependencies
        % for *all* backends with the *same* settings of these options.
        FileName = glue_dir_names_file_name(BaseParentDirs,
            BaseNameNoExt, ExtStr),
        DirComponents = BaseParentDirs
    ;
        UseSubdirs = yes,
        choose_subdir_name_new(Globals, ExtStr, SubDirName),
        make_file_name_new(Globals, [SubDirName | BaseParentDirs],
            Search, BaseNameNoExt, OtherExt, DirComponents, FileName)
    ).

:- pred is_current_dir_extension_new(string::in, newext::out) is semidet.

is_current_dir_extension_new(ExtStr, NewExt) :-
    % Since the newext_executable_gs and newext_library_gs alternatives
    % go in the current directory only with --no-use-grade-subdir, this 
    % predicate is not well named. However, the transformation it performs
    % will become unnecessary once we switch over to using newexts exclusively
    % in the compiler, including at the call sites calling
    % module_name_to_file_name and its variants.
    %
    % XXX While separating newext_executable_gs from newext_executable
    % separating newext_library_gs from newext_library are needed to get 
    % test_file_name_extensions to report no discrepancies, bootchecking
    % the compiler with --use-grade-subdirs in a grade that targets C
    % fails as soons as it tries to build the first .c file in the library.
    % This is because --use-grade-subdirs is not *intended* to be actually
    % usable with mmake.
    %
    % According to the documentation of the --user-grade subdirs option,
    % *all* executables and libraries *should* be put into a grade subdir
    % if that option is specified, not just some. They should then be
    % copied or linked to the current directory.
    (
        % Executable files.
        % XXX The Ext = "" here is wrong. While an empty extension
        % *can* mean we are building the name of an executable,
        % it can also mean we are building the name of a phony Mmakefile
        % target for a library, such as libmer_std in the library
        % directory.
        ( ExtStr = ""
        ; ExtStr = ".bat"
        ),
        NewExt = newext_executable_gs(ExtStr)
    ;
        ExtStr = ".exe",
        NewExt = newext_executable(ExtStr)
    ;
        % Library files.
        ( ExtStr = ".$A"
        ; ExtStr = ".a"
        ; ExtStr = ".dll"
        ; ExtStr = ".init"
        ; ExtStr = ".jar"
        ),
        NewExt = newext_library_gs(ExtStr)
    ;
        ( ExtStr = ".$(EXT_FOR_SHARED_LIB)"
        ; ExtStr = ".dylib"     % references can be generated only via options
        ; ExtStr = ".lib"
        ; ExtStr = ".so"
        ),
        NewExt = newext_library(ExtStr)
    ;
        % Machine-dependent header files for generated C code.
        % XXX There is no good reason for .mh files to be treated differently
        % from .mih files.
        ExtStr = ".mh",
        NewExt = newext_mh(ExtStr)
    ;
        % Output files intended for use by the user.
        % The MLDS dump files with extensions .c_dump* and .mih_dump*
        % also fit into this category, but their filenames are constructed
        % by getting the filenames for the .c and .mih extensions
        % and adding a suffix to that.
        ( ExtStr = ".dependency_graph"
        ; ExtStr = ".err"
        ; ExtStr = ".hlds_dump"
        ; ExtStr = ".mlds_dump"
        ; ExtStr = ".order"
        ; ExtStr = ".ugly"
        ),
        NewExt = newext_user(ExtStr)
    ;
        ( ExtStr = ".defn_extents"
        ; ExtStr = ".defn_line_counts"
        ; ExtStr = ".defns"
        ; ExtStr = ".imports_graph"
        ; ExtStr = ".local_call_tree"
        ; ExtStr = ".local_call_tree_order"
        ; ExtStr = ".mode_constraints"
        ; ExtStr = ".order_trans_opt"
        ; ExtStr = ".type_repns"
        ; ExtStr = ".xml"
        ),
        NewExt = newext_user_ngs(ExtStr)
    ;
        % Mmake targets.
        ( ExtStr = ".all_int3s"
        ; ExtStr = ".all_ints"
        ; ExtStr = ".all_opts"
        ; ExtStr = ".all_trans_opts"
        ; ExtStr = ".check"
        ; ExtStr = ".classes"
        ; ExtStr = ".clean"
        ; ExtStr = ".depend"
        ; ExtStr = ".install_grade_hdrs"
        ; ExtStr = ".install_hdrs"
        ; ExtStr = ".install_ints"
        ; ExtStr = ".install_opts"
        ; ExtStr = ".int3s"
        ; ExtStr = ".ints"
        ; ExtStr = ".javas"
        ; ExtStr = ".opts"
        ; ExtStr = ".realclean"
        ; ExtStr = ".trans_opts"
        ),
        NewExt = newext_mmake_target(ExtStr)
    ).

    % Decide which ext_other extensions go in which directories.
    %
:- pred choose_subdir_name_new(globals::in, string::in, string::out) is det.

choose_subdir_name_new(Globals, ExtStr, SubDirName) :-
    ( if
        ( ExtStr = ".dir/*.o"
        ; ExtStr = ".dir/*.$O"
        )
    then
        SubDirName = "dirs"
    else if
        % _init.c, _init.cs, _init.o etc. files go in the cs, css, os etc
        % subdirectories.
        string.remove_prefix("_init.", ExtStr, ExtName)
    then
        SubDirName = ExtName ++ "s"
    else if
        globals.lookup_string_option(Globals, library_extension, LibExt),
        globals.lookup_string_option(Globals, shared_library_extension,
            SharedLibExt),
        % Static and shared libraries go in the `lib' subdirectory.
        ( ExtStr = LibExt
        ; ExtStr = SharedLibExt
        )
    then
        SubDirName = "lib"
    else if
        % The usual case: `*.foo' files go in the `foos' subdirectory.
        string.remove_prefix(".", ExtStr, ExtName)
    then
        SubDirName = ExtName ++ "s"
    else
        unexpected($pred, "unknown extension `" ++ ExtStr ++ "'")
    ).

:- pred make_file_name_new(globals::in, list(dir_name)::in, maybe_search::in,
    file_name::in, other_newext::in, list(string)::out, file_name::out) is det.

make_file_name_new(Globals, SubDirNames, _Search, BaseNameNoExt, OtherExt,
        DirComponents, FileName) :-
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    OtherExt = other_newext(ExtStr),
    ( if
        UseGradeSubdirs = yes,
        file_is_arch_or_grade_dependent_new(Globals, OtherExt)
    then
        make_grade_subdir_file_name_new(Globals, SubDirNames,
            BaseNameNoExt, ExtStr, DirComponents, FileName)
    else if
        UseSubdirs = yes
    then
        DirComponents = ["Mercury" | SubDirNames],
        FileName =
            glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr)
    else
        DirComponents = SubDirNames,
        FileName =
            glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr)
    ).

:- pred make_grade_subdir_file_name_new(globals::in, list(dir_name)::in,
    file_name::in, string::in, list(string)::out, file_name::out) is det.

make_grade_subdir_file_name_new(Globals, SubDirNames, BaseNameNoExt, ExtStr,
        DirComponents, FileName) :-
    grade_directory_component(Globals, Grade),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    % The extra "Mercury" is needed so we can use `--intermod-directory
    % Mercury/<grade>/<target_arch>' and `--c-include
    % Mercury/<grade>/<target_arch>' to find the local `.opt' and `.mih'
    % files without messing up the search for the files for installed
    % libraries.
    DirComponents = ["Mercury", Grade, TargetArch, "Mercury" | SubDirNames],
    FileName = glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr).

%---------------------%

:- pred file_is_arch_or_grade_dependent_new(globals::in, other_newext::in)
    is semidet.

file_is_arch_or_grade_dependent_new(Globals, OtherExt) :-
    OtherExt = other_newext(ExtStr),
    (
        file_is_arch_or_grade_dependent_2_new(ExtStr)
    ;
        globals.lookup_string_option(Globals, executable_file_extension,
            ExtStr)
    ;
        globals.lookup_string_option(Globals, library_extension, ExtStr)
    ;
        globals.lookup_string_option(Globals, shared_library_extension, ExtStr)
    ;
        some [ObjExtStr] (
            (
                globals.lookup_string_option(Globals,
                    object_file_extension, ObjExtStr)
            ;
                globals.lookup_string_option(Globals,
                    pic_object_file_extension, ObjExtStr)
            ),
            (
                ExtStr = ObjExtStr
            ;
                ExtStr = "_init" ++ ObjExtStr
            )
        )
    ).

:- pred file_is_arch_or_grade_dependent_2_new(string::in) is semidet.

    % The `.used' file isn't grade dependent itself, but it contains
    % information collected while compiling a grade-dependent `.c', `.cs',
    % etc file.
file_is_arch_or_grade_dependent_2_new(".dir").

:- pred valid_other_newext(other_newext::in) is semidet.

valid_other_newext(other_newext(ExtStr)) :-
    % We define what string is valid as an argument of ext/1 negatively:
    % any extension string is valid as an argument of ext/1 *unless*
    % it has some other representation.
    not (
        ExtStr = ".m"       % ext_src
    ;
        is_current_dir_extension_new(ExtStr, _)
    ;
        ( ExtStr = ".d"
        ; ExtStr = ".dv"
        ; ExtStr = ".dep"
        ; ExtStr = ".int0"
        ; ExtStr = ".int"
        ; ExtStr = ".int2"
        ; ExtStr = ".int3"
        ; ExtStr = ".date0"
        ; ExtStr = ".date"
        ; ExtStr = ".date3"
        ; ExtStr = ".opt"
        ; ExtStr = ".trans_opt"
        ; ExtStr = ".optdate"
        ; ExtStr = ".trans_opt_date"
        ; ExtStr = ".mih"
        ; ExtStr = ".class"
        ; ExtStr = ".java"
        ; ExtStr = ".java_date"
        ; ExtStr = ".cs"
        ; ExtStr = ".cs_date"
        ; ExtStr = ".c"
        ; ExtStr = ".c_date"
        ; ExtStr = ".o"
        ; ExtStr = ".pic_o"
        ; ExtStr = ".$O"
        ; ExtStr = "_init.c"
        ; ExtStr = "_init.$O"
        ; ExtStr = "_init.o"
        ; ExtStr = "_init.pic_o"
        ; ExtStr = ".mbc"
        ; ExtStr = ".bytedebug"
        ; ExtStr = ".analysis"
        ; ExtStr = ".analysis_date"
        ; ExtStr = ".analysis_status"
        ; ExtStr = ".imdg"
        ; ExtStr = ".request"
        ; ExtStr = ".module_dep"
        ; ExtStr = ".used"
        ; ExtStr = ".track_flags"
        ; ExtStr = ".prof"
        ; ExtStr = ".err_date"
        ; ExtStr = ".dir/*.$O"
        )
    ).

% XXX END OF CODE DUPLICATION

%---------------------------------------------------------------------------%

test_file_name_extensions(Globals0, Stream, !IO) :-
    TestModuleName = qualified(qualified(unqualified("abc"), "def"), "xyz"),
    Extensions = all_extensions(Globals0),
    make_cur_dir_globals(Globals0, GlobalsCur),
    globals.set_option(use_subdirs, bool(yes), GlobalsCur, GlobalsSub),
    globals.set_option(use_grade_subdirs, bool(yes),
        GlobalsSub, GlobalsGradeSub),

    test_extensions_loop(GlobalsCur, GlobalsSub, GlobalsGradeSub, Stream,
        TestModuleName, Extensions, no, SomeErrors, !IO),
    (
        SomeErrors = no
    ;
        SomeErrors = yes,
        io.set_exit_status(1, !IO)
    ).

:- pred make_cur_dir_globals(globals::in, globals::out) is det.

make_cur_dir_globals(!Globals) :-
    globals.set_option(executable_file_extension,
        string("exec_file_opt"), !Globals),
    globals.set_option(library_extension,
        string("lib_opt"), !Globals),
    globals.set_option(shared_library_extension,
        string("shlib_opt"), !Globals),
    globals.set_option(java_object_file_extension,
        string("java_obj_opt"), !Globals),
    globals.set_option(object_file_extension,
        string("obj_opt"), !Globals),
    globals.set_option(pic_object_file_extension,
        string("pic_obj_opt"), !Globals),
    globals.set_option(use_subdirs, bool(no), !Globals),
    globals.set_option(use_grade_subdirs, bool(no), !Globals).

:- pred test_extensions_loop(globals::in, globals::in, globals::in,
    io.text_output_stream::in, module_name::in, list(string)::in,
    bool::in, bool::out, io::di, io::uo) is det.

test_extensions_loop(_, _, _, _, _, [], !SomeError, !IO).
test_extensions_loop(GlobalsCur, GlobalsSub, GlobalsGradeSub, Stream,
        ModuleName, [Ext | Exts], !SomeError, !IO) :-
    test_one_extension(GlobalsCur, Stream, "cur", do_not_search,
        ModuleName, Ext, !SomeError, !IO),
    test_one_extension(GlobalsCur, Stream, "cur", do_search,
        ModuleName, Ext, !SomeError, !IO),
    test_one_extension(GlobalsSub, Stream, "sub", do_not_search,
        ModuleName, Ext, !SomeError, !IO),
    test_one_extension(GlobalsSub, Stream, "sub", do_search,
        ModuleName, Ext, !SomeError, !IO),
    test_one_extension(GlobalsGradeSub, Stream, "gsub", do_not_search,
        ModuleName, Ext, !SomeError, !IO),
    test_one_extension(GlobalsGradeSub, Stream, "gsub", do_search,
        ModuleName, Ext, !SomeError, !IO),
    test_extensions_loop(GlobalsCur, GlobalsSub, GlobalsGradeSub, Stream,
        ModuleName, Exts, !SomeError, !IO).

:- pred test_one_extension(globals::in, io.text_output_stream::in,
    string::in, maybe_search::in, module_name::in, string::in,
    bool::in, bool::out, io::di, io::uo) is det.

test_one_extension(Globals, Stream, GlobalsStr, MaybeSearch, ModuleName, Ext,
        !SomeError, !IO) :-
    From = "TEST",
    % The value of MkDir does not influence the filename generated
    % by module_name_to_file_name_ext.
    MkDir = do_not_create_dirs,
    module_name_to_file_name_ext(Globals, From, MaybeSearch, MkDir,
        ext_other(other_ext(Ext)), ModuleName, OldFileName, !IO),
    NewExt = make_new_extension(Ext),
    module_name_to_file_name_ext_new(Globals, From, MaybeSearch, MkDir,
        NewExt, ModuleName, NewFileName, !IO),
    ( if OldFileName = NewFileName then
        true
    else
        ( MaybeSearch = do_search,     MaybeSearchStr = "schy"
        ; MaybeSearch = do_not_search, MaybeSearchStr = "schn"
        ),
        io.format(Stream, "BAD FILENAME %s/%s/%-8s %s vs %s\n",
            [s(GlobalsStr), s(MaybeSearchStr), s("<" ++ Ext ++ ">:"),
            s(OldFileName), s(NewFileName)], !IO),
        !:SomeError = yes
    ).

%---------------------%

:- func all_extensions(globals) = list(string).

all_extensions(Globals) = AllExtensionStrs :-
    StrExtensions = string_extensions,
    OptionExtensions = option_extensions,
    StrOptionExtensions = string_option_extensions,
    list.map(globals.lookup_string_option(Globals),
        OptionExtensions, OptionExtensionStrs),
    MakeStrOptStr =
        ( pred(Str - Opt::in, StrOptStr::out) is det :-
            globals.lookup_string_option(Globals, Opt, OptStr),
            StrOptStr = Str ++ OptStr
        ),
    list.map(MakeStrOptStr,
        StrOptionExtensions, StrOptionExtensionStrs),
    AllExtensionStrs0 = StrExtensions ++ OptionExtensionStrs ++
        StrOptionExtensionStrs,
    % Depending on option values, some of OptionExtensionStrs and
    % StrOptionExtensionStrs may duplicate strings in StrExtensions.
    list.sort_and_remove_dups(AllExtensionStrs0, AllExtensionStrs).

:- func string_extensions = list(string).

% We don't test the .m extension, since that is handled separately
% by module_name_to_source_file_name.
string_extensions = 
    ["",
    ".$A",
    ".$O",
    ".a",
    ".all_int3s",
    ".all_ints",
    ".all_opts",
    ".all_trans_opts",
    ".analysis",
    ".analysis_date",
    ".analysis_status",
    ".bat",
    ".bytedebug",
    ".c",
    ".c_date",
    ".check",
    ".class",
    ".classes",
    ".clean",
    ".cs",
    ".cs_date",
    ".d",
    ".date",
    ".date0",
    ".date3",
    ".defn_extents",
    ".defn_line_counts",
    ".defns",
    ".dep",
    ".depend",
    ".dependency_graph",
    ".dir/*.$O",
    ".dll",
    ".dv",
    ".err",
    ".err_date",
    ".exe",
    ".hlds_dump",
    ".imdg",
    ".imports_graph",
    ".init",
    ".install_grade_hdrs",
    ".install_hdrs",
    ".install_ints",
    ".install_opts",
    ".int",
    ".int0",
    ".int2",
    ".int3",
    ".int3s",
    ".ints",
    ".jar",
    ".java",
    ".java_date",
    ".javas",
    ".local_call_tree",
    ".local_call_tree_order",
    ".mbc",
    ".mh",
    ".mih",
    ".mlds_dump",
    ".mode_constraints",
    ".module_dep",
    ".o",
    ".opt",
    ".optdate",
    ".opts",
    ".order",
    ".order_trans_opt",
    ".pic_o",
    ".prof",
    ".realclean",
    ".request",
    ".so",
    ".track_flags",
    ".trans_opt",
    ".trans_opt_date",
    ".trans_opts",
    ".type_repns",
    ".ugly",
    ".used",
    ".xml",
    "_init.$O",
    "_init.c",
    "_init.o",
    "_init.pic_o"].

:- func option_extensions = list(option).

option_extensions =
    [executable_file_extension,
    library_extension,
    java_object_file_extension,
    object_file_extension,
    pic_object_file_extension,
    shared_library_extension].

:- func string_option_extensions = assoc_list(string, option).

string_option_extensions =
    ["_init" - object_file_extension,
    "_init" - pic_object_file_extension].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The rest of this file is concerned with gathering and writing out
% statistical information for profiling.
%
% The profile we are building is a map from keys to values. Each key records
% the parameters of each call to module_name_to_file_name_general: which
% extension of which module are we trying to look up, and whether the process
% of looking up involves searching or making directories. The values record
% the filename that results from the lookup, and the number of times
% we have done the exact same lookup.
%
% After the profile is dumped into a file, the information in it can then be
% subject to different kinds of postprocessing.

:- type record_key
    --->    record_key(module_name, ext, maybe_search, maybe_create_dirs).

:- type record_value
    --->    record_value(string, int).

:- mutable(translations, map(record_key, record_value), map.init, ground,
    [untrailed, attach_to_io_state]).

:- pred record_translation(maybe_search::in, maybe_create_dirs::in, ext::in,
    module_name::in, string::in, io::di, io::uo) is det.

record_translation(Search, MkDir, Ext, ModuleName, FileName, !IO) :-
    get_translations(Translations0, !IO),
    Key = record_key(ModuleName, Ext, Search, MkDir),
    ( if map.search(Translations0, Key, Value0) then
        Value0 = record_value(ValueFileName, Count0),
        expect(unify(FileName, ValueFileName), $module,
            "FileName != ValueFileName"),
        Value = record_value(ValueFileName, Count0 + 1),
        map.det_update(Key, Value, Translations0, Translations)
    else
        Value = record_value(FileName, 1),
        map.det_insert(Key, Value, Translations0, Translations)
    ),
    set_translations(Translations, !IO).

%---------------------%

:- mutable(no_mkdirs, map(string, int), map.init, ground,
    [untrailed, attach_to_io_state]).
:- mutable(mkdirs, map(string, int), map.init, ground,
    [untrailed, attach_to_io_state]).

:- pred record_no_mkdir(string::in, io::di, io::uo) is det.

record_no_mkdir(DirName, !IO) :-
    get_no_mkdirs(NoMkDirs0, !IO),
    ( if map.search(NoMkDirs0, DirName, Count0) then
        map.det_update(DirName, Count0 + 1, NoMkDirs0, NoMkDirs)
    else
        map.det_insert(DirName, 1, NoMkDirs0, NoMkDirs)
    ),
    set_no_mkdirs(NoMkDirs, !IO).

:- pred record_mkdir(string::in, io::di, io::uo) is det.

record_mkdir(DirName, !IO) :-
    get_mkdirs(MkDirs0, !IO),
    ( if map.search(MkDirs0, DirName, Count0) then
        map.det_update(DirName, Count0 + 1, MkDirs0, MkDirs)
    else
        map.det_insert(DirName, 1, MkDirs0, MkDirs)
    ),
    set_mkdirs(MkDirs, !IO).

%---------------------%

write_translations_record_if_any(!IO) :-
    get_translations(Translations, !IO),
    get_no_mkdirs(NoMkDirs, !IO),
    get_mkdirs(MkDirs, !IO),
    ( if
        map.is_empty(Translations),
        map.is_empty(NoMkDirs),
        map.is_empty(MkDirs)
    then
        true
    else
        map.foldl4(gather_translation_stats, Translations,
            0, NumKeys, 0, NumLookups,
            map.init, ExtMap, map.init, ExtSchDirMap),
        io.open_append("/tmp/TRANSLATIONS_RECORD", Result, !IO),
        (
            Result = ok(Stream),
            io.format(Stream, "overall_stats %d %d\n",
                [i(NumKeys), i(NumLookups)], !IO),
            map.foldl(write_out_ext_entry(Stream), ExtMap, !IO),
            map.foldl(write_out_ext_sch_dir_entry(Stream), ExtSchDirMap, !IO),
            map.foldl(write_out_no_mkdirs_entry(Stream), NoMkDirs, !IO),
            map.foldl(write_out_mkdirs_entry(Stream), MkDirs, !IO),
            io.close_output(Stream, !IO)
        ;
            Result = error(_)
        )
    ).

%---------------------%

:- type count_sum
    --->    count_sum(int, int).

:- pred gather_translation_stats(record_key::in, record_value::in,
    int::in, int::out, int::in, int::out,
    map(string, count_sum)::in, map(string, count_sum)::out,
    map(string, count_sum)::in, map(string, count_sum)::out) is det.

gather_translation_stats(Key, Value, !NumKeys, !NumLookups,
        !ExtMap, !ExtSchDirMap) :-
    !:NumKeys = !.NumKeys + 1,
    Value = record_value(_FileName, Count),
    !:NumLookups = !.NumLookups + Count,
    Key = record_key(_ModuleName, Ext0, Search, MkDir),
    (
        Ext0 = ext_src,
        ExtStr = ".m"
    ;
        Ext0 = ext_other(other_ext(ExtStr0)),
        ( if ExtStr0 = "" then
            ExtStr = "no_suffix"
        else
            ExtStr = ExtStr0
        )
    ),
    (
        Search = do_search,
        SearchStr = "_search"
    ;
        Search = do_not_search,
        SearchStr = "_nosearch"
    ),
    (
        MkDir = do_create_dirs,
        MkDirStr = "_mkdir"
    ;
        MkDir = do_not_create_dirs,
        MkDirStr = "_nomkdir"
    ),
    ExtSchDir = ExtStr ++ SearchStr ++ MkDirStr,
    update_count_sum_map(ExtStr, Count, !ExtMap),
    update_count_sum_map(ExtSchDir, Count, !ExtSchDirMap).

:- pred update_count_sum_map(T::in, int::in,
    map(T, count_sum)::in, map(T, count_sum)::out) is det.

update_count_sum_map(Key, Count, !Map) :-
    ( if map.search(!.Map, Key, Entry0) then
        Entry0 = count_sum(Cnt0, Sum0),
        Entry = count_sum(Cnt0 + 1, Sum0 + Count),
        map.det_update(Key, Entry, !Map)
    else
        Entry = count_sum(1, Count),
        map.det_insert(Key, Entry, !Map)
    ).

:- pred write_out_ext_entry(io.text_output_stream::in,
    string::in, count_sum::in, io::di, io::uo) is det.

write_out_ext_entry(Stream, Ext, count_sum(Cnt, Sum), !IO) :-
    io.format(Stream, "ext %d %d %s\n", [i(Cnt), i(Sum), s(Ext)], !IO).

:- pred write_out_ext_sch_dir_entry(io.text_output_stream::in,
    string::in, count_sum::in, io::di, io::uo) is det.

write_out_ext_sch_dir_entry(Stream, ExtSchDir, count_sum(Cnt, Sum), !IO) :-
    io.format(Stream, "ext_sch_dir %d %d %s\n",
        [i(Cnt), i(Sum), s(ExtSchDir)], !IO).

%---------------------%

:- pred write_out_no_mkdirs_entry(io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

write_out_no_mkdirs_entry(Stream, DirName, Cnt, !IO) :-
    io.format(Stream, "no_dir_name %d %s\n", [i(Cnt), s(DirName)], !IO).

:- pred write_out_mkdirs_entry(io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

write_out_mkdirs_entry(Stream, DirName, Cnt, !IO) :-
    io.format(Stream, "dir_name %d %s\n", [i(Cnt), s(DirName)], !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.file_names.
%---------------------------------------------------------------------------%
