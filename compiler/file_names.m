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
% This module deals with the connections between module names and files.
%
%---------------------------------------------------------------------------%

:- module parse_tree.file_names.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.

:- import_module io.

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
%
% XXX This interface should be improved in two ways.
%
% - First, the argument order
%
%       module_name_to_file_name(Globals, Mkdir, Extension,
%           Module, FileName, !IO)
%
%   would make it possible to use list.map_foldl to convert a list of
%   module names to file names with a single call.
%   DONE, with the _ho variants.
%
% - Second, the implementation of this predicate effectively divides
%   the set of possible values of Extension into classes of extensions,
%   treating every extension in a given class the same way.
%
%   We should replace the simple string Extension argument with a more
%   structured specification of the extension, one that puts a wrapper
%   around the actual suffix indicating what class the extension falls in,
%   as in e.g. ec_library(".dylib"). For some classes, maybe the majority,
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
% XXX Given the wide variety of uses cases that choose_file_name has
% to handle for its callers, the only way to ensure that a diff implementing
% the above ideas handles *all* of those uses cases correctly is probably to
%
% - Gather a list of all the extensions choose_file_name is called with.
%
% - Set up a test environment with distinctively named directories in
%   all the relevant directory search options.
%
% - Invoke choose_file_name and module_name_to_file_name_general
%   with all possible combinations of 
%
%       subdir option
%       empty and distinctive nonempty base parent dirs
%       unqualified and qualified module names
%       extension
%       search
%       mkdir
%
%   and record the results as the baseline.
%
% - Repeat the exercise with the proposed replacement code, and
%   compare the results to the baseline.

:- type maybe_create_dirs
    --->    do_create_dirs
    ;       do_not_create_dirs.

    % Return the file name of the Mercury source for the given module.
    %
:- pred module_source_filename(globals::in, module_name::in, file_name::out,
    io::di, io::uo) is det.

    % module_name_to_file_name(Globals, Mkdir, Extension, Module, FileName,
    %   !IO):
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
:- pred module_name_to_file_name(globals::in, maybe_create_dirs::in,
    string::in, module_name::in, file_name::out, io::di, io::uo) is det.

    % module_name_to_search_file_name(Globals, Extension, Module, FileName,
    %   !IO):
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
:- pred module_name_to_search_file_name(globals::in,
    string::in, module_name::in, file_name::out, io::di, io::uo) is det.

:- type maybe_search
    --->    do_search
    ;       do_not_search.

    % module_name_to_lib_file_name(Globals, Prefix, Module, Extension, MkDir,
    %   FileName, !IO):
    %
    % Like module_name_to_file_name, but also allows a prefix.
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name(globals::in, string::in, module_name::in,
    string::in, maybe_create_dirs::in, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name(Globals, Module, FactTableFileName, Ext, MkDir,
    %   FileName, !IO):
    %
    % Returns the filename to use when compiling fact table files.
    % If `MkDir' is do_create_dirs, then create any directories needed.
    %
:- pred fact_table_file_name(globals::in, module_name::in, file_name::in,
    string::in, maybe_create_dirs::in, file_name::out, io::di, io::uo) is det.

    % extra_link_obj_file_name(Globals, Module, ExtraLinkObjName, Ext,
    %   MkDir, FileName, !IO):
    %
    % Returns the filename to use when compiling extra objects that must be
    % linked into the executable (currently used only for fact tables).
    % If `MkDir' is do_create_dirs, make any directories necessary.
    %
:- pred extra_link_obj_file_name(globals::in,module_name::in, file_name::in,
    string::in, maybe_create_dirs::in, file_name::out, io::di, io::uo) is det.

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

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

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

module_source_filename(Globals, ModuleName, SourceFileName, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs,
        ".m", ModuleName, SourceFileName, !IO).

%---------------------%

module_name_to_file_name(Globals, MkDir, Ext, ModuleName, FileName, !IO) :-
    module_name_to_file_name_general(Globals, do_not_search, MkDir,
        Ext, ModuleName, FileName, !IO).

%---------------------%

module_name_to_search_file_name(Globals, Ext, ModuleName, FileName, !IO) :-
    module_name_to_file_name_general(Globals, do_search, do_not_create_dirs,
        Ext, ModuleName, FileName, !IO).

%---------------------------------------------------------------------------%

:- pred module_name_to_file_name_general(globals::in,
    maybe_search::in, maybe_create_dirs::in, string::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name_general(Globals, Search, MkDir, Ext,
        ModuleName, FileName, !IO) :-
    ( if
        Ext = ".m"
    then
        % Look up the module in the module->file mapping.
        source_file_map.lookup_module_source_file(ModuleName, FileName, !IO)
    else
        ( if
            % Java files need to be placed into a package subdirectory
            % and may need mangling.
            ( string.suffix(Ext, ".java")
            ; string.suffix(Ext, ".class")
            )
        then
            EffectiveModuleName = ModuleName,
            BaseParentDirs = ["jmercury"],
            mangle_sym_name_for_java(ModuleName, module_qual, "__",
                BaseNameNoExt)
        else if
            % Erlang uses `.' as a package separator and expects a module
            % `a.b.c' to be in a file `a/b/c.erl'. Rather than that, we use
            % a flat namespace with `__' as module separators.
            ( string.suffix(Ext, ".erl")
            ; string.suffix(Ext, ".hrl")
            ; string.suffix(Ext, ".beam")
            )
        then
            EffectiveModuleName =
                qualify_mercury_std_library_module_name(ModuleName),
            BaseParentDirs = [],
            BaseNameNoExt = sym_name_to_string_sep(EffectiveModuleName, "__")
        else
            EffectiveModuleName = ModuleName,
            BaseParentDirs = [],
            BaseNameNoExt = sym_name_to_string_sep(ModuleName, ".")
        ),
        choose_file_name(Globals, EffectiveModuleName, BaseParentDirs,
            BaseNameNoExt, Ext, Search, MkDir, FileName, !IO)
    ),
    trace [compile_time(flag("file_name_translations")),
        runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
    (
        get_translations(Translations0, !TIO),
        Key = record_key(ModuleName, Ext, Search, MkDir),
        ( if map.search(Translations0, Key, Value0) then
            Value0 = record_value(ValueFileName, Count0),
            expect(unify(FileName, ValueFileName), $pred,
                "FileName != ValueFileName"),
            Value = record_value(ValueFileName, Count0 + 1),
            map.det_update(Key, Value, Translations0, Translations)
        else
            Value = record_value(FileName, 1),
            map.det_insert(Key, Value, Translations0, Translations)
        ),
        set_translations(Translations, !TIO)
    ).

module_name_to_lib_file_name(Globals, Prefix, ModuleName, Ext, MkDir,
        FileName, !IO) :-
    BaseFileName = sym_name_to_string(ModuleName),
    BaseNameNoExt = Prefix ++ BaseFileName,
    choose_file_name(Globals, ModuleName, [], BaseNameNoExt, Ext,
        do_not_search, MkDir, FileName, !IO).

fact_table_file_name(Globals, ModuleName, FactTableFileName, Ext, MkDir,
        FileName, !IO) :-
    choose_file_name(Globals, ModuleName, [], FactTableFileName, Ext,
        do_not_search, MkDir, FileName, !IO).

extra_link_obj_file_name(Globals, ModuleName, ExtraLinkObjName, Ext, MkDir,
        FileName, !IO) :-
    choose_file_name(Globals, ModuleName, [], ExtraLinkObjName, Ext,
        do_not_search, MkDir, FileName, !IO).

    % choose_file_name(ModuleName, BaseParentDirs, BaseName, Ext, Search,
    %   MkDir, FileName, !IO)
    %
    % BaseParentDirs is usually empty. For Java files, BaseParentDirs are the
    % package directories that the file needs to be placed in.
    %
:- pred choose_file_name(globals::in, module_name::in, list(string)::in,
    string::in, string::in, maybe_search::in, maybe_create_dirs::in,
    file_name::out, io::di, io::uo) is det.

choose_file_name(Globals, _ModuleName, BaseParentDirs, BaseNameNoExt, Ext,
        Search, MkDir, FileName, !IO) :-
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_string_option(Globals, library_extension, LibExt),
    globals.lookup_string_option(Globals, shared_library_extension,
        SharedLibExt),
    ( if
        % If we are searching for (rather than writing) a `.mih' file,
        % use the plain file name. This is so that searches for files
        % in installed libraries will work. `--c-include-directory' is
        % set so that searches for files in the current directory will work.
        % Similarly for `.hrl' files. We set `--erlang-include-directory'
        % for those.

        Search = do_search,
        ( Ext = ".mih"
        ; Ext = ".mih.tmp"
        ; Ext = ".hrl"
        ; Ext = ".hrl.tmp"
        )
    then
        FileName = BaseNameNoExt ++ Ext
    else if
        UseSubdirs = no
    then
        % Even if not putting files in a `Mercury' directory, Java files will
        % have non-empty BaseParentDirs (the package) which may need to be
        % created.
        % ZZZ Most of the code of make_file_name handles UseSubdirs = yes.
        make_file_name(Globals, BaseParentDirs, Search, MkDir,
            BaseNameNoExt, Ext, FileName, !IO)
    else if
        % The source files, the final executables, library files (including
        % .init files) output files intended for use by the user, and phony
        % Mmake targets names go in the current directory.

        not (
            UseGradeSubdirs = yes,
            file_is_arch_or_grade_dependent(Globals, Ext)
        ),
        (
            % Executable files.
            ( Ext = ""
            ; Ext = ".bat"
            ; Ext = ".exe"

            % Library files.
            ; Ext = ".a"
            ; Ext = ".$A"
            ; Ext = ".lib"
            ; Ext = ".so"
            ; Ext = ".dll"
            ; Ext = ".dylib"
            ; Ext = ".$(EXT_FOR_SHARED_LIB)"
            ; Ext = ".jar"
            ; Ext = ".beams"
            ; Ext = ".init"

            % mercury_update_interface requires the `.init.tmp' files to be
            % in the same directory as the `.init' files.
            ; Ext = ".init.tmp"

            % output files intended for use by the user (the .h_dump* and
            % .c_dump* MLDS dumps also fit into this category, but for
            % efficiency, to keep this as a switch, we deal with them below).
            ; Ext = ".mh"

            % mercury_update_interface requires the `.mh.tmp' files to be
            % in the same directory as the `.mh' files.
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
            ; Ext = ".ils"
            ; Ext = ".javas"
            ; Ext = ".classes"
            ; Ext = ".erls"
            ; Ext = ".beams"
            ; Ext = ".opts"
            ; Ext = ".trans_opts"
            ; Ext = ".all_ints"
            ; Ext = ".all_int3s"
            ; Ext = ".all_opts"
            ; Ext = ".all_trans_opts"
            )
        ;
            % Output files intended for use by the user.

            ( string.prefix(Ext, ".c_dump")
            ; string.prefix(Ext, ".mih_dump")
            )
        )
    then
        FileName = BaseNameNoExt ++ Ext
    else
        % We need to handle a few cases specially.

        ( if
            ( Ext = ".dir/*.o"
            ; Ext = ".dir/*.$O"
            )
        then
            SubDirName = "dirs"
        else if
            % .$O, .pic_o and .lpic_o files need to go in the same directory,
            % so that using .$(EXT_FOR_PIC_OBJECTS) will work.
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
        then
            SubDirName = "os"
        else if
            % _init.c, _init.s, _init.o etc. files go in the cs, ss, os etc
            % subdirectories.
            string.append("_init.", ExtName, Ext)
        then
            string.append(ExtName, "s", SubDirName)
        else if
            % .int.tmp, .opt.tmp, etc. files need to go in the ints, opts, etc
            % subdirectories.
            string.append(".", ExtName0, Ext),
            string.remove_suffix(ExtName0, ".tmp", ExtName)
        then
            string.append(ExtName, "s", SubDirName)
        else if
            % `.dv' files go in the `deps' subdirectory,
            % along with the `.dep' files
            Ext = ".dv"
        then
            SubDirName = "deps"
        else if
            % Static and shared libraries go in the `lib' subdirectory.
            ( Ext = LibExt
            ; Ext = SharedLibExt
            )
        then
            SubDirName = "lib"
        else if
            % The usual case: `*.foo' files go in the `foos' subdirectory.
            string.append(".", ExtName, Ext)
        then
            string.append(ExtName, "s", SubDirName)
        else if
            % Launcher scripts go in the `bin' subdirectory.
            Ext = ""
        then
            SubDirName = "bin"
        else
            unexpected($pred, "unknown extension `" ++ Ext ++ "'")
        ),

        make_file_name(Globals, [SubDirName | BaseParentDirs], Search, MkDir,
            BaseNameNoExt, Ext, FileName, !IO)
    ).

file_name_to_module_name(FileName, ModuleName) :-
    ModuleName = string_to_sym_name(FileName).

module_name_to_file_name_stem(ModuleName, FileName) :-
    FileName = sym_name_to_string(ModuleName).

module_name_to_make_var_name(ModuleName, MakeVarName) :-
    MakeVarName = sym_name_to_string(ModuleName).

:- pred make_file_name(globals::in, list(dir_name)::in, maybe_search::in,
    maybe_create_dirs::in, file_name::in, string::in, file_name::out,
    io::di, io::uo) is det.

make_file_name(Globals, SubDirNames, Search, MkDir, BaseNameNoExt, Ext,
        FileName, !IO) :-
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    ( if
        UseGradeSubdirs = yes,
        file_is_arch_or_grade_dependent(Globals, Ext),

        % If we are searching for (rather than writing) the file, just search
        % in Mercury/<ext>s. This is so that searches for files in installed
        % libraries work. `--intermod-directories' is set so this will work.

        not (
            Search = do_search,
            ( Ext = ".opt"
            ; Ext = ".trans_opt"
            ; Ext = ".analysis"
            ; Ext = ".imdg"
            ; Ext = ".request"
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
    (
        DirComponents = [],
        FileName = BaseNameNoExt ++ Ext
    ;
        DirComponents = [_ | _],
        (
            MkDir = do_create_dirs,
            DirName = dir.relative_path_name_from_components(DirComponents),
            make_directory(DirName, _, !IO)
        ;
            MkDir = do_not_create_dirs
        ),
        Components = DirComponents ++ [BaseNameNoExt ++ Ext],
        FileName = dir.relative_path_name_from_components(Components)
    ).

:- pred file_is_arch_or_grade_dependent(globals::in, string::in) is semidet.

file_is_arch_or_grade_dependent(Globals, Ext0) :-
    % for mercury_update_interface
    ( if string.remove_suffix(Ext0, ".tmp", BaseExt) then
        Ext = BaseExt
    else
        Ext = Ext0
    ),
    (
        file_is_arch_or_grade_dependent_2(Ext)
    ;
        globals.lookup_string_option(Globals, executable_file_extension, Ext)
    ;
        globals.lookup_string_option(Globals, library_extension, Ext)
    ;
        globals.lookup_string_option(Globals, shared_library_extension, Ext)
    ;
        some [ObjExt] (
            (
                globals.lookup_string_option(Globals,
                    object_file_extension, ObjExt)
            ;
                globals.lookup_string_option(Globals,
                    pic_object_file_extension, ObjExt)
            ),
            (
                Ext = ObjExt
            ;
                Ext = "_init" ++ ObjExt
            )
        )
    ).

:- pred file_is_arch_or_grade_dependent_2(string::in) is semidet.

    % The `.used' file isn't grade dependent itself, but it contains
    % information collected while compiling a grade-dependent `.c', `il',
    % etc file.
file_is_arch_or_grade_dependent_2("").
file_is_arch_or_grade_dependent_2(".bat").
file_is_arch_or_grade_dependent_2(".used").
file_is_arch_or_grade_dependent_2(".opt").
file_is_arch_or_grade_dependent_2(".optdate").
file_is_arch_or_grade_dependent_2(".trans_opt").
file_is_arch_or_grade_dependent_2(".trans_opt_date").
file_is_arch_or_grade_dependent_2(".analysis").
file_is_arch_or_grade_dependent_2(".analysis_date").
file_is_arch_or_grade_dependent_2(".analysis_status").
file_is_arch_or_grade_dependent_2(".imdg").
file_is_arch_or_grade_dependent_2(".track_flags").
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
file_is_arch_or_grade_dependent_2(".cs").
file_is_arch_or_grade_dependent_2(".cs_date").
file_is_arch_or_grade_dependent_2(".java").
file_is_arch_or_grade_dependent_2(".java_date").
file_is_arch_or_grade_dependent_2(".jar").
file_is_arch_or_grade_dependent_2(".class").
file_is_arch_or_grade_dependent_2(".erl").
file_is_arch_or_grade_dependent_2(".erl_date").
file_is_arch_or_grade_dependent_2(".beam").
file_is_arch_or_grade_dependent_2(".beams").
file_is_arch_or_grade_dependent_2(".hrl").
file_is_arch_or_grade_dependent_2(".dir").
file_is_arch_or_grade_dependent_2(".dll").
file_is_arch_or_grade_dependent_2(".$A").
file_is_arch_or_grade_dependent_2(".a").
file_is_arch_or_grade_dependent_2("_init.c").
file_is_arch_or_grade_dependent_2("_init.$O").
file_is_arch_or_grade_dependent_2("_init.erl").
file_is_arch_or_grade_dependent_2("_init.beam").

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

:- type record_key
    --->    record_key(module_name, string, maybe_search, maybe_create_dirs).

:- type record_value
    --->    record_value(string, int).

:- mutable(translations, map(record_key, record_value), map.init, ground,
    [untrailed, attach_to_io_state]).

write_translations_record_if_any(!IO) :-
    get_translations(Translations, !IO),
    ( if map.is_empty(Translations) then
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
            io.close_output(Stream, !IO)
        ;
            Result = error(_)
        )
    ).

:- type ext_search_mkdir
    --->    ext_search_mkdir(string, maybe_search, maybe_create_dirs).

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
    ( if Ext0 = "" then
        Ext = "no_suffix"
    else
        Ext = Ext0
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
    ExtSchDir = Ext ++ SearchStr ++ MkDirStr,
    update_count_sum_map(Ext, Count, !ExtMap),
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

%---------------------------------------------------------------------------%
:- end_module parse_tree.file_names.
%---------------------------------------------------------------------------%
