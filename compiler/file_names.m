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
% XXX This interface could be improved in several ways.
%
% - First, the implementation of this predicate effectively divides
%   the set of possible values of Ext into classes of extensions,
%   treating every extension in a given class the same way.
%   While the code handling some of classes accesses the filesystem,
%   the code handling some other classes does not. If we put the wrappers
%   for these two kinds of classes into two separate types, we could
%   have a version of this predicate for each type, one with and one
%   without an I/O state pair.
%
% - Second, calls which search for a source file for reading (that may not
%   exist) should be separated from calls that construct a file name
%   to write the file.
%

    % If the proper place for a file is in a subdirectory (e.g. Mercury/css),
    % but the subdirectory does not exist, which in this case may mean either
    %
    % - that Mercury exists but Mercury/css does not, or
    % - that Mercury does not exist, which of course means that Mercury/css
    %   does not exist either,
    %
    % values of this type tell the predicate they are given to whether it
    % should create any such missing directories.
    %
    % XXX Or, we could just return the path to the file (in the form of
    % a list of directory names) to our caller, and let *it* call
    % the predicate to create those directories (a non-maybe version of
    % maybe_create_dirs_on_path below). This way, we wouldn't have to pass
    % a I/O state pair to a filename translation predicate *just in case*
    % Mkdir is do_create_dirs.
    %
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
    %
    % XXX The function symbols of this type are misnamed. The code in
    % this module *never* does any searches. What this flag controls
    % is whether we return a "file name to search for in a list of dirs"
    % or a "file name where it is known exactly what directory it is in".
    % The former never has a directory name (as distinct from file name)
    % component; the latter may have some.
:- type maybe_search
    --->    do_search
    ;       do_not_search.

%---------------------%

% Values of the "ext" type partition the set of filename extensions
% that the Mercury compiler cares about into several categories.
% Generally speaking, the extensions in each category are treated
% the same by the module_name_to_* predicates below. (There are exceptions,
% but they should be eliminated soon).
%
% The two considerations that control which category a given extension
% belongs to are
%
% - the purpose of the files with that extension, and
% - how the placement of files with that extension in directories is affected
%   by the --use-subdir and --use-grade-subdir options being set.
%
% Generally speaking, the places where the files using a given extension
% (or category of extensions) will usually be either
%
% - the current directory,
% - a non-grade-specific subdirectory, which will be "Mercury/<X>s"
%   for some string X,
% - a grade-specific subdirectory, which will be
%   "Mercury/<grade>/<arch>/Mercury/<X>s" for some string X.
%   (See make_grade_subdir_file_name) for the rationale for this scheme.)
%
% Java extensions are an exception; they include an extra "jmercury" component
% in the path.
%
% There are several approaches we can use to decide which directory the files
% using an extension should be put into. The main ones are the following.
%
% - The files of some extensions are always stored in the current directory.
%
% - The files of some extensions are stored in a non-grade-specific
%   subdirectory if --use-subdirs is specified, and in the current directory
%   otherwise.
%
% - The files of some extensions are stored in a grade-specific subdirectory
%   if --use-grade-subdirs is specified, and in the current directory
%   otherwise.
%
% - The files of some extensions are stored in a grade-specific subdirectory
%   if --use-grade-subdirs is specified, in a non-grade-specified dubdirectory
%   if --use-grade-subdirs is not specified but --use-subdirs is, and
%   in the current directory otherwise.
%
% (Note that --use-grade-subdirs is not *intended* to be actually usable
% with mmake.)
%
% However, other approaches also exist, though they are all variations
% on these themes.
%
% One common variation is that when constructing a filename to search for,
% we never include any directory name component in the filename we return.
% XXX There are extension classes for which we *do* return directory name
% components even when constructing a filename to be searched for.
% It is not (yet) clear to me (zs) whether this is an intentional choice,
% or whether what we do with do_search is immaterial because we *always*
% translate those extensions with do_not_search.
%
% In the function symbols below,
% - the "gs" suffix stands for the use of a grade-specific directory, while
% - the "ngs" suffix stands for the use of a non-grade-specific directory.
%
% XXX We should probably invert the classification scheme.
% Instead of making the role (interface file, target file, object file, etc)
% be the primary division point, and the directory treatment the second
% division point, we should make the directory treatment the first one.
% However, there is no point in dong that until we have firmly, and
% *explicitly*, decided the directory treatment we want for each and every
% extension. (The current system was arrived at by piling patch upon patch
% on a large piece of over-complex code, and cannot be considered to
% represent the result of explicit deliberation.)

:- type ext
    --->    ext_src
            % Mercury source files.
            % The extension string is ".m".

    ;       ext_int(ext_int)
    ;       ext_opt(ext_opt)
            % Compiler-generated interface files and optimization files,
            % and the timestamp files showing when they were last checked.

    ;       ext_mh(ext_mh)
            % Compiler-generated header file for a module that is intended
            % for inclusion by user-written C source files.
            % The extension string is ".mh".

    ;       ext_mih(ext_mih)
            % Compiler-generated header file for a module that is intended
            % for inclusion by Mercury-generated C source files.
            % The extension string is ".mih".

    ;       ext_target_c_cs(ext_target_c_cs)
            % C and C# source files generated by the Mercury compiler.

    ;       ext_target_java(ext_target_java)
            % Java source files generated by the Mercury compiler.
            % The names of these files are computed by a different algorithm
            % from the names of C and C# source files.

    ;       ext_target_date(ext_target_date)
            % Timestamp files that record the date and time when a target
            % language (C, C# or Java) source files was last logically remade.
            % (The "logically" parts means that if the new, up-to-date version
            % is bit-for-bit identical to the old version, then we update
            % the timestamp file, but not the file it refers to.)

    ;       ext_target_obj(ext_obj)
            % Object files generated for C source files generated for a module
            % by the Mercury compiler.

    ;       ext_target_init_c(ext_init_c)
    ;       ext_target_init_obj(ext_init_obj)
            % C and object files associated not with a module but with
            % a whole program, containing the code needed to initialize
            % various tables for the runtime system.

    ;       ext_exec(ext_exec)
    ;       ext_exec_gs(ext_exec_gs)
            % Executables generated for a whole program.
            %
            % The ext_exec executables always go into the current directory.
            % The ext_exec_gs executables (the "gs" suffix stands for
            % "grade-specific")
            %
            % - have filenames in the current directory
            %   with --no-use-grade-subdirs,
            % - have filenames in the selected grade's subdir
            %   with --use-grade-subdirs.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.
            %
            % XXX According to the documentation of the --user-grade subdirs
            % option, *all* executables and libraries *should* be put
            % into a grade subdir if that option is specified, not just some.
            % They should then be copied or linked to the current directory.

    ;       ext_lib(ext_lib)
    ;       ext_lib_gs(ext_lib_gs)
            % Libraries, which may be statically or dynamically linked,
            % generated for a set of modules.
            %
            % The ext_lib libraries always go into the current directory.
            % The ext_lib_gs libraries (the "gs" suffix stands for
            % "grade-specific")
            %
            % - have filenames in the current directory
            %   with --no-use-grade-subdirs,
            % - have filenames in the selected grade's subdir
            %   with --use-grade-subdirs.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.
            %
            % XXX According to the documentation of the --user-grade subdirs
            % option, *all* executables and libraries *should* be put
            % into a grade subdir if that option is specified, not just some.
            % They should then be copied or linked to the current directory.

    ;       ext_mmake_fragment(ext_mmake_fragment)
            % Compiler-generated files that are designed to be bodily included
            % in Mmakefiles.

    ;       ext_mmake_target(ext_mmake_target)
            % These extensions are used not to create filenames, but to
            % create mmake target names. Some do refer to real files,
            % but they can (and some do) refer to these using extension
            % strings that can contain references to make variables.
            % Some of the other generated make targets are phony targets,
            % meaning that they never correspond to real files at all.

    ;       ext_user(ext_user)
    ;       ext_user_ngs(ext_user_ngs)
            % Compiler-generated files that are intended to be read
            % by the programmer.
            %
            % The ext_user files always go into the current directory.
            % The ext_user_ngs file (the "ngs" suffix stands for
            % "non-grade-specific")
            %
            % - have filenames in the current directory
            %   with --no-use-subdirs,
            % - have filenames in a subdirectory of the Mercury directory
            %   with --use-subdirs.

    ;       ext_analysis(ext_analysis)
            % Compiler-generated files that are part of the incomplete
            % attempt at an intermodule analysis and optimization framework
            % in analysis.m and its clients.

    ;       ext_bytecode(ext_bytecode)
            % Compiler-generated files that represent bytecode
            % for a long-ago attempt at a bytecode based Mercury debugger.

    ;       ext_misc_ngs(ext_misc_ngs)
    ;       ext_misc_gs(ext_misc_gs).
            % XXX Document me.

%-------------%

% Each extension specification is followed below
%
% - either by a string giving the extension (the usual case),
% - the name of an option giving the extension, or
% - a prefix and the name of an option giving the rest of the extension.

:- type ext_int
    --->    ext_int_int0                % ".int0"
    ;       ext_int_int1                % ".int"
    ;       ext_int_int2                % ".int2"
    ;       ext_int_int3                % ".int3"
    ;       ext_int_date_int0           % ".date0"
    ;       ext_int_date_int12          % ".date"
    ;       ext_int_date_int3.          % ".date3"

:- type ext_opt
    --->    ext_opt_plain               % ".opt"
    ;       ext_opt_trans               % ".trans_opt"
    ;       ext_opt_date_plain          % ".optdate"
    ;       ext_opt_date_trans.         % ".trace_opt_date"

:- type ext_mh
    --->    ext_mh_mh.                  % ".mh"

:- type ext_mih
    --->    ext_mih_mih.                % ".mih"

:- type ext_target_c_cs
    --->    ext_target_c                % ".c"
    ;       ext_target_cs.              % ".cs"

:- type ext_target_java
    --->    ext_target_java_java        % ".java"
    ;       ext_target_java_class.      % ".class"

:- type ext_target_date
    --->    ext_target_date_c           % ".c_date"
    ;       ext_target_date_cs          % ".cs_date"
    ;       ext_target_date_java.       % ".java_date"

:- type ext_obj
    --->    ext_obj_dollar_o            % ".$O"
    ;       ext_obj_dollar_efpo         % ".$(EXT_FOR_PIC_OBJECTS)"
    ;       ext_obj_o                   % ".o"
    ;       ext_obj_pic_o               % ".pic_o"
    ;       ext_obj_obj_opt             % object_file_extension option
    ;       ext_obj_pic_obj_opt.        % pic_object_file_extension option

:- type ext_init_c
    --->    ext_init_c.                 % ".init_c"

:- type ext_init_obj
    --->    ext_init_obj_dollar_o       % ".init.$O"
    ;       ext_init_obj_o              % ".init.c"
    ;       ext_init_obj_pic_o          % ".init.pic_o"
    ;       ext_init_obj_obj_opt        % "_init" ++ object_file_extension
    ;       ext_init_obj_pic_obj_opt.   % "_init" ++ pic_object_file_extension

:- type ext_exec
    --->    ext_exec_exe.               % ".exe"

:- type ext_exec_gs
    --->    ext_exec_gs_noext           % ""
            % XXX While an empty extension *usually means we are building
            % the name of an executable, it can also mean we are building
            % the name of a phony Mmakefile target for a library, such as
            % libmer_std in the library directory.
    ;       ext_exec_gs_bat             % ".bat"
    ;       ext_exec_exec_opt.          % executable_file_extension

:- type ext_lib
    --->    ext_lib_dollar_efsl         % ".(EXT_FOR_SHARED_LIB)"
    ;       ext_lib_lib                 % ".lib"
    ;       ext_lib_so.                 % ".so"

:- type ext_lib_gs
    --->    ext_lib_gs_dollar_a         % ".$A"
    ;       ext_lib_gs_archive          % ".a"
    ;       ext_lib_gs_dll              % ".dll"
    ;       ext_lib_gs_init             % ".init"
    ;       ext_lib_gs_jar              % ".jar"
    ;       ext_lib_gs_lib_opt          % library_extension
    ;       ext_lib_gs_sh_lib_opt.      % shared_library_extension

:- type ext_mmake_fragment
    --->    ext_mf_d                    % ".d"
    ;       ext_mf_dv                   % ".dv"
    ;       ext_mf_dep                  % ".dep"
    % XXX DODGY This extension is use in only one place, in write_deps_file.m.
    % It looks strange to me (zs), because I have no idea where ".dir"
    % comes from, or what system component puts object files there.
    ;       ext_mf_dir_sl_all_os.       % ".dir/*.$O"

:- type ext_mmake_target
    --->    ext_mt_all_int3s            % ".all_int3s"
    ;       ext_mt_all_ints             % ".all_int3"
    ;       ext_mt_all_opts             % ".all_opts"
    ;       ext_mt_all_trans_opts       % ".all_trans_opts"
    ;       ext_mt_check                % ".check"
    ;       ext_mt_classes              % ".classes"
    ;       ext_mt_clean                % ".clean"
    ;       ext_mt_depend               % ".depend"
    ;       ext_mt_install_grade_hdrs   % ".install_grade_hdrs"
    ;       ext_mt_install_hdrs         % ".install_hdrs"
    ;       ext_mt_install_ints         % ".install_ints"
    ;       ext_mt_install_opts         % ".install_opts"
    ;       ext_mt_int3s                % ".int3s"
    ;       ext_mt_ints                 % ".ints"
    ;       ext_mt_javas                % ".javas"
    ;       ext_mt_opts                 % ".opts"
    ;       ext_mt_realclean            % ".realclean"
    ;       ext_mt_trans_opts.          % ".trans_opts"

:- type ext_user
    --->    ext_user_depgraph           % ".dependency_graph"
    ;       ext_user_err                % ".err"
    ;       ext_user_hlds_dump          % ".hlds_dump"
    ;       ext_user_mlds_dump          % ".mlds_dump"
    ;       ext_user_order              % ".order"
    ;       ext_user_ugly.              % ".ugly"

:- type ext_user_ngs
    --->    ext_user_ngs_defn_ext       % ".defn_extents"
    ;       ext_user_ngs_defn_lc        % ".defn_line_counts"
    ;       ext_user_ngs_defns          % ".defns"
    ;       ext_user_ngs_imports_graph  % ".imports_graph"
    ;       ext_user_ngs_lct            % ".local_call_tree"
    ;       ext_user_ngs_lct_order      % ".local_call_tree_order"
    ;       ext_user_ngs_mode_constr    % ".mode_constraints"
    ;       ext_user_ngs_order_to       % ".order_trans_opt"
    ;       ext_user_ngs_type_repns     % ".type_repns"
    ;       ext_user_ngs_xml.           % ".xnl"

:- type ext_analysis
    --->    ext_an_analysis             % ".analysis"
    ;       ext_an_date                 % ".analysis_date"
    ;       ext_an_status               % ".analysis_status"
    ;       ext_an_imdg                 % ".imdg"
    ;       ext_an_request.             % ".request"

:- type ext_bytecode
    --->    ext_bc_mbc                  % ".bc"
    ;       ext_bc_bytedebug.           % ".bytedebug"

% XXX The extensions in the two misc categories probably belong
% in one or another category above, but I (zs) am not sure where
% each *would* belong.

:- type ext_misc_ngs
    --->    ext_misc_ngs_module_dep     % ".module_dep"
            % XXX DODGY What is the correctness argument for making this
            % a NON-grade-specific extension? If *anything* in a .module_dep
            % file can *ever* be grade dependent, this should be a
            % grade-specific extension.
    ;       ext_misc_ngs_err_date       % ".err_date"
            % XXX DODGY If you recompile a module in a different grade,
            % the contents of the .err file may change, for example
            % because one grade satisfies the requirements of a
            % require_feature_set declaration and the other does not.
            % To me (zs), this argues in favor of .err_date files
            % belonging in a grade-specific directory. The fact that
            % it would obviously be harder to people to find them there
            % is no relevant, since people shouldn't *have* to find
            % .err_date files.
    ;       ext_misc_ngs_prof.          % ".prof"
            % XXX DODGY Given that different profiling grades generate
            % different profiles (specifically, they produce different subsets
            % of the whole set of kinds of info that the non-deep profiler can
            % generate), shouldn't this be a grade-specific extension?

:- type ext_misc_gs
    --->    ext_misc_gs_used            % ".used"
    ;       ext_misc_gs_track_flags.    % ".track_flags"

:- func extension_to_string(globals, ext) = string.

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
    maybe_create_dirs::in, ext::in, module_name::in, file_name::out,
    io::di, io::uo) is det.

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
:- pred module_name_to_search_file_name(globals::in, string::in,
    ext::in, module_name::in, file_name::out, io::di, io::uo) is det.

    % module_name_to_lib_file_name(Globals, MkDir, Prefix, Ext,
    %   Module, FileName, !IO):
    %
    % Like module_name_to_file_name, but also allows a prefix.
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name(globals::in, string::in,
    maybe_create_dirs::in, string::in, ext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name(Globals, MkDir, Ext, FactTableFileName,
    %   FileName, !IO):
    %
    % Returns the filename to use when compiling fact table files.
    % If `MkDir' is do_create_dirs, then create any directories needed.
    %
:- pred fact_table_file_name(globals::in, string::in, maybe_create_dirs::in,
    ext::in, file_name::in, file_name::out, io::di, io::uo) is det.

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

    % This predicate is intended to output profiling data that can later
    % be used to improve the operation of this module. It appends to
    % /tmp/TRANSLATIONS_RECORD information about the frequency with which
    % this module is asked to translate file names with various suffixes,
    % provided that the gathering of this information has been enabled by
    % both the right trace flag at compile time and the right environment
    % variable at runtime.
    %
:- pred write_translations_record_if_any(globals::in, io::di, io::uo) is det.

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
:- import_module maybe.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

extension_to_string(Globals, Ext) = ExtStr :-
    (
        Ext = ext_src,
        ExtStr = ".m"
    ;
        Ext = ext_int(ExtInt),
        ext_int_extension_dir(ExtInt, ExtStr, _SubDirName)
    ;
        Ext = ext_opt(ExtOpt),
        ext_opt_extension_dir(ExtOpt, ExtStr, _SubDirName)
    ;
        Ext = ext_mh(ExtMh),
        ext_mh_extension(ExtMh, ExtStr)
    ;
        Ext = ext_mih(ExtMh),
        ext_mih_extension_dir(ExtMh, ExtStr, _SubDirName)
    ;
        Ext = ext_target_c_cs(ExtCCs),
        ext_target_c_cs_extension_dir(ExtCCs, ExtStr, _SubDirName)
    ;
        Ext = ext_target_java(ExtJava),
        ext_target_java_extension_dirs(ExtJava, ExtStr, _SubDirNames)
    ;
        Ext = ext_target_date(ExtTargetDate),
        ext_target_date_extension_dir(ExtTargetDate, ExtStr, _SubDirName)
    ;
        Ext = ext_target_obj(ExtObj),
        ext_obj_extension_dir(Globals, ExtObj, ExtStr, _SubDirName)
    ;
        Ext = ext_target_init_c(ExtInitC),
        ext_init_c_extension_dir(ExtInitC, ExtStr, _SubDirName)
    ;
        Ext = ext_target_init_obj(ExtInitObj),
        ext_init_obj_extension_dir(Globals, ExtInitObj, ExtStr, _SubDirName)
    ;
        Ext = ext_exec(ExtExec),
        ext_exec_extension(ExtExec, ExtStr)
    ;
        Ext = ext_exec_gs(ExtExecGs),
        ext_exec_gs_extension_dir(Globals, ExtExecGs, ExtStr, _SubDirName)
    ;
        Ext = ext_lib(ExtLib),
        ext_lib_extension(ExtLib, ExtStr)
    ;
        Ext = ext_lib_gs(ExtLibGs),
        ext_lib_gs_extension_dir(Globals, ExtLibGs, ExtStr, _SubDirName)
    ;
        Ext = ext_mmake_fragment(ExtMf),
        ext_mmake_fragment_extension_dir(ExtMf, ExtStr, _SubDirName)
    ;
        Ext = ext_mmake_target(ExtMT),
        ext_mmake_target_extension(ExtMT, ExtStr)
    ;
        Ext = ext_user(ExtUser),
        ext_user_extension(ExtUser, ExtStr)
    ;
        Ext = ext_user_ngs(ExtUserNgs),
        ext_user_ngs_extension_dir(ExtUserNgs, ExtStr, _SubDirName)
    ;
        Ext = ext_analysis(ExtAn),
        ext_analysis_extension_dir(ExtAn, ExtStr, _SubDirName)
    ;
        Ext = ext_bytecode(ExtByte),
        ext_bytecode_extension_dir(ExtByte, ExtStr, _SubDirName)
    ;
        Ext = ext_misc_ngs(ExtMiscNgs),
        ext_misc_ngs_extension_dir(ExtMiscNgs, ExtStr, _SubDirName)
    ;
        Ext = ext_misc_gs(ExtMiscGs),
        ext_misc_gs_extension_dir(ExtMiscGs, ExtStr, _SubDirName)
    ).

%---------------------------------------------------------------------------%

:- pred ext_int_extension_dir(ext_int::in, string::out, string::out) is det.

ext_int_extension_dir(ext_int_int0,         ".int0",    "int0s").
ext_int_extension_dir(ext_int_int1,         ".int",     "ints").
ext_int_extension_dir(ext_int_int2,         ".int2",    "int2s").
ext_int_extension_dir(ext_int_int3,         ".int3",    "int3s").
ext_int_extension_dir(ext_int_date_int0,    ".date0",   "date0s").
ext_int_extension_dir(ext_int_date_int12,   ".date",    "dates").
ext_int_extension_dir(ext_int_date_int3,    ".date3",   "date3s").

:- pred ext_opt_extension_dir(ext_opt::in, string::out, string::out) is det.

ext_opt_extension_dir(ext_opt_plain,
    ".opt",            "opts").
ext_opt_extension_dir(ext_opt_trans,
    ".trans_opt",      "trans_opts").
ext_opt_extension_dir(ext_opt_date_plain,
    ".optdate",        "optdates").
ext_opt_extension_dir(ext_opt_date_trans,
    ".trans_opt_date", "trans_opt_dates").

:- pred ext_mh_extension(ext_mh::in, string::out) is det.

ext_mh_extension(ext_mh_mh, ".mh").

:- pred ext_mih_extension_dir(ext_mih::in, string::out, string::out) is det.

ext_mih_extension_dir(ext_mih_mih, ".mih", "mihs").

:- pred ext_target_c_cs_extension_dir(ext_target_c_cs::in,
    string::out, string::out) is det.

ext_target_c_cs_extension_dir(ext_target_c,     ".c",    "cs").
ext_target_c_cs_extension_dir(ext_target_cs,    ".cs",    "css").

:- pred ext_target_java_extension_dirs(ext_target_java::in,
    string::out, list(string)::out) is det.

ext_target_java_extension_dirs(ext_target_java_java,
    ".java",    ["javas", "jmercury"]).
ext_target_java_extension_dirs(ext_target_java_class,
    ".class",   ["classs", "jmercury"]).

:- pred ext_target_date_extension_dir(ext_target_date::in,
    string::out, string::out) is det.

ext_target_date_extension_dir(ext_target_date_c,
    ".c_date",      "c_dates").
ext_target_date_extension_dir(ext_target_date_cs,
    ".cs_date",     "cs_dates").
ext_target_date_extension_dir(ext_target_date_java,
    ".java_date",   "java_dates").

:- pred ext_obj_extension_dir(globals::in, ext_obj::in,
    string::out, string::out) is det.

% Both deviations below from the "delete initial dot, add final 's'" rule
% are intentional.
ext_obj_extension_dir(_, ext_obj_dollar_o,   ".$O",     "os").
ext_obj_extension_dir(_, ext_obj_dollar_efpo, ".$(EXT_FOR_PIC_OBJECTS)", "os").
ext_obj_extension_dir(_, ext_obj_o,          ".o",      "os").
ext_obj_extension_dir(_, ext_obj_pic_o,      ".pic_o",  "os").
ext_obj_extension_dir(Globals, ext_obj_obj_opt, ExtStr, "os") :-
    globals.lookup_string_option(Globals, object_file_extension, ExtStr).
ext_obj_extension_dir(Globals, ext_obj_pic_obj_opt, ExtStr, "os") :-
    globals.lookup_string_option(Globals, pic_object_file_extension, ExtStr).

:- pred ext_init_c_extension_dir(ext_init_c::in,
    string::out, string::out) is det.

% The deviation below from the "delete initial dot, add final 's'" rule
% is intentional.
ext_init_c_extension_dir(ext_init_c, "_init.c", "cs").

:- pred ext_init_obj_extension_dir(globals::in, ext_init_obj::in,
    string::out, string::out) is det.

% All the deviations below from the "delete initial dot, add final 's'" rule
% are intentional.
ext_init_obj_extension_dir(_, ext_init_obj_dollar_o,  "_init.$O",     "os").
ext_init_obj_extension_dir(_, ext_init_obj_o,         "_init.o",      "os").
ext_init_obj_extension_dir(_, ext_init_obj_pic_o,     "_init.pic_o",  "os").
ext_init_obj_extension_dir(Globals, ext_init_obj_obj_opt, ExtStr, "os") :-
    globals.lookup_string_option(Globals, object_file_extension, ExtStr0),
    ExtStr = "_init" ++ ExtStr0.
ext_init_obj_extension_dir(Globals, ext_init_obj_pic_obj_opt, ExtStr, "os") :-
    globals.lookup_string_option(Globals, pic_object_file_extension, ExtStr0),
    ExtStr = "_init" ++ ExtStr0.

:- pred ext_exec_extension(ext_exec::in, string::out) is det.

ext_exec_extension(ext_exec_exe, ".exe").

:- pred ext_exec_gs_extension_dir(globals::in, ext_exec_gs::in,
    string::out, string::out) is det.

% Launcher scripts go in the `bin' subdirectory.
ext_exec_gs_extension_dir(_, ext_exec_gs_noext,    "",     "bin").
ext_exec_gs_extension_dir(_, ext_exec_gs_bat,      ".bat", "bats").
ext_exec_gs_extension_dir(Globals, ext_exec_exec_opt, ExtStr, "bin") :-
    globals.lookup_string_option(Globals, executable_file_extension, ExtStr).

:- pred ext_lib_extension(ext_lib::in, string::out) is det.

ext_lib_extension(ext_lib_dollar_efsl, ".$(EXT_FOR_SHARED_LIB)").
ext_lib_extension(ext_lib_lib,          ".lib").
ext_lib_extension(ext_lib_so,           ".so").

:- pred ext_lib_gs_extension_dir(globals::in, ext_lib_gs::in,
    string::out, string::out) is det.

% XXX EXT While "$As" follows the rule: "delete initial dot, add final 's'",
% it is *extremely unlikely* to be acceptable directory name.
ext_lib_gs_extension_dir(_, ext_lib_gs_dollar_a,   ".$A",      "$As").
ext_lib_gs_extension_dir(_, ext_lib_gs_archive,    ".a",       "as").
ext_lib_gs_extension_dir(_, ext_lib_gs_dll,        ".dll",     "dlls").
ext_lib_gs_extension_dir(_, ext_lib_gs_init,       ".init",    "inits").
ext_lib_gs_extension_dir(_, ext_lib_gs_jar,        ".jar",     "jars").
ext_lib_gs_extension_dir(Globals, ext_lib_gs_lib_opt, ExtStr, "lib") :-
    globals.lookup_string_option(Globals, library_extension, ExtStr).
ext_lib_gs_extension_dir(Globals, ext_lib_gs_sh_lib_opt, ExtStr, "lib") :-
    globals.lookup_string_option(Globals, shared_library_extension, ExtStr).

:- pred ext_mmake_fragment_extension_dir(ext_mmake_fragment::in,
    string::out, string::out) is det.

% Both deviations below from the "delete initial dot, add final 's'" rule
% are intentional, though I (zs) don't know the reason for the second.
ext_mmake_fragment_extension_dir(ext_mf_d,      ".d",   "ds").
ext_mmake_fragment_extension_dir(ext_mf_dv,     ".dv",  "deps").
ext_mmake_fragment_extension_dir(ext_mf_dep,    ".dep", "deps").
ext_mmake_fragment_extension_dir(ext_mf_dir_sl_all_os,
    ".dir/*.$O", "dirs").

:- pred ext_mmake_target_extension(ext_mmake_target::in, string::out) is det.

ext_mmake_target_extension(ext_mt_all_int3s,        ".all_int3s").
ext_mmake_target_extension(ext_mt_all_ints,         ".all_ints").
ext_mmake_target_extension(ext_mt_all_opts,         ".all_opts").
ext_mmake_target_extension(ext_mt_all_trans_opts,   ".all_trans_opts").
ext_mmake_target_extension(ext_mt_check,            ".check").
ext_mmake_target_extension(ext_mt_classes,          ".classes").
ext_mmake_target_extension(ext_mt_clean,            ".clean").
ext_mmake_target_extension(ext_mt_depend,           ".depend").
ext_mmake_target_extension(ext_mt_install_grade_hdrs, ".install_grade_hdrs").
ext_mmake_target_extension(ext_mt_install_hdrs,     ".install_hdrs").
ext_mmake_target_extension(ext_mt_install_ints,     ".install_ints").
ext_mmake_target_extension(ext_mt_install_opts,     ".install_opts").
ext_mmake_target_extension(ext_mt_int3s,            ".int3s").
ext_mmake_target_extension(ext_mt_ints,             ".ints").
ext_mmake_target_extension(ext_mt_javas,            ".javas").
ext_mmake_target_extension(ext_mt_opts,             ".opts").
ext_mmake_target_extension(ext_mt_realclean,        ".realclean").
ext_mmake_target_extension(ext_mt_trans_opts,       ".trans_opts").

:- pred ext_user_extension(ext_user::in, string::out) is det.

ext_user_extension(ext_user_depgraph,   ".dependency_graph").
ext_user_extension(ext_user_err,        ".err").
ext_user_extension(ext_user_hlds_dump,  ".hlds_dump").
ext_user_extension(ext_user_mlds_dump,  ".mlds_dump").
ext_user_extension(ext_user_order,      ".order").
ext_user_extension(ext_user_ugly,       ".ugly").

:- pred ext_user_ngs_extension_dir(ext_user_ngs::in, string::out, string::out)
    is det.

ext_user_ngs_extension_dir(ext_user_ngs_defn_ext,
    ".defn_extents", "defn_extentss").
ext_user_ngs_extension_dir(ext_user_ngs_defn_lc,
    ".defn_line_counts", "defn_line_countss").
ext_user_ngs_extension_dir(ext_user_ngs_defns,
    ".defns", "defnss").
ext_user_ngs_extension_dir(ext_user_ngs_imports_graph,
    ".imports_graph", "imports_graphs").
ext_user_ngs_extension_dir(ext_user_ngs_lct,
    ".local_call_tree", "local_call_trees").
ext_user_ngs_extension_dir(ext_user_ngs_lct_order,
    ".local_call_tree_order", "local_call_tree_orders").
ext_user_ngs_extension_dir(ext_user_ngs_mode_constr,
    ".mode_constraints", "mode_constraintss").
ext_user_ngs_extension_dir(ext_user_ngs_order_to,
    ".order_trans_opt", "order_trans_opts").
ext_user_ngs_extension_dir(ext_user_ngs_type_repns,
    ".type_repns", "type_repnss").
ext_user_ngs_extension_dir(ext_user_ngs_xml,
    ".xml", "xmls").

:- pred ext_analysis_extension_dir(ext_analysis::in,
    string::out, string::out) is det.

ext_analysis_extension_dir(ext_an_analysis,
    ".analysis",        "analysiss").
ext_analysis_extension_dir(ext_an_date,
    ".analysis_date",   "analysis_dates").
ext_analysis_extension_dir(ext_an_status,
    ".analysis_status", "analysis_statuss").
ext_analysis_extension_dir(ext_an_imdg,
    ".imdg",            "imdgs").
ext_analysis_extension_dir(ext_an_request,
    ".request",         "requests").

:- pred ext_bytecode_extension_dir(ext_bytecode::in,
    string::out, string::out) is det.

ext_bytecode_extension_dir(ext_bc_mbc,          ".mbc",         "mbcs").
ext_bytecode_extension_dir(ext_bc_bytedebug,    ".bytedebug",   "bytedebugs").

:- pred ext_misc_ngs_extension_dir(ext_misc_ngs::in,
    string::out, string::out) is det.

ext_misc_ngs_extension_dir(ext_misc_ngs_module_dep,
    ".module_dep", "module_deps").
ext_misc_ngs_extension_dir(ext_misc_ngs_err_date,
    ".err_date", "err_dates").
ext_misc_ngs_extension_dir(ext_misc_ngs_prof,
    ".prof", "profs").

:- pred ext_misc_gs_extension_dir(ext_misc_gs::in,
    string::out, string::out) is det.

ext_misc_gs_extension_dir(ext_misc_gs_used,
    ".used",        "useds").
ext_misc_gs_extension_dir(ext_misc_gs_track_flags,
    ".track_flags", "track_flagss").

%---------------------------------------------------------------------------%
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
    module_name_to_file_name_ext(Globals, From, do_search,
        do_not_create_dirs, Ext, ModuleName, FileName, !IO).

module_name_to_lib_file_name(Globals, From, MkDir, Prefix, Ext,
        ModuleName, FileName, !IO) :-
    BaseFileName = sym_name_to_string(ModuleName),
    BaseNameNoExt = Prefix ++ BaseFileName,
    FakeModuleName = unqualified(BaseNameNoExt),
    module_name_to_file_name_ext(Globals, From, do_not_search, MkDir,
        Ext, FakeModuleName, FileName, !IO).

fact_table_file_name(Globals, From, MkDir, Ext,
        FactTableFileName, FileName, !IO) :-
    FakeModuleName = unqualified(FactTableFileName),
    module_name_to_file_name_ext(Globals, From, do_not_search, MkDir,
        Ext, FakeModuleName, FileName, !IO).

%---------------------------------------------------------------------------%

:- pred module_name_to_file_name_ext(globals::in, string::in,
    maybe_search::in, maybe_create_dirs::in, ext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name_ext(Globals, From, Search, MkDir, Ext,
        ModuleName, FileName, !IO) :-
    (
        Ext = ext_src,
        module_name_to_source_file_name(ModuleName, FileName, !IO)
    ;
        (
            Ext = ext_int(ExtInt),
            ext_int_extension_dir(ExtInt, ExtStr, SubDirName)
        ;
            Ext = ext_user_ngs(ExtUserNgs),
            ext_user_ngs_extension_dir(ExtUserNgs, ExtStr, SubDirName)
        ;
            Ext = ext_bytecode(ExtByte),
            ext_bytecode_extension_dir(ExtByte, ExtStr, SubDirName)
        ;
            Ext = ext_misc_ngs(ExtMiscNgs), % XXX Probably never used.
            ext_misc_ngs_extension_dir(ExtMiscNgs, ExtStr, SubDirName)
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            DirComponents = ["Mercury", SubDirName],
            FileName =
                glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = ext_opt(ExtOpt),
        ext_opt_extension_dir(ExtOpt, ExtStr, SubDirName),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
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
                    ( ExtOpt = ext_opt_plain
                    ; ExtOpt = ext_opt_trans
                    )
                )
            then
                make_grade_subdir_file_name(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            else
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        (
            Ext = ext_mh(ExtMh),
            ext_mh_extension(ExtMh, ExtStr)
        ;
            Ext = ext_exec(ExtExec),
            ext_exec_extension(ExtExec, ExtStr)
        ;
            Ext = ext_lib(ExtLib),
            ext_lib_extension(ExtLib, ExtStr)
        ;
            Ext = ext_mmake_target(ExtMT),
            ext_mmake_target_extension(ExtMT, ExtStr)
        ;
            Ext = ext_user(ExtUser),
            ext_user_extension(ExtUser, ExtStr)
        ),
        % Output files intended for use by the user, and phony Mmake target
        % names go in the current directory. So do .mh files, and *some*,
        % but not all, kinds of executable and library files.
        % XXX Why is that?
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        FileName = BaseNameNoExt ++ ExtStr
    ;
        Ext = ext_mih(ExtMh),
        ext_mih_extension_dir(ExtMh, ExtStr, SubDirName),
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
                globals.lookup_bool_option(Globals, use_grade_subdirs,
                    UseGradeSubdirs),
                (
                    UseGradeSubdirs = no,
                    DirComponents = ["Mercury", SubDirName],
                    FileName = glue_dir_names_file_name(DirComponents,
                        BaseNameNoExt, ExtStr)
                ;
                    UseGradeSubdirs = yes,
                    make_grade_subdir_file_name(Globals, [SubDirName],
                        BaseNameNoExt, ExtStr, DirComponents, FileName)
                ),
                maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
            )
        )
    ;
        (
            Ext = ext_target_c_cs(ExtCCs),
            ext_target_c_cs_extension_dir(ExtCCs, ExtStr, SubDirName)
        ;
            Ext = ext_target_date(ExtTargetDate),
            ext_target_date_extension_dir(ExtTargetDate, ExtStr, SubDirName)
        ;
            Ext = ext_misc_gs(ExtMiscGs),
            ext_misc_gs_extension_dir(ExtMiscGs, ExtStr, SubDirName)
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = ext_target_java(ExtJava),
        ext_target_java_extension_dirs(ExtJava, ExtStr, SubDirNames),
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
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury" |  SubDirNames],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name(Globals, SubDirNames,
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            )
        ),
        maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
    ;
        (
            Ext = ext_target_obj(ExtObj),
            ext_obj_extension_dir(Globals, ExtObj, ExtStr, SubDirName)
        ;
            Ext = ext_target_init_obj(ExtInitObj),
            ext_init_obj_extension_dir(Globals, ExtInitObj, ExtStr, SubDirName)
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = ext_target_init_c(ExtInitC),
        ext_init_c_extension_dir(ExtInitC, ExtStr, SubDirName),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            (
                UseGradeSubdirs = no,
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ;
                UseGradeSubdirs = yes,
                make_grade_subdir_file_name(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        (
            Ext = ext_exec_gs(ExtExecGs),
            ext_exec_gs_extension_dir(Globals, ExtExecGs, ExtStr, SubDirName)
        ;
            Ext = ext_lib_gs(ExtLibGs),
            ext_lib_gs_extension_dir(Globals, ExtLibGs, ExtStr, SubDirName)
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
            make_grade_subdir_file_name(Globals, [SubDirName],
                BaseNameNoExt, ExtStr, DirComponents, FileName),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = ext_mmake_fragment(ExtMf),
        ext_mmake_fragment_extension_dir(ExtMf, ExtStr, SubDirName),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            DirComponents = ["Mercury", SubDirName],
            FileName =
                glue_dir_names_file_name(DirComponents, BaseNameNoExt, ExtStr),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = ext_analysis(ExtAn),
        ext_analysis_extension_dir(ExtAn, ExtStr, SubDirName),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            globals.lookup_bool_option(Globals, use_grade_subdirs,
                UseGradeSubdirs),
            ( if
                UseGradeSubdirs = yes,
                % XXX The code from which this code is derived had this
                % comment, which I (zs) don't think adequately describes
                % the logic behind this confusing code:
                %
                % If we are searching for (rather than writing) the file,
                % just search in Mercury/<ext>s. This is so that searches
                % for files in installed libraries work.
                % `--intermod-directories' is set so this will work.
                not (
                    Search = do_search,
                    ( ExtAn = ext_an_analysis
                    ; ExtAn = ext_an_imdg
                    ; ExtAn = ext_an_request
                    )
                )
            then
                make_grade_subdir_file_name(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            else
                DirComponents = ["Mercury", SubDirName],
                FileName = glue_dir_names_file_name(DirComponents,
                    BaseNameNoExt, ExtStr)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ),
    trace [compile_time(flag("file_name_translations")),
        runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
    (
        record_translation(From, Search, MkDir, Ext, ModuleName, FileName,
            !TIO)
    ).

%---------------------------------------------------------------------------%

:- pred make_grade_subdir_file_name(globals::in, list(dir_name)::in,
    file_name::in, string::in, list(string)::out, file_name::out) is det.

make_grade_subdir_file_name(Globals, SubDirNames, BaseNameNoExt, ExtStr,
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

%---------------------------------------------------------------------------%
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

:- pred record_translation(string::in, maybe_search::in, maybe_create_dirs::in,
    ext::in, module_name::in, string::in, io::di, io::uo) is det.

record_translation(_From, Search, MkDir, Ext, ModuleName, FileName, !IO) :-
    % XXX We could record _From in the table. It could show us where
    % most translation requests come from.
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

write_translations_record_if_any(Globals, !IO) :-
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
        map.foldl4(gather_translation_stats(Globals), Translations,
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

:- pred gather_translation_stats(globals::in, record_key::in, record_value::in,
    int::in, int::out, int::in, int::out,
    map(string, count_sum)::in, map(string, count_sum)::out,
    map(string, count_sum)::in, map(string, count_sum)::out) is det.

gather_translation_stats(Globals, Key, Value, !NumKeys, !NumLookups,
        !ExtMap, !ExtSchDirMap) :-
    !:NumKeys = !.NumKeys + 1,
    Value = record_value(_FileName, Count),
    !:NumLookups = !.NumLookups + Count,
    Key = record_key(_ModuleName, Ext, Search, MkDir),
    ExtStr0 = extension_to_string(Globals, Ext),
    ( if ExtStr0 = "" then
        ExtStr = "no_suffix"
    else
        ExtStr = ExtStr0
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
