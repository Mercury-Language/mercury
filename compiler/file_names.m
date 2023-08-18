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
:- import_module list.

%---------------------------------------------------------------------------%
%
% XXX This interface could be improved.
%
% Some parts of the compiler, such as choose_analysis_cache_dir_name,
% want to know which directory should hold files with a given extension,
% *without* constructing a filename for a module with that extension.
% It should be possible for them to get that information without
% duplicating the logic we have here (which violates Don't Repeat Yourself).
%
% Separating the process of constructing the directory name from the
% the process of creating the filename would also allow us to optimize
% code (such as the code that makes mmakefile fragments) that want
% to convert many module names to filenames using the same extension,
% by doing computing the directoy name parts of those filenames just once.
%

    % For some kinds of files, we know exactly in which directory
    % they should be; for other kinds, we may have to search several
    % directories. For the latter, our clients will need to call the
    % module_name_to_search_file_name predicate, which internally sets
    % Search to for_search.
    %
    % Note that do_create_dirs is not compatible with for_search; if you
    % know what one of several directories should contain a file, but don't
    % know which one, you have by definition no basis you can use to choose
    % which one to create. This invariant is enforced by the fact that
    % module_name_to_search_file_name always passes do_not_create_dirs
    % alongside for_search.
    %
    % This type is not used in the interface of this module, but it is used
    % by the *clients* of this module, usually to decide whether to call
    % module_name_to_file_name or module_name_to_search_file_name.
    %
    % Note that module_name_to_search_file_name constructs just the
    % *filename to search for*, and leaves the actual searching to be done
    % by some other system component. Usually, that component is
    % either module_name_to_search_file_name's caller, or *its* caller, etc.
    % However, in some cases (such as .mh/.mih files), we just output
    % the file name to search for, and leave it to the target language compiler
    % to do the searching.
    %
    % XXX This setup makes it hard to build a database of
    %
    % - which extensions are ever subject to search, and
    % - what the search path of each such extension is.
    %
:- type maybe_for_search
    --->    not_for_search
            % We want a "file name where it is known exactly what directory
            % it is in". The returned filename may contain one or more
            % directory name components.
    ;       for_search.
            % We want a "file name to search for in a list of dirs".
            % The returned filename will never contain any directory name
            % components.

    % This type specifies whether you want to search for a file or not.
    %
    % XXX This type is not used in this module at all.
    % It is used by the *clients* of this module,
    % but it could be moved elsewhere.
    %
:- type maybe_search
    --->    do_not_search
    ;       do_search.

    % If you want to search for a filename, you first need to work out
    % what name you need to search for.
    %
    % If you don't want to search for a filename, you want to construct
    % the final filename directly.
    %
    % This function does the appropriate conversion in each case.
    %
:- func maybe_search_to_maybe_for_search(maybe_search) = maybe_for_search.

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
%   (See make_grade_subdir_name) for the rationale for this scheme.)
%
% Some Java extensions are an exception; they include an extra "jmercury"
% component in the path.
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
%   if --use-grade-subdirs is specified, in a non-grade-specified subdirectory
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
% or whether what we do with for_search is immaterial because we *always*
% translate those extensions with not_for_search.
%
% In the function symbols below,
% - the "gs" suffix stands for the use of a grade-specific directory, while
% - the "ngs" suffix stands for the use of a non-grade-specific directory.
%
% XXX We should probably invert the classification scheme.
% Instead of making the role (interface file, target file, object file, etc)
% be the primary division point, and the directory treatment the second
% division point, we should make the directory treatment the first one.
% However, there is no point in doing that until we have firmly, and
% *explicitly*, decided the directory treatment we want for each and every
% extension. (The current system was arrived at by piling patch upon patch
% on a large piece of over-complex code, and cannot be considered to
% represent the result of explicit deliberation.)

:- type ext
%   --->    ext_src
            % Mercury source files.
            % The extension string is ".m".
            %
            % This extension is not part of the ext type, because
            %
            % - converting it to a filename requires an I/O state pair,
            %   because it may need to read Mercury.modules; while
            % - none of the *other* extensions need an I/O state pair
            %   for *their* conversions to file names.
            %
            % Deleting the I/O state pair from the predicates that do
            % filename translations only therefore requires removing
            % this extension from the ext type.

    --->    ext_cur(ext_cur)
            % All extensions whose files always get put into the current
            % directory.

    ;       ext_cur_ngs(ext_cur_ngs)
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory.

    ;       ext_cur_gs(ext_cur_gs)
            % All extensions whose files can get put either into the current
            % directory, or into a grade-specific subdirectory.

    ;       ext_cur_ngs_gs(ext_cur_ngs_gs)
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory, or into
            % a grade-specific subdirectory, with search being irrelevant.

    ;       ext_cur_ngs_gs_java(ext_cur_ngs_gs_java)
            % All extensions using a java-specific set of rules.
            % With respect to directory structure, they use the same sort
            % of rules as the ext_cur_ngs_gs extensions, but the specifics
            % differ, and they also use a different algorithm for converting
            % module names to file names.

    ;       ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur)
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory, or into
            % a grade-specific subdirectory, with search being specified
            % restricting the options to just the first alternative.

    ;       ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs).
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory, or into
            % a grade-specific subdirectory, with search being specified
            % restricting the options to just the first two alternatives.

%---------------------%

% Each extension specification is followed below
%
% - either by a string giving the extension (the usual case),
% - the name of an option giving the extension, or
% - a prefix, and the name of an option giving the rest of the extension.

:- type ext_cur
            % Compiler-generated C header file for a module that is intended
            % for inclusion by user-written C source files.
    --->    ext_cur_mh                      % ".mh"

            % These extensions are used not to create filenames, but to
            % create mmake target names. Some do refer to real files,
            % but they can (and some do) refer to these using extension
            % strings that can contain references to make variables.
            % Some of the other generated make targets are phony targets,
            % meaning that they never correspond to real files at all.
    ;       ext_cur_pmt_all_int3s           % ".all_int3s"
    ;       ext_cur_pmt_all_ints            % ".all_int3"
    ;       ext_cur_pmt_all_opts            % ".all_opts"
    ;       ext_cur_pmt_all_trans_opts      % ".all_trans_opts"
    ;       ext_cur_pmt_check               % ".check"
    ;       ext_cur_pmt_classes             % ".classes"
    ;       ext_cur_pmt_clean               % ".clean"
    ;       ext_cur_pmt_depend              % ".depend"
    ;       ext_cur_pmt_install_grade_hdrs  % ".install_grade_hdrs"
    ;       ext_cur_pmt_install_hdrs        % ".install_hdrs"
    ;       ext_cur_pmt_install_ints        % ".install_ints"
    ;       ext_cur_pmt_install_opts        % ".install_opts"
    ;       ext_cur_pmt_int3s               % ".int3s"
    ;       ext_cur_pmt_ints                % ".ints"
    ;       ext_cur_pmt_javas               % ".javas"
    ;       ext_cur_pmt_opts                % ".opts"
    ;       ext_cur_pmt_realclean           % ".realclean"
    ;       ext_cur_pmt_trans_opts          % ".trans_opts"

            % Compiler-generated files that are intended to be read
            % by the programmer.
    ;       ext_cur_user_defn_ext           % ".defn_extents"
    ;       ext_cur_user_defn_lc            % ".defn_line_counts"
    ;       ext_cur_user_defns              % ".defns"
    ;       ext_cur_user_depgraph           % ".dependency_graph"
    ;       ext_cur_user_err                % ".err"
    ;       ext_cur_user_hlds_dump          % ".hlds_dump"
    ;       ext_cur_user_imports_graph      % ".imports_graph"
    ;       ext_cur_user_lct                % ".local_call_tree"
    ;       ext_cur_user_lct_order          % ".local_call_tree_order"
    ;       ext_cur_user_mlds_dump          % ".mlds_dump"
    ;       ext_cur_user_mode_constr        % ".mode_constraints"
    ;       ext_cur_user_order              % ".order"
    ;       ext_cur_user_order_to           % ".order_trans_opt"
    ;       ext_cur_user_type_repns         % ".type_repns"
    ;       ext_cur_user_ugly               % ".ugly"
    ;       ext_cur_user_xml.               % ".xml"

:- type ext_cur_ngs
            % Compiler-generated interface files, and the timestamp files
            % showing when they were last checked.
    --->    ext_cur_ngs_int_int0            % ".int0"
    ;       ext_cur_ngs_int_int1            % ".int"
    ;       ext_cur_ngs_int_int2            % ".int2"
    ;       ext_cur_ngs_int_int3            % ".int3"
    ;       ext_cur_ngs_int_date_int0       % ".date0"
    ;       ext_cur_ngs_int_date_int12      % ".date"
    ;       ext_cur_ngs_int_date_int3       % ".date3"

            % Compiler-generated files that are designed to be bodily included
            % in Mmakefiles.
    ;       ext_cur_ngs_mf_d                % ".d"
    ;       ext_cur_ngs_mf_dv               % ".dv"
    ;       ext_cur_ngs_mf_dep              % ".dep"

            % Compiler-generated files that represent bytecode
            % for a long-ago attempt at a bytecode based Mercury debugger.
    ;       ext_cur_ngs_bc_mbc              % ".bc"
    ;       ext_cur_ngs_bc_bytedebug        % ".bytedebug"

            % Misc extensions.
    ;       ext_cur_ngs_misc_module_dep     % ".module_dep"
            % XXX DODGY What is the correctness argument for making this
            % a NON-grade-specific extension? If *anything* in a .module_dep
            % file can *ever* be grade dependent, this should be a
            % grade-specific extension.
    ;       ext_cur_ngs_misc_err_date       % ".err_date"
            % XXX DODGY If you recompile a module in a different grade,
            % the contents of the .err file may change, for example
            % because one grade satisfies the requirements of a
            % require_feature_set declaration and the other does not.
            % To me (zs), this argues in favor of .err_date files
            % belonging in a grade-specific directory. The fact that
            % it would obviously be harder to people to find them there
            % is no relevant, since people shouldn't *have* to find
            % .err_date files.
            % XXX zs and juliensf agree on this.
    ;       ext_cur_ngs_misc_prof.          % ".prof"
            % XXX DODGY Given that different profiling grades generate
            % different profiles (specifically, they produce different subsets
            % of the whole set of kinds of info that the non-deep profiler can
            % generate), shouldn't this be a grade-specific extension?
            % XXX zs and juliensf agree on this.

:- type ext_cur_gs
            % Executables generated for a whole program.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.
            %
            % XXX According to the documentation of the --use-grade-subdirs
            % option, *all* executables and libraries *should* be put
            % into a grade subdir if that option is specified, not just some.
            % They should then be copied or linked to the current directory.
    --->    ext_cur_gs_exec_noext           % ""
            % XXX While an empty extension *usually means we are building
            % the name of an executable, it can also mean we are building
            % the name of a phony Mmakefile target for a library, such as
            % libmer_std in the library directory.

    ;       ext_cur_gs_exec_exe             % ".exe"
    ;       ext_cur_gs_exec_bat             % ".bat"
    ;       ext_cur_gs_exec_exec_opt        % executable_file_extension

            % Libraries, which may be statically or dynamically linked,
            % generated for a set of modules.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.
    ;       ext_cur_gs_lib_dollar_efsl      % ".(EXT_FOR_SHARED_LIB)"
%   ;       ext_cur_gs_lib_lib              % ".lib"
%   ;       ext_cur_gs_lib_so               % ".so"
            % NOTE Neither ext_cur_gs_lib_lib nor ext_cur_gs_lib_so are
            % ever referred to by that name. All references to files with
            % those extensions use ext_cur_gs_lib_lib_opt and
            % ext_cur_gs_lib_sh_lib_opt.
    ;       ext_cur_gs_lib_dollar_a         % ".$A"
    ;       ext_cur_gs_lib_archive          % ".a"
    ;       ext_cur_gs_lib_dll              % ".dll"
    ;       ext_cur_gs_lib_init             % ".init"
    ;       ext_cur_gs_lib_jar              % ".jar"
    ;       ext_cur_gs_lib_lib_opt          % library_extension
    ;       ext_cur_gs_lib_sh_lib_opt.      % shared_library_extension

:- type ext_cur_ngs_gs
            % Timestamp files showing when their corresponding .*opt files
            % were last checked.
    --->    ext_cur_ngs_gs_opt_date_plain   % ".optdate"
    ;       ext_cur_ngs_gs_opt_date_trans   % ".trace_opt_date"

            % C and C# source files generated by the Mercury compiler.
    ;       ext_cur_ngs_gs_target_c         % ".c"
    ;       ext_cur_ngs_gs_target_cs        % ".cs"

            % Timestamp files that record the date and time when a target
            % language (C, C# or Java) source files was last logically remade.
            % (The "logically" parts means that if the new, up-to-date version
            % is bit-for-bit identical to the old version, then we update
            % the timestamp file, but not the file it refers to.)
    ;       ext_cur_ngs_gs_target_date_c    % ".c_date"
    ;       ext_cur_ngs_gs_target_date_cs   % ".cs_date"
    ;       ext_cur_ngs_gs_target_date_java % ".java_date"

            % C files associated not with a module but with a whole program,
            % containing the code needed to initialize various tables for
            % the runtime system.
    ;       ext_cur_ngs_gs_init_c           % ".init_c"

            % Object files generated for C source files generated for a module
            % by the Mercury compiler.
    ;       ext_cur_ngs_gs_obj_dollar_o     % ".$O"
    ;       ext_cur_ngs_gs_obj_dollar_efpo  % ".$(EXT_FOR_PIC_OBJECTS)"
    ;       ext_cur_ngs_gs_obj_o            % ".o"
    ;       ext_cur_ngs_gs_obj_pic_o        % ".pic_o"
    ;       ext_cur_ngs_gs_obj_obj_opt      % object_file_extension option
    ;       ext_cur_ngs_gs_obj_pic_obj_opt  % pic_object_file_extension option

            % Object files associated not with a module but with
            % a whole program, containing the code needed to initialize
            % various tables for the runtime system.
    ;       ext_cur_ngs_gs_init_obj_dollar_o    % ".init.$O"
    ;       ext_cur_ngs_gs_init_obj_o           % ".init.c"
    ;       ext_cur_ngs_gs_init_obj_pic_o       % ".init.pic_o"
    ;       ext_cur_ngs_gs_init_obj_obj_opt
                                        % "_init" ++ object_file_extension
    ;       ext_cur_ngs_gs_init_obj_pic_obj_opt
                                        % "_init" ++ pic_object_file_extension

            % Compiler-generated files that are part of the incomplete
            % attempt at an intermodule analysis and optimization framework
            % in analysis.m and its clients.
    ;       ext_cur_ngs_gs_an_ds_date           % ".analysis_date"
    ;       ext_cur_ngs_gs_an_ds_status         % ".analysis_status"

            % Misc extensions.
    ;       ext_cur_ngs_gs_misc_used            % ".used"
    ;       ext_cur_ngs_gs_misc_track_flags.    % ".track_flags"

:- type ext_cur_ngs_gs_java
            % Java source files generated by the Mercury compiler.
            % The names of these files, and of the directories
            % that store them, are computed by a different algorithm
            % from the ones applicable to C and C# source files.
    --->    ext_cur_ngs_gs_java_java            % ".java"
    ;       ext_cur_ngs_gs_java_class.          % ".class"

:- type ext_cur_ngs_gs_max_cur
            % Compiler-generated header file for a module that is intended
            % for inclusion by Mercury-generated C source files.
    --->    ext_cur_ngs_gs_max_cur_mih.         % ".mih"

:- type ext_cur_ngs_gs_max_ngs
            % Compiler-generated optimization files.
    --->    ext_cur_ngs_gs_max_ngs_opt_plain    % ".opt"
    ;       ext_cur_ngs_gs_max_ngs_opt_trans    % ".trans_opt"

            % Compiler-generated files that are part of the incomplete
            % attempt at an intermodule analysis and optimization framework
            % in analysis.m and its clients.
    ;       ext_cur_ngs_gs_max_ngs_an_analysis  % ".analysis"
    ;       ext_cur_ngs_gs_max_ngs_an_imdg      % ".imdg"
    ;       ext_cur_ngs_gs_max_ngs_an_request.  % ".request"

:- func extension_to_string(globals, ext) = string.

%---------------------%

:- func module_name_to_base_file_name_no_ext(ext, module_name) = file_name.
:- func module_name_to_base_file_name_no_ext_non_java(module_name) = file_name.
:- func module_name_to_base_file_name_no_ext_java(module_name) = file_name.

%---------------------%

% XXX Most of the predicates below take a "from" string argument,
% for which the caller is expected pass $pred or some other identification
% of the call site. This is a temporary measure, intended to help debug
% any problems that may arise during the process of slicing the space
% of extensions into smaller and smaller pieces.

    % Return the file name of the Mercury source for the given module.
    %
    % Currently we use the convention that the module `foo.bar.baz' should be
    % named `foo.bar.baz.m', and allow other naming conventions with the
    % `-f' option.
    %
:- pred module_name_to_source_file_name(module_name::in, file_name::out,
    io::di, io::uo) is det.

    % module_name_to_file_name_return_dirs(Globals, From, Ext, Module,
    %   DirNames, FileName):
    %
    % Convert a module name and file extension to the corresponding file name.
    % Return the directory pathname in FileName as DirNames. The caller
    % can then create those directories (with create_any_dirs_on_path below),
    % or not, as they wish.
    %
    % The version without the _return_dirs suffix does not return the
    % DirNames argument, which is what callers that don't want to create
    % those directories usually want.
    %
    % The version with the _create_dirs suffix instead calls
    % create_any_dirs_on_path itself.
    %
    % This arrangement allows the first two versions to work *without*
    % being passed an I/O state pair.
    %
    % The versions whose names include "full_curdir" return two filenames.
    % The first, the "full" filename may be in a non-grade-specific
    % or in a grade-specific directory, or it can be in the current directory.
    % The second, the "curdir" filename will always be in the current
    % directory.
    %
    % Note that these predicates are also used to create some "phony" Makefile
    % targets that do not have corresponding files, e.g. `<foo>.clean'.
    %
:- pred module_name_to_file_name_return_dirs(globals::in,
    string::in, ext::in, module_name::in, list(dir_name)::out,
    file_name::out) is det.
:- pred module_name_to_file_name(globals::in,
    string::in, ext::in, module_name::in, file_name::out) is det.
:- pred module_name_to_file_name_full_curdir(globals::in,
    string::in, ext::in, module_name::in,
    file_name::out, file_name::out) is det.
:- pred module_name_to_file_name_create_dirs(globals::in,
    string::in, ext::in, module_name::in, file_name::out,
    io::di, io::uo) is det.
:- pred module_name_to_file_name_full_curdir_create_dirs(globals::in,
    string::in, ext::in, module_name::in, file_name::out, file_name::out,
    io::di, io::uo) is det.

    % module_name_to_search_file_name(Globals, From, Ext, Module, FileName):
    %
    % As module_name_to_file_name, but for a file which might be
    % in an installed library, not the current directory. There are no
    % variants with _return_dirs or _create_dirs suffixes, because
    % there is no point in creating the directories you are trying to search;
    % if you have to create a directory, it won't contain the file
    % you are looking for.
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
    ext::in, module_name::in, file_name::out) is det.

    % module_name_to_lib_file_name_return_dirs(Globals, From, Prefix, Ext,
    %   Module, DirNames, FileName):
    %
    % Like module_name_to_file_name_return_dirs, but also allows a prefix.
    % The variants without the _return_dirs suffix and with the _create_dirs
    % suffix, and with full_curdir,  mean the same thing as with
    % module_name_to_file_name_return_dirs.
    %
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name_return_dirs(globals::in, string::in,
    string::in, ext::in, module_name::in, list(dir_name)::out, file_name::out)
    is det.
:- pred module_name_to_lib_file_name(globals::in, string::in,
    string::in, ext::in, module_name::in, file_name::out) is det.
:- pred module_name_to_lib_file_name_full_curdir(globals::in, string::in,
    string::in, ext::in, module_name::in, file_name::out, file_name::out)
    is det.
:- pred module_name_to_lib_file_name_create_dirs(globals::in, string::in,
    string::in, ext::in, module_name::in, file_name::out,
    io::di, io::uo) is det.
:- pred module_name_to_lib_file_name_full_curdir_create_dirs(globals::in,
    string::in, string::in, ext::in, module_name::in,
    file_name::out, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name_return_dirs(Globals, Ext, FactTableFileName,
    %   DirNames, FileName):
    %
    % Returns the filename to use when compiling fact table files.
    % Return the directory pathname in FileName as DirNames. The caller
    % can then create those directories (with create_any_dirs_on_path below),
    % or not, as they wish.
    %
:- pred fact_table_file_name_return_dirs(globals::in, string::in,
    ext::in, file_name::in, list(dir_name)::out, file_name::out) is det.

%---------------------------------------------------------------------------%

    % Return the directory path of the directory into which files
    % with the given extension should be put.
    %
:- pred ext_to_dir_path(globals::in, maybe_for_search::in, ext::in,
    list(dir_name)::out) is det.

%---------------------------------------------------------------------------%

    % If the proper place for a file is in a subdirectory (e.g. Mercury/css),
    % but the subdirectory does not exist, which in this case may mean either
    %
    % - that Mercury exists but Mercury/css does not, or
    % - that Mercury does not exist, which of course means that Mercury/css
    %   does not exist either,
    %
    % values of this type tell maybe_create_dirs_on_path whether
    % it should create any such missing directories.
    %
:- type maybe_create_dirs
    --->    do_create_dirs
    ;       do_not_create_dirs.

    % maybe_create_any_dirs_on_path(Mkdir, DirNames, !IO):
    %
    % If Mkdir = do_create_dirs, then create the directory whose name
    % given by the given directory component names.
    %
:- pred maybe_create_any_dirs_on_path(maybe_create_dirs::in,
    list(string)::in, io::di, io::uo) is det.

    % create_any_dirs_on_path(DirNames, !IO):
    %
    % Create the directory whose name given by the given directory
    % component names.
    %
:- pred create_any_dirs_on_path(list(string)::in, io::di, io::uo) is det.

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
:- pred get_java_dir_path(globals::in, ext_cur_ngs_gs_java::in,
    list(dir_name)::out) is det.

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
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

maybe_search_to_maybe_for_search(do_not_search) = not_for_search.
maybe_search_to_maybe_for_search(do_search) = for_search.

%---------------------------------------------------------------------------%

:- pragma inline(func(extension_to_string/2)).

extension_to_string(Globals, Ext) = ExtStr :-
    (
        Ext = ext_cur(ExtCur),
        ext_cur_extension(ExtCur, ExtStr)
    ;
        Ext = ext_cur_ngs(ExtCurNgs),
        ext_cur_ngs_extension_dir(ExtCurNgs, ExtStr, _SubDirName)
    ;
        Ext = ext_cur_gs(ExtCurGs),
        ext_cur_gs_extension_dir(Globals, ExtCurGs, ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gs(ExtCurNgsGs),
        ext_cur_ngs_gs_extension_dir(Globals, ExtCurNgsGs,
            ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gs_java(ExtCurNgsGsJava),
        ext_cur_ngs_gs_java_extension_dir(ExtCurNgsGsJava,
            ExtStr, _SubDirNames)
    ;
        Ext = ext_cur_ngs_gs_max_cur(ExtCurNgsGsMaxCur),
        ext_cur_ngs_gs_max_cur_extension_dir(ExtCurNgsGsMaxCur,
            ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gs_max_ngs(ExtCurNgsGsMaxNgs),
        ext_cur_ngs_gs_max_ngs_extension_dir(ExtCurNgsGsMaxNgs,
            ExtStr, _SubDirName)
    ).

%---------------------------------------------------------------------------%

:- pred ext_cur_extension(ext_cur::in, string::out) is det.

ext_cur_extension(ext_cur_mh,                       ".mh").
ext_cur_extension(ext_cur_pmt_all_int3s,            ".all_int3s").
ext_cur_extension(ext_cur_pmt_all_ints,             ".all_ints").
ext_cur_extension(ext_cur_pmt_all_opts,             ".all_opts").
ext_cur_extension(ext_cur_pmt_all_trans_opts,       ".all_trans_opts").
ext_cur_extension(ext_cur_pmt_check,                ".check").
ext_cur_extension(ext_cur_pmt_classes,              ".classes").
ext_cur_extension(ext_cur_pmt_clean,                ".clean").
ext_cur_extension(ext_cur_pmt_depend,               ".depend").
ext_cur_extension(ext_cur_pmt_install_grade_hdrs,   ".install_grade_hdrs").
ext_cur_extension(ext_cur_pmt_install_hdrs,         ".install_hdrs").
ext_cur_extension(ext_cur_pmt_install_ints,         ".install_ints").
ext_cur_extension(ext_cur_pmt_install_opts,         ".install_opts").
ext_cur_extension(ext_cur_pmt_int3s,                ".int3s").
ext_cur_extension(ext_cur_pmt_ints,                 ".ints").
ext_cur_extension(ext_cur_pmt_javas,                ".javas").
ext_cur_extension(ext_cur_pmt_opts,                 ".opts").
ext_cur_extension(ext_cur_pmt_realclean,            ".realclean").
ext_cur_extension(ext_cur_pmt_trans_opts,           ".trans_opts").
ext_cur_extension(ext_cur_user_defn_ext,            ".defn_extents").
ext_cur_extension(ext_cur_user_defn_lc,             ".defn_line_counts").
ext_cur_extension(ext_cur_user_defns,               ".defns").
ext_cur_extension(ext_cur_user_depgraph,            ".dependency_graph").
ext_cur_extension(ext_cur_user_err,                 ".err").
ext_cur_extension(ext_cur_user_hlds_dump,           ".hlds_dump").
ext_cur_extension(ext_cur_user_imports_graph,       ".imports_graph").
ext_cur_extension(ext_cur_user_lct,                 ".local_call_tree").
ext_cur_extension(ext_cur_user_lct_order,           ".local_call_tree_order").
ext_cur_extension(ext_cur_user_mlds_dump,           ".mlds_dump").
ext_cur_extension(ext_cur_user_mode_constr,         ".mode_constraints").
ext_cur_extension(ext_cur_user_order,               ".order").
ext_cur_extension(ext_cur_user_order_to,            ".order_trans_opt").
ext_cur_extension(ext_cur_user_type_repns,          ".type_repns").
ext_cur_extension(ext_cur_user_ugly,                ".ugly").
ext_cur_extension(ext_cur_user_xml,                 ".xml").

:- pred ext_cur_ngs_extension_dir(ext_cur_ngs::in,
    string::out, string::out) is det.

ext_cur_ngs_extension_dir(ext_cur_ngs_int_int0,         ".int0",    "int0s").
ext_cur_ngs_extension_dir(ext_cur_ngs_int_int1,         ".int",     "ints").
ext_cur_ngs_extension_dir(ext_cur_ngs_int_int2,         ".int2",    "int2s").
ext_cur_ngs_extension_dir(ext_cur_ngs_int_int3,         ".int3",    "int3s").
ext_cur_ngs_extension_dir(ext_cur_ngs_int_date_int0,    ".date0",   "date0s").
ext_cur_ngs_extension_dir(ext_cur_ngs_int_date_int12,   ".date",    "dates").
ext_cur_ngs_extension_dir(ext_cur_ngs_int_date_int3,    ".date3",   "date3s").

ext_cur_ngs_extension_dir(ext_cur_ngs_mf_d,      ".d",   "ds").
% The next two deviations below from the "delete initial dot, add final 's'"
% rule are intentional, though I (zs) don't know the reason for the second.
ext_cur_ngs_extension_dir(ext_cur_ngs_mf_dv,     ".dv",  "deps").
ext_cur_ngs_extension_dir(ext_cur_ngs_mf_dep,    ".dep", "deps").
ext_cur_ngs_extension_dir(ext_cur_ngs_bc_mbc,    ".mbc", "mbcs").
ext_cur_ngs_extension_dir(ext_cur_ngs_bc_bytedebug,
    ".bytedebug", "bytedebugs").
ext_cur_ngs_extension_dir(ext_cur_ngs_misc_module_dep,
    ".module_dep", "module_deps").
ext_cur_ngs_extension_dir(ext_cur_ngs_misc_err_date,
    ".err_date", "err_dates").
ext_cur_ngs_extension_dir(ext_cur_ngs_misc_prof, ".prof", "profs").

:- pred ext_cur_gs_extension_dir(globals::in, ext_cur_gs::in,
    string::out, string::out) is det.

% Launcher scripts go in the `bin' subdirectory.
ext_cur_gs_extension_dir(_, ext_cur_gs_exec_noext,    "",     "bin").
ext_cur_gs_extension_dir(_, ext_cur_gs_exec_exe,      ".exe", "bin").
ext_cur_gs_extension_dir(_, ext_cur_gs_exec_bat,      ".bat", "bin").
ext_cur_gs_extension_dir(Globals, ext_cur_gs_exec_exec_opt, ExtStr, "bin") :-
    globals.lookup_string_option(Globals, executable_file_extension, ExtStr).
ext_cur_gs_extension_dir(_, ext_cur_gs_lib_dollar_efsl,
    ".$(EXT_FOR_SHARED_LIB)", "lib").
% ext_cur_gs_extension_dir(_, ext_cur_gs_lib_lib,        ".lib",  "lib").
% ext_cur_gs_extension_dir(_, ext_cur_gs_lib_so,         ".so",   "lib").
ext_cur_gs_extension_dir(_, ext_cur_gs_lib_dollar_a,   ".$A",   "lib").
ext_cur_gs_extension_dir(_, ext_cur_gs_lib_archive,    ".a",    "lib").
ext_cur_gs_extension_dir(_, ext_cur_gs_lib_dll,        ".dll",  "lib").
ext_cur_gs_extension_dir(_, ext_cur_gs_lib_init,       ".init", "inits").
ext_cur_gs_extension_dir(_, ext_cur_gs_lib_jar,        ".jar",  "lib").
ext_cur_gs_extension_dir(Globals, ext_cur_gs_lib_lib_opt, ExtStr, "lib") :-
    globals.lookup_string_option(Globals, library_extension, ExtStr).
ext_cur_gs_extension_dir(Globals, ext_cur_gs_lib_sh_lib_opt, ExtStr, "lib") :-
    globals.lookup_string_option(Globals, shared_library_extension, ExtStr).

:- pred ext_cur_ngs_gs_extension_dir(globals::in, ext_cur_ngs_gs::in,
    string::out, string::out) is det.

ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_opt_date_plain,
        ".optdate", "optdates").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_opt_date_trans,
        ".trans_opt_date", "trans_opt_dates").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_target_c,     ".c",    "cs").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_target_cs,    ".cs",   "css").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_target_date_c,
        ".c_date",    "c_dates").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_target_date_cs,
        ".cs_date",   "cs_dates").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_target_date_java,
        ".java_date", "java_dates").
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_obj_dollar_o,
        ".$O", "os").
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_obj_dollar_efpo,
        ".$(EXT_FOR_PIC_OBJECTS)", "os").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_obj_o,
        ".o", "os").
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_obj_pic_o,
        ".pic_o", "os").
ext_cur_ngs_gs_extension_dir(Globals, ext_cur_ngs_gs_obj_obj_opt,
        ExtStr, "os") :-
    globals.lookup_string_option(Globals, object_file_extension, ExtStr).
ext_cur_ngs_gs_extension_dir(Globals, ext_cur_ngs_gs_obj_pic_obj_opt,
        ExtStr, "os") :-
    globals.lookup_string_option(Globals, pic_object_file_extension, ExtStr).
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_init_c, "_init.c", "cs").
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_init_obj_dollar_o,
        "_init.$O",     "os").
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_init_obj_o,
        "_init.o",      "os").
% The deviation from the "delete initial dot, add final 's'" rule
% is intentional.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_init_obj_pic_o,
        "_init.pic_o",  "os").
ext_cur_ngs_gs_extension_dir(Globals, ext_cur_ngs_gs_init_obj_obj_opt,
        ExtStr, "os") :-
    globals.lookup_string_option(Globals, object_file_extension, ExtStr0),
    ExtStr = "_init" ++ ExtStr0.
ext_cur_ngs_gs_extension_dir(Globals, ext_cur_ngs_gs_init_obj_pic_obj_opt,
        ExtStr, "os") :-
    globals.lookup_string_option(Globals, pic_object_file_extension, ExtStr0),
    ExtStr = "_init" ++ ExtStr0.
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_an_ds_date,
        ".analysis_date",   "analysis_dates").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_an_ds_status,
        ".analysis_status", "analysis_statuss").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_misc_used,
        ".used",        "useds").
ext_cur_ngs_gs_extension_dir(_, ext_cur_ngs_gs_misc_track_flags,
        ".track_flags", "track_flags").

:- pred ext_cur_ngs_gs_java_extension_dir(ext_cur_ngs_gs_java::in,
    string::out, string::out) is det.

ext_cur_ngs_gs_java_extension_dir(ext_cur_ngs_gs_java_java,
        ".java",    "javas").
ext_cur_ngs_gs_java_extension_dir(ext_cur_ngs_gs_java_class,
        ".class",   "classes").

:- pred ext_cur_ngs_gs_max_cur_extension_dir(ext_cur_ngs_gs_max_cur::in,
    string::out, string::out) is det.

ext_cur_ngs_gs_max_cur_extension_dir(ext_cur_ngs_gs_max_cur_mih,
        ".mih", "mihs").

:- pred ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs::in,
    string::out, string::out) is det.

ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs_opt_plain,
        ".opt",        "opts").
ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs_opt_trans,
        ".trans_opt",  "trans_opts").
ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs_an_analysis,
        ".analysis",    "analyses").
ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs_an_imdg,
        ".imdg",        "imdgs").
ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs_an_request,
        ".request",     "requests").

%---------------------------------------------------------------------------%

:- pragma inline(func(module_name_to_base_file_name_no_ext/2)).

module_name_to_base_file_name_no_ext(Ext, ModuleName) = BaseNameNoExt :-
    (
        ( Ext = ext_cur(_)
        ; Ext = ext_cur_ngs(_)
        ; Ext = ext_cur_gs(_)
        ; Ext = ext_cur_ngs_gs(_)
        ; Ext = ext_cur_ngs_gs_max_cur(_)
        ; Ext = ext_cur_ngs_gs_max_ngs(_)
        ),
        BaseNameNoExt =
            module_name_to_base_file_name_no_ext_non_java(ModuleName)
    ;
        Ext = ext_cur_ngs_gs_java(_),
        BaseNameNoExt = module_name_to_base_file_name_no_ext_java(ModuleName)
    ).

:- pragma inline(func(module_name_to_base_file_name_no_ext_non_java/1)).

module_name_to_base_file_name_no_ext_non_java(ModuleName) =
    sym_name_to_string_sep(ModuleName, ".").

:- pragma inline(func(module_name_to_base_file_name_no_ext_java/1)).

module_name_to_base_file_name_no_ext_java(ModuleName) = BaseNameNoExt :-
    mangle_sym_name_for_java(ModuleName, module_qual, "__", BaseNameNoExt).

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

module_name_to_file_name_return_dirs(Globals, From, Ext,
        ModuleName, DirNames, FullFileName) :-
    module_name_to_file_name_ext(Globals, From, not_for_search, no,
        Ext, ModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

module_name_to_file_name(Globals, From, Ext,
        ModuleName, FullFileName) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, ModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

module_name_to_file_name_full_curdir(Globals, From, Ext,
        ModuleName, FullFileName, CurDirFileName) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, ModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

module_name_to_file_name_create_dirs(Globals, From, Ext,
        ModuleName, FullFileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, ModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName),
    create_any_dirs_on_path(DirNames, !IO).

module_name_to_file_name_full_curdir_create_dirs(Globals, From, Ext,
        ModuleName, FullFileName, CurDirFileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, ModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName),
    create_any_dirs_on_path(DirNames, !IO).

%---------------------%

module_name_to_search_file_name(Globals, From, Ext,
        ModuleName, FullFileName) :-
    module_name_to_file_name_ext(Globals, From, for_search,
        yes(do_not_create_dirs), Ext, ModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

%---------------------%

module_name_to_lib_file_name_return_dirs(Globals, From, Prefix, Ext,
        ModuleName, DirNames, FullFileName) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        no, Ext, FakeModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

module_name_to_lib_file_name(Globals, From, Prefix, Ext,
        ModuleName, FullFileName) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, FakeModuleName,
        DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

module_name_to_lib_file_name_full_curdir(Globals, From, Prefix, Ext,
        ModuleName, FullFileName, CurDirFileName) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, FakeModuleName,
        DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

module_name_to_lib_file_name_create_dirs(Globals, From, Prefix, Ext,
        ModuleName, FullFileName, !IO) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, FakeModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName),
    create_any_dirs_on_path(DirNames, !IO).

module_name_to_lib_file_name_full_curdir_create_dirs(Globals, From, Prefix,
        Ext, ModuleName, FullFileName, CurDirFileName, !IO) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, FakeModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName),
    create_any_dirs_on_path(DirNames, !IO).

:- func make_fake_module_name(string, module_name) = module_name.

make_fake_module_name(Prefix, ModuleName) = FakeModuleName :-
    BaseFileName = sym_name_to_string(ModuleName),
    BaseNameNoExt = Prefix ++ BaseFileName,
    FakeModuleName = unqualified(BaseNameNoExt).

%---------------------%

fact_table_file_name_return_dirs(Globals, From, Ext,
        FactTableFileName, DirNames, FullFileName) :-
    FakeModuleName = unqualified(FactTableFileName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        no, Ext, FakeModuleName, DirNames, CurDirFileName),
    FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName).

%---------------------------------------------------------------------------%

:- pred module_name_to_file_name_ext(globals::in, string::in,
    maybe_for_search::in, maybe(maybe_create_dirs)::in, ext::in,
    module_name::in, list(dir_name)::out, file_name::out) is det.

module_name_to_file_name_ext(Globals, From, Search, StatOnlyMkdir, Ext,
        ModuleName, DirNames, CurDirFileName) :-
    ext_to_dir_path(Globals, Search, Ext, DirNames),
    BaseNameNoExt = module_name_to_base_file_name_no_ext(Ext, ModuleName),
    ExtStr = extension_to_string(Globals, Ext),
    CurDirFileName = BaseNameNoExt ++ ExtStr,
    trace [compile_time(flag("file_name_translations")),
        runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
    (
        FullFileName = glue_dir_names_base_name(DirNames, CurDirFileName),
        record_translation(From, Search, StatOnlyMkdir,
            Ext, ModuleName, FullFileName, !TIO)
    ).

:- pragma inline(pred(ext_to_dir_path/4)).

ext_to_dir_path(Globals, Search, Ext, DirNames) :-
    (
        Ext = ext_cur(_ExtCur),
        % Output files intended for use by the user, and phony Mmake target
        % names go in the current directory, and so do .mh files,
        DirNames = []
    ;
        Ext = ext_cur_ngs(ExtCurNgs),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNames = []
        ;
            ( SubdirSetting = use_cur_ngs_subdir
            ; SubdirSetting = use_cur_ngs_gs_subdir
            ),
            ext_cur_ngs_extension_dir(ExtCurNgs, _ExtStr, SubDirName),
            DirNames = ["Mercury", SubDirName]
        )
    ;
        Ext = ext_cur_gs(ExtCurGs),
        % Executables and library files go in the current directory
        % only with --no-use-grade-subdirs; with --use-grade-subdirs,
        % they go in a grade subdir.
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            ( SubdirSetting = use_cur_dir
            ; SubdirSetting = use_cur_ngs_subdir
            ),
            DirNames = []
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_gs_extension_dir(Globals, ExtCurGs, _ExtStr, SubDirName),
            DirNames = make_grade_subdir_name(Globals, SubDirName)
        )
    ;
        Ext = ext_cur_ngs_gs(ExtCurNgsGs),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNames = []
        ;
            SubdirSetting = use_cur_ngs_subdir,
            ext_cur_ngs_gs_extension_dir(Globals, ExtCurNgsGs,
                _ExtStr, SubDirName),
            DirNames = ["Mercury", SubDirName]
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_ngs_gs_extension_dir(Globals, ExtCurNgsGs,
                _ExtStr, SubDirName),
            DirNames = make_grade_subdir_name(Globals, SubDirName)
        )
    ;
        Ext = ext_cur_ngs_gs_java(ExtCurNgsGsJava),
        get_java_dir_path(Globals, ExtCurNgsGsJava, DirNames0),
        DirNames = DirNames0 ++ ["jmercury"]
    ;
        Ext = ext_cur_ngs_gs_max_cur(ExtCurNgsGsMaxCur),
        (
            Search = for_search,
            % If we are searching for (rather than writing) a `.mih' file,
            % use the plain file name. This is so that searches for files
            % in installed libraries will work. `--c-include-directory' is set
            % so that searches for files in the current directory will work.
            DirNames = []
        ;
            Search = not_for_search,
            globals.get_subdir_setting(Globals, SubdirSetting),
            (
                SubdirSetting = use_cur_dir,
                DirNames = []
            ;
                SubdirSetting = use_cur_ngs_subdir,
                ext_cur_ngs_gs_max_cur_extension_dir(ExtCurNgsGsMaxCur,
                    _ExtStr, SubDirName),
                DirNames = ["Mercury", SubDirName]
            ;
                SubdirSetting = use_cur_ngs_gs_subdir,
                ext_cur_ngs_gs_max_cur_extension_dir(ExtCurNgsGsMaxCur,
                    _ExtStr, SubDirName),
                DirNames = make_grade_subdir_name(Globals, SubDirName)
            )
        )
    ;
        Ext = ext_cur_ngs_gs_max_ngs(ExtCurNgsGsMaxNgs),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNames = []
        ;
            SubdirSetting = use_cur_ngs_subdir,
            ext_cur_ngs_gs_max_ngs_extension_dir(ExtCurNgsGsMaxNgs,
                _ExtStr, SubDirName),
            DirNames = ["Mercury", SubDirName]
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_ngs_gs_max_ngs_extension_dir(ExtCurNgsGsMaxNgs,
                _ExtStr, SubDirName),
            (
                Search = for_search,
                DirNames = ["Mercury", SubDirName]
            ;
                Search = not_for_search,
                DirNames = make_grade_subdir_name(Globals, SubDirName)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- func make_grade_subdir_name(globals, dir_name) = list(string).

make_grade_subdir_name(Globals, SubDirName) = GradeSubDirNames :-
    grade_directory_component(Globals, Grade),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    % The extra "Mercury" is needed so we can use `--intermod-directory
    % Mercury/<grade>/<target_arch>' and `--c-include
    % Mercury/<grade>/<target_arch>' to find the local `.opt' and `.mih'
    % files without messing up the search for the files for installed
    % libraries.
    GradeSubDirNames = ["Mercury", Grade, TargetArch, "Mercury", SubDirName].

:- func glue_dir_names_base_name(list(string), string) = string.

glue_dir_names_base_name(DirComponents, CurDirFileName) = FullFileName :-
    (
        DirComponents = [],
        FullFileName = CurDirFileName
    ;
        DirComponents = [_ | _],
        Components = DirComponents ++ [CurDirFileName],
        FullFileName = dir.relative_path_name_from_components(Components)
    ).

%---------------------------------------------------------------------------%

maybe_create_any_dirs_on_path(Mkdir, DirComponents, !IO) :-
    (
        Mkdir = do_not_create_dirs
    ;
        Mkdir = do_create_dirs,
        create_any_dirs_on_path(DirComponents, !IO)
    ).

create_any_dirs_on_path(DirComponents, !IO) :-
    (
        DirComponents = []
    ;
        DirComponents = [_ | _],
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
    ).

:- mutable(made_dirs, set_tree234(string), set_tree234.init, ground,
    [untrailed, attach_to_io_state]).

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

get_java_dir_path(Globals, ExtCurNgsGsJava, DirNames) :-
    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        SubdirSetting = use_cur_dir,
        DirNames = []
    ;
        SubdirSetting = use_cur_ngs_subdir,
        ext_cur_ngs_gs_java_extension_dir(ExtCurNgsGsJava,
            _ExtStr, SubDirName),
        DirNames = ["Mercury", SubDirName]
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        ext_cur_ngs_gs_java_extension_dir(ExtCurNgsGsJava,
            _ExtStr, SubDirName),
        DirNames = make_grade_subdir_name(Globals, SubDirName)
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
%

:- type record_key
    --->    record_key(
                module_name,
                ext,
                maybe_for_search,
                maybe(maybe_create_dirs)
            ).

:- type record_value
    --->    record_value(string, int).

:- mutable(translations, map(record_key, record_value), map.init, ground,
    [untrailed, attach_to_io_state]).

:- pred record_translation(string::in, maybe_for_search::in,
    maybe(maybe_create_dirs)::in, ext::in, module_name::in, string::in,
    io::di, io::uo) is det.

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
    Key = record_key(_ModuleName, Ext, Search, MaybeMkdir),
    ExtStr0 = extension_to_string(Globals, Ext),
    ( if ExtStr0 = "" then
        ExtStr = "no_suffix"
    else
        ExtStr = ExtStr0
    ),
    (
        Search = for_search,
        SearchStr = "_search"
    ;
        Search = not_for_search,
        SearchStr = "_nosearch"
    ),
    (
        MaybeMkdir = no,
        MkDirStr = "_returndir"
    ;
        MaybeMkdir = yes(MkDir),
        (
            MkDir = do_create_dirs,
            MkDirStr = "_mkdir"
        ;
            MkDir = do_not_create_dirs,
            MkDirStr = "_nomkdir"
        )
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
