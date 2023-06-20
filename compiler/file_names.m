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

:- func extension_to_string(ext, newext) = string.

:- func make_module_dep_file_extension = other_ext is det.

%---------------------%

:- type newext
    --->    newext_src
            % Mercury source files.
            % The extension string is ".m".

    ;       newext_int(ext_int)
    ;       newext_opt(ext_opt)
            % Compiler-generated interface files and optimization files,
            % and the timestamp files showing when they were last checked.

    ;       newext_mh(ext_mh)
            % Compiler-generated header file for a module that is intended
            % for inclusion by user-written C source files.
            % The extension string is ".mh".

    ;       newext_mih(ext_mih)
            % Compiler-generated header file for a module that is intended
            % for inclusion by Mercury-generated C source files.
            % The extension string is ".mih".

    ;       newext_target_c_cs(ext_target_c_cs)
            % C and C# source files generated by the Mercury compiler.

    ;       newext_target_java(ext_target_java)
            % Java source files generated by the Mercury compiler.
            % The names of these files are computed by a different algorithm
            % from the names of C and C# source files.

    ;       newext_target_date(ext_target_date)
            % Timestamp files that record the date and time when a target
            % language (C, C# or Java) source files was last logically remade.
            % (The "logically" parts means that if the new, up-to-date version
            % is bit-for-bit identical to the old version, then we update
            % the timestamp file, but not the file it refers to.)

    ;       newext_target_obj(ext_obj)
            % Object files generated for C source files generated for a module
            % by the Mercury compiler.

    ;       newext_target_init_c(ext_init_c)
    ;       newext_target_init_obj(ext_init_obj)
            % C and object files associated not with a module but with
            % a whole program, containing the code needed to initialize
            % various tables for the runtime system.

    ;       newext_exec(ext_exec)
    ;       newext_exec_gs(ext_exec_gs)
            % Executables generated for a whole program.
            %
            % The newext_exec executables always go into the current directory.
            % The newext_exec_gs executables (the "gs" suffix stands for
            % "grade-specific")
            %
            % - have filenames in the current directory
            %   with --no-use-grade-subdirs,
            % - have filenames in the selected grade's subdir
            %   with --use-grade-subdirs.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.

    ;       newext_lib(ext_lib)
    ;       newext_lib_gs(ext_lib_gs)
            % Libraries, which may be statically or dynamically linked,
            % generated for a set of modules.
            %
            % The newext_lib libraries always go into the current directory.
            % The newext_lib_gs libraries (the "gs" suffix stands for
            % "grade-specific")
            %
            % - have filenames in the current directory
            %   with --no-use-grade-subdirs,
            % - have filenames in the selected grade's subdir
            %   with --use-grade-subdirs.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.

    ;       newext_mmake_fragment(ext_mmake_fragment)
            % Compiler-generated files that are designed to be bodily included
            % in Mmakefiles.

    ;       newext_mmake_target(ext_mmake_target)
            % These extensions are used not to create filenames, but to
            % create mmake target names. Some do refer to real files,
            % but they can (and some do) refer to these using extension
            % strings that can contain references to make variables.
            % Some of the other generated make targets are phony targets,
            % meaning that they never correspond to real files at all.

    ;       newext_user(ext_user)
    ;       newext_user_ngs(ext_user_ngs)
            % Compiler-generated files that are intended to be read
            % by the programmer.
            %
            % The newext_user files always go into the current directory.
            % The newext_user_ngs file (the "ngs" suffix stands for
            % "non-grade-specific")
            %
            % - have filenames in the current directory
            %   with --no-use-subdirs,
            % - have filenames in a subdirectory of the Mercury directory
            %   with --use-subdirs.

    ;       newext_analysis(ext_analysis)
            % Compiler-generated files that are part of the incomplete
            % attempt at an intermodule analysis and optimization framework
            % in analysis.m and its clients.

    ;       newext_bytecode(ext_bytecode)
            % Compiler-generated files that represent bytecode
            % for a long-ago attempt at a bytecode based Mercury debugger.

    ;       newext_misc_ngs(ext_misc_ngs)
    ;       newext_misc_gs(ext_misc_gs)
            % XXX Document me.

    ;       newext_other(other_newext).
            % The general case. The extension string must not be covered
            % by any of the other cases above.

:- type ext_int
    --->    ext_int_int0
    ;       ext_int_int1
    ;       ext_int_int2
    ;       ext_int_int3
    ;       ext_int_date_int0
    ;       ext_int_date_int12
    ;       ext_int_date_int3.

:- type ext_opt
    --->    ext_opt_plain
    ;       ext_opt_trans
    ;       ext_opt_date_plain
    ;       ext_opt_date_trans.

:- type ext_mh
    --->    ext_mh_mh.

:- type ext_mih
    --->    ext_mih_mih.

:- type ext_target_c_cs
    --->    ext_target_c
    ;       ext_target_cs.

:- type ext_target_java
    --->    ext_target_java_java
    ;       ext_target_java_class.

:- type ext_target_date
    --->    ext_target_date_c
    ;       ext_target_date_cs
    ;       ext_target_date_java.

:- type ext_obj
    --->    ext_obj_dollar_o
    ;       ext_obj_dollar_efpo
    ;       ext_obj_o
    ;       ext_obj_pic_o
    ;       ext_obj_obj_opt
    ;       ext_obj_pic_obj_opt.

:- type ext_init_c
    --->    ext_init_c.

:- type ext_init_obj
    --->    ext_init_obj_dollar_o
    ;       ext_init_obj_o
    ;       ext_init_obj_pic_o
    ;       ext_init_obj_obj_opt
    ;       ext_init_obj_pic_obj_opt.

:- type ext_exec
    --->    ext_exec_exe.

:- type ext_exec_gs
    --->    ext_exec_gs_noext
    ;       ext_exec_gs_bat
    ;       ext_exec_exec_opt.

:- type ext_lib
    --->    ext_lib_dollar_efsl
    ;       ext_lib_lib
    ;       ext_lib_so.

:- type ext_lib_gs
    --->    ext_lib_gs_dollar_a
    ;       ext_lib_gs_archive
    ;       ext_lib_gs_dll
    ;       ext_lib_gs_init
    ;       ext_lib_gs_jar
    ;       ext_lib_gs_lib_opt
    ;       ext_lib_gs_sh_lib_opt.

:- type ext_mmake_fragment
    --->    ext_mf_d
    ;       ext_mf_dv
    ;       ext_mf_dep
    ;       ext_mf_dir_sl_all_os.    % DODGY

:- type ext_mmake_target
    --->    ext_mt_all_int3s
    ;       ext_mt_all_ints
    ;       ext_mt_all_opts
    ;       ext_mt_all_trans_opts
    ;       ext_mt_check
    ;       ext_mt_classes
    ;       ext_mt_clean
    ;       ext_mt_depend
    ;       ext_mt_install_grade_hdrs
    ;       ext_mt_install_hdrs
    ;       ext_mt_install_ints
    ;       ext_mt_install_opts
    ;       ext_mt_int3s
    ;       ext_mt_ints
    ;       ext_mt_javas
    ;       ext_mt_opts
    ;       ext_mt_realclean
    ;       ext_mt_trans_opts.

:- type ext_user
    --->    ext_user_depgraph
    ;       ext_user_err
    ;       ext_user_hlds_dump
    ;       ext_user_mlds_dump
    ;       ext_user_order
    ;       ext_user_ugly.

:- type ext_user_ngs
    --->    ext_user_ngs_defn_ext
    ;       ext_user_ngs_defn_lc
    ;       ext_user_ngs_defns
    ;       ext_user_ngs_imports_graph
    ;       ext_user_ngs_lct
    ;       ext_user_ngs_lct_order
    ;       ext_user_ngs_mode_constr
    ;       ext_user_ngs_order_to
    ;       ext_user_ngs_type_repns
    ;       ext_user_ngs_xml.

:- type ext_analysis
    --->    ext_an_analysis
    ;       ext_an_date
    ;       ext_an_status
    ;       ext_an_imdg
    ;       ext_an_request.

:- type ext_bytecode
    --->    ext_bc_mbc
    ;       ext_bc_bytedebug.

:- type ext_misc_ngs
    --->    ext_misc_ngs_module_dep     % DODGY
    ;       ext_misc_ngs_err_date       % DODGY
    ;       ext_misc_ngs_prof.          % DODGY

:- type ext_misc_gs
    --->    ext_misc_gs_used
    ;       ext_misc_gs_track_flags.

    % XXX To be deleted soon.
:- type other_newext
    --->    other_newext(string).

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
    maybe_create_dirs::in, ext::in, newext::in,
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
:- pred module_name_to_search_file_name(globals::in, string::in,
    ext::in, newext::in, module_name::in, file_name::out,
    io::di, io::uo) is det.

    % module_name_to_lib_file_name(Globals, MkDir, Prefix, Ext,
    %   Module, FileName, !IO):
    %
    % Like module_name_to_file_name, but also allows a prefix.
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name(globals::in, string::in,
    maybe_create_dirs::in, string::in, other_ext::in, newext::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name(Globals, MkDir, Ext, FactTableFileName,
    %   FileName, !IO):
    %
    % Returns the filename to use when compiling fact table files.
    % If `MkDir' is do_create_dirs, then create any directories needed.
    %
:- pred fact_table_file_name(globals::in, string::in, maybe_create_dirs::in,
    other_ext::in, newext::in, file_name::in, file_name::out,
    io::di, io::uo) is det.

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

:- func old_extension_to_string(ext) = string.

old_extension_to_string(Ext) = ExtStr :-
    (
        Ext = ext_src,
        ExtStr = ".m"
    ;
        Ext = ext_other(OtherExt),
        ExtStr = other_extension_to_string(OtherExt)
    ).

:- func other_extension_to_string(other_ext) = string.

other_extension_to_string(OtherExt) = ExtStr :-
    OtherExt = other_ext(ExtStr).

extension_to_string(Ext, NewExt) = ExtStr :-
    ExtStr = old_extension_to_string(Ext),
    trace [run_time(not(env("NO_EXT_CHECKS"))), io(!TIO)]
    (
        NewExtStr = newext_to_string(NewExt),
        ( if ExtStr = NewExtStr then
            true
        else
            ExtRawStr = string.string(Ext),
            NewExtRawStr = string.string(NewExt),
            string.format("EXT_TO_STRING MISMATCH for %s/%s: %s vs %s\n",
                [s(ExtRawStr), s(NewExtRawStr), s(ExtStr), s(NewExtStr)], Msg),
            unexpected($pred, Msg)
        )
    ).

:- func newext_to_string(newext) = string.

newext_to_string(Ext) = ExtStr :-
    (
        Ext = newext_src,
        ExtStr = ".m"
    ;
        Ext = newext_int(ExtInt),
        ext_int_extension_dir(ExtInt, ExtStr, _SubDirName)
    ;
        Ext = newext_opt(ExtOpt),
        ext_opt_extension_dir(ExtOpt, ExtStr, _SubDirName)
    ;
        Ext = newext_mh(ExtMh),
        ext_mh_extension(ExtMh, ExtStr)
    ;
        Ext = newext_mih(ExtMh),
        ext_mih_extension_dir(ExtMh, ExtStr, _SubDirName)
    ;
        Ext = newext_target_c_cs(ExtCCs),
        ext_target_c_cs_extension_dir(ExtCCs, ExtStr, _SubDirName)
    ;
        Ext = newext_target_java(ExtJava),
        ext_target_java_extension_dirs(ExtJava, ExtStr, _SubDirNames)
    ;
        Ext = newext_target_date(ExtTargetDate),
        ext_target_date_extension_dir(ExtTargetDate, ExtStr, _SubDirName)
    ;
        Ext = newext_target_obj(ExtObj),
        ext_obj_extension(ExtObj, ExtStr)
    ;
        Ext = newext_target_init_c(ExtInitC),
        ext_init_c_extension_dir(ExtInitC, ExtStr, _SubDirName)
    ;
        Ext = newext_target_init_obj(ExtInitObj),
        ext_init_obj_extension(ExtInitObj, ExtStr)
    ;
        Ext = newext_exec(ExtExec),
        ext_exec_extension(ExtExec, ExtStr)
    ;
        Ext = newext_exec_gs(ExtExecGs),
        ext_exec_gs_extension(ExtExecGs, ExtStr)
    ;
        Ext = newext_lib(ExtLib),
        ext_lib_extension(ExtLib, ExtStr)
    ;
        Ext = newext_lib_gs(ExtLibGs),
        ext_lib_gs_extension(ExtLibGs, ExtStr)
    ;
        Ext = newext_mmake_fragment(ExtMf),
        ext_mmake_fragment_extension_dir(ExtMf, ExtStr, _SubDirName)
    ;
        Ext = newext_mmake_target(ExtMT),
        ext_mmake_target_extension(ExtMT, ExtStr)
    ;
        Ext = newext_user(ExtUser),
        ext_user_extension(ExtUser, ExtStr)
    ;
        Ext = newext_user_ngs(ExtUserNgs),
        ext_user_ngs_extension_dir(ExtUserNgs, ExtStr, _SubDirName)
    ;
        Ext = newext_analysis(ExtAn),
        ext_analysis_extension_dir(ExtAn, ExtStr, _SubDirName)
    ;
        Ext = newext_bytecode(ExtByte),
        ext_bytecode_extension_dir(ExtByte, ExtStr, _SubDirName)
    ;
        Ext = newext_misc_ngs(ExtMiscNgs),
        ext_misc_ngs_extension_dir(ExtMiscNgs, ExtStr, _SubDirName)
    ;
        Ext = newext_misc_gs(ExtMiscGs),
        ext_misc_gs_extension_dir(ExtMiscGs, ExtStr, _SubDirName)
    ;
        Ext = newext_other(_),
        unexpected($pred, "newext_other")
    ).

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

:- pred newext_uses_option(newext::in) is semidet.

newext_uses_option(Ext) :-
    require_complete_switch [Ext]
    (
        ( Ext = newext_src
        ; Ext = newext_analysis(_)
        ; Ext = newext_bytecode(_)
        ; Ext = newext_exec(_)
        ; Ext = newext_int(_)
        ; Ext = newext_lib(_)
        ; Ext = newext_mh(_)
        ; Ext = newext_mih(_)
        ; Ext = newext_misc_gs(_)
        ; Ext = newext_misc_ngs(_)
        ; Ext = newext_mmake_fragment(_)
        ; Ext = newext_mmake_target(_)
        ; Ext = newext_opt(_)
        ; Ext = newext_other(_)
        ; Ext = newext_target_c_cs(_)
        ; Ext = newext_target_date(_)
        ; Ext = newext_target_init_c(_)
        ; Ext = newext_target_java(_)
        ; Ext = newext_user(_)
        ; Ext = newext_user_ngs(_)
        ),
        fail
    ;
        Ext = newext_target_obj(ExtObj),
        require_complete_switch [ExtObj]
        (
            ( ExtObj = ext_obj_dollar_o
            ; ExtObj = ext_obj_dollar_efpo
            ; ExtObj = ext_obj_o
            ; ExtObj = ext_obj_pic_o
            ),
            fail
        ;
            ( ExtObj = ext_obj_obj_opt
            ; ExtObj = ext_obj_pic_obj_opt
            )
        )
    ;
        Ext = newext_target_init_obj(ExtInitObj),
        (
            ( ExtInitObj = ext_init_obj_dollar_o
            ; ExtInitObj = ext_init_obj_o
            ; ExtInitObj = ext_init_obj_pic_o
            ),
            fail
        ;
            ( ExtInitObj = ext_init_obj_obj_opt
            ; ExtInitObj = ext_init_obj_pic_obj_opt
            )
        )
    ;
        Ext = newext_exec_gs(ExtExecGs),
        (
            ( ExtExecGs = ext_exec_gs_noext
            ; ExtExecGs = ext_exec_gs_bat
            ),
            fail
        ;
            ExtExecGs = ext_exec_exec_opt
        )
    ;
        Ext = newext_lib_gs(ExtLibGs),
        (
            ( ExtLibGs = ext_lib_gs_dollar_a
            ; ExtLibGs = ext_lib_gs_archive
            ; ExtLibGs = ext_lib_gs_dll
            ; ExtLibGs = ext_lib_gs_init
            ; ExtLibGs = ext_lib_gs_jar
            ),
            fail
        ;
            ( ExtLibGs = ext_lib_gs_lib_opt
            ; ExtLibGs = ext_lib_gs_sh_lib_opt
            )
        )
    ).

module_name_to_file_name(Globals, From, MkDir, Ext, NewExt,
        ModuleName, FileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, do_not_search, MkDir,
        Ext, ModuleName, FileName, !IO),
    trace [run_time(not(env("NO_EXT_CHECKS"))), io(!TIO)]
    (
        module_name_to_file_name_ext_new(Globals, From, do_not_search, MkDir,
            NewExt, ModuleName, NewFileName, !TIO),
        ( if ( newext_uses_option(NewExt) ; FileName = NewFileName ) then
            true
        else
            ExtStr = string.string(Ext),
            NewExtStr = string.string(NewExt),
            string.format("FILENAME MISMATCH 1 for %s/%s: %s vs %s\n",
                [s(ExtStr), s(NewExtStr), s(FileName), s(NewFileName)], Msg),
            unexpected($pred, Msg)
        )
    ).

module_name_to_search_file_name(Globals, From, Ext, NewExt,
        ModuleName, FileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, do_search, do_not_create_dirs,
        Ext, ModuleName, FileName, !IO),
    trace [run_time(not(env("NO_EXT_CHECKS"))), io(!TIO)]
    (
        module_name_to_file_name_ext_new(Globals, From, do_search,
            do_not_create_dirs, NewExt, ModuleName, NewFileName, !TIO),
        ( if ( newext_uses_option(NewExt) ; FileName = NewFileName ) then
            true
        else
            ExtStr = string.string(Ext),
            NewExtStr = string.string(NewExt),
            string.format("FILENAME MISMATCH 2 for %s/%s: %s vs %s\n",
                [s(ExtStr), s(NewExtStr), s(FileName), s(NewFileName)], Msg),
            unexpected($pred, Msg)
        )
    ).

module_name_to_lib_file_name(Globals, From, MkDir, Prefix, Ext, NewExt,
        ModuleName, FileName, !IO) :-
    BaseFileName = sym_name_to_string(ModuleName),
    BaseNameNoExt = Prefix ++ BaseFileName,
    % Library files do not need the preprocessing that
    % module_name_to_file_name_ext does before calling choose_file_name.
    choose_file_name(Globals, From, do_not_search, Ext,
        [], BaseNameNoExt, DirComponents, FileName),
    maybe_create_dirs_on_path(MkDir, DirComponents, !IO),
    trace [run_time(not(env("NO_EXT_CHECKS"))), io(!TIO)]
    (
        FakeModuleName = unqualified(BaseNameNoExt),
        module_name_to_file_name_ext_new(Globals, From, do_not_search, MkDir,
            NewExt, FakeModuleName, NewFileName, !TIO),
        ( if ( newext_uses_option(NewExt) ; FileName = NewFileName ) then
            true
        else
            ExtStr = string.string(Ext),
            NewExtStr = string.string(NewExt),
            string.format("FILENAME MISMATCH 3 for %s/%s: %s vs %s\n",
                [s(ExtStr), s(NewExtStr), s(FileName), s(NewFileName)], Msg),
            unexpected($pred, Msg)
        )
    ).

fact_table_file_name(Globals, From, MkDir, Ext, NewExt,
        FactTableFileName, FileName, !IO) :-
    % Fact table files do not need the preprocessing that
    % module_name_to_file_name_ext does before calling choose_file_name.
    choose_file_name(Globals, From, do_not_search, Ext,
        [], FactTableFileName, DirComponents, FileName),
    maybe_create_dirs_on_path(MkDir, DirComponents, !IO),
    trace [run_time(not(env("NO_EXT_CHECKS"))), io(!TIO)]
    (
        FakeModuleName = unqualified(FactTableFileName),
        module_name_to_file_name_ext_new(Globals, From, do_not_search, MkDir,
            NewExt, FakeModuleName, NewFileName, !TIO),
        ( if ( newext_uses_option(NewExt) ; FileName = NewFileName ) then
            true
        else
            ExtStr = string.string(Ext),
            NewExtStr = string.string(NewExt),
            string.format("FILENAME MISMATCH 4 for %s/%s: %s vs %s\n",
                [s(ExtStr), s(NewExtStr), s(FileName), s(NewFileName)], Msg),
            unexpected($pred, Msg)
        )
    ).

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
    then
        SubDirName = "os"
    else if
        % _init.c, _init.cs, etc. files go in the cs, css, etc
        % subdirectories.
        string.remove_prefix("_init.", ExtStr, ExtName)
    then
        SubDirName = ExtName ++ "s"
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

:- func make_new_extension(string) = newext.

make_new_extension(ExtStr) = NewExt :-
    % Since the newext_exec_gs and newext_lib_gs alternatives
    % go in the current directory only with --no-use-grade-subdir, this
    % predicate is not well named. However, the transformation it performs
    % will become unnecessary once we switch over to using newexts exclusively
    % in the compiler, including at the call sites calling
    % module_name_to_file_name and its variants.
    %
    % XXX While separating newext_exec_gs from newext_exec
    % separating newext_lib_gs from newext_lib are needed to get
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
    %
    % The classifications marked "DODGY" below select a path in
    % module_name_to_file_name_ext_new that computes the right filename
    % (the filename computed by old code), but the name of their category
    % may not be applicable.
    ( if
        (
            ExtStr = ".m",
            NewExt0 = newext_src
        ;
            % Executable files.
            % XXX The Ext = "" here is wrong. While an empty extension
            % *can* mean we are building the name of an executable,
            % it can also mean we are building the name of a phony Mmakefile
            % target for a library, such as libmer_std in the library
            % directory.
            ( ExtStr = "",          ExtExecGs = ext_exec_gs_noext
            ; ExtStr = ".bat",      ExtExecGs = ext_exec_gs_bat
            ),
            NewExt0 = newext_exec_gs(ExtExecGs)
        ;
            ExtStr = ".exe",        ExtExec = ext_exec_exe,
            NewExt0 = newext_exec(ExtExec)
        ;
            % Library files.
            ( ExtStr = ".$A",       ExtLibGs = ext_lib_gs_dollar_a
            ; ExtStr = ".a",        ExtLibGs = ext_lib_gs_archive
            ; ExtStr = ".dll",      ExtLibGs = ext_lib_gs_dll
            ; ExtStr = ".init",     ExtLibGs = ext_lib_gs_init
            ; ExtStr = ".jar",      ExtLibGs = ext_lib_gs_jar
            ),
            NewExt0 = newext_lib_gs(ExtLibGs)
        ;
            ( ExtStr = ".$(EXT_FOR_SHARED_LIB)",
                                    ExtLib = ext_lib_dollar_efsl
            ; ExtStr = ".lib",      ExtLib = ext_lib_lib
            ; ExtStr = ".so",       ExtLib = ext_lib_so
            ),
            NewExt0 = newext_lib(ExtLib)
        ;
            % Machine-dependent header files for generated C code.
            % XXX There is no good reason for .mh files to be treated
            % differently from .mih files.
            ExtStr = ".mh",         ExtMh = ext_mh_mh,
            NewExt0 = newext_mh(ExtMh)
        ;
            ExtStr = ".mih",        ExtMih = ext_mih_mih,
            NewExt0 = newext_mih(ExtMih)
        ;
            % Output files intended for use by the user.
            % The MLDS dump files with extensions .c_dump* and .mih_dump*
            % also fit into this category, but their filenames are constructed
            % by getting the filenames for the .c and .mih extensions
            % and adding a suffix to that.
            ( ExtStr = ".dependency_graph",     EU = ext_user_depgraph
            ; ExtStr = ".err",                  EU = ext_user_err
            ; ExtStr = ".hlds_dump",            EU = ext_user_hlds_dump
            ; ExtStr = ".mlds_dump",            EU = ext_user_mlds_dump
            ; ExtStr = ".order",                EU = ext_user_order
            ; ExtStr = ".ugly",                 EU = ext_user_ugly
            ),
            NewExt0 = newext_user(EU)
        ;
            ( ExtStr = ".defn_extents",         EU = ext_user_ngs_defn_ext
            ; ExtStr = ".defn_line_counts",     EU = ext_user_ngs_defn_lc
            ; ExtStr = ".defns",                EU = ext_user_ngs_defns
            ; ExtStr = ".imports_graph",        EU = ext_user_ngs_imports_graph
            ; ExtStr = ".local_call_tree",      EU = ext_user_ngs_lct
            ; ExtStr = ".local_call_tree_order",EU = ext_user_ngs_lct_order
            ; ExtStr = ".mode_constraints",     EU = ext_user_ngs_mode_constr
            ; ExtStr = ".order_trans_opt",      EU = ext_user_ngs_order_to
            ; ExtStr = ".type_repns",           EU = ext_user_ngs_type_repns
            ; ExtStr = ".xml",                  EU = ext_user_ngs_xml
            ),
            NewExt0 = newext_user_ngs(EU)
        ;
            ( ExtStr = ".d",                    EMF = ext_mf_d
            ; ExtStr = ".dv",                   EMF = ext_mf_dv
            ; ExtStr = ".dep",                  EMF = ext_mf_dep
            ; ExtStr = ".dir/*.$O",             EMF = ext_mf_dir_sl_all_os
            ),
            NewExt0 = newext_mmake_fragment(EMF)
        ;
            % Mmake targets.
            ( ExtStr = ".all_int3s",            EMT = ext_mt_all_int3s
            ; ExtStr = ".all_ints",             EMT = ext_mt_all_ints
            ; ExtStr = ".all_opts",             EMT = ext_mt_all_opts
            ; ExtStr = ".all_trans_opts",       EMT = ext_mt_all_trans_opts
            ; ExtStr = ".check",                EMT = ext_mt_check
            ; ExtStr = ".classes",              EMT = ext_mt_classes
            ; ExtStr = ".clean",                EMT = ext_mt_clean
            ; ExtStr = ".depend",               EMT = ext_mt_depend
            ; ExtStr = ".install_grade_hdrs",   EMT = ext_mt_install_grade_hdrs
            ; ExtStr = ".install_hdrs",         EMT = ext_mt_install_hdrs
            ; ExtStr = ".install_ints",         EMT = ext_mt_install_ints
            ; ExtStr = ".install_opts",         EMT = ext_mt_install_opts
            ; ExtStr = ".int3s",                EMT = ext_mt_int3s
            ; ExtStr = ".ints",                 EMT = ext_mt_ints
            ; ExtStr = ".javas",                EMT = ext_mt_javas
            ; ExtStr = ".opts",                 EMT = ext_mt_opts
            ; ExtStr = ".realclean",            EMT = ext_mt_realclean
            ; ExtStr = ".trans_opts",           EMT = ext_mt_trans_opts
            ),
            NewExt0 = newext_mmake_target(EMT)
        ;
            ( ExtStr = ".int0",                 ExtInt = ext_int_int0
            ; ExtStr = ".int",                  ExtInt = ext_int_int1
            ; ExtStr = ".int2",                 ExtInt = ext_int_int2
            ; ExtStr = ".int3",                 ExtInt = ext_int_int3
            ; ExtStr = ".date0",                ExtInt = ext_int_date_int0
            ; ExtStr = ".date",                 ExtInt = ext_int_date_int12
            ; ExtStr = ".date3",                ExtInt = ext_int_date_int3
            ),
            NewExt0 = newext_int(ExtInt)
        ;
            ( ExtStr = ".opt",                  ExtOpt = ext_opt_plain
            ; ExtStr = ".trans_opt",            ExtOpt = ext_opt_trans
            ; ExtStr = ".optdate",              ExtOpt = ext_opt_date_plain
            ; ExtStr = ".trans_opt_date",       ExtOpt = ext_opt_date_trans
            ),
            NewExt0 = newext_opt(ExtOpt)
        ;
            ( ExtStr = ".c",                    ExtCCs = ext_target_c
            ; ExtStr = ".cs",                   ExtCCs = ext_target_cs
            ),
            NewExt0 = newext_target_c_cs(ExtCCs)
        ;
            ( ExtStr = ".class",                ExtJ = ext_target_java_class
            ; ExtStr = ".java",                 ExtJ = ext_target_java_java
            ),
            NewExt0 = newext_target_java(ExtJ)
        ;
            ( ExtStr = ".c_date",               ExtDate = ext_target_date_c
            ; ExtStr = ".cs_date",              ExtDate = ext_target_date_cs
            ; ExtStr = ".java_date",            ExtDate = ext_target_date_java
            ),
            NewExt0 = newext_target_date(ExtDate)
        ;
            ( ExtStr = ".$O",                   ExtObj = ext_obj_dollar_o
            ; ExtStr = ".$(EXT_FOR_PIC_OBJECTS)",ExtObj = ext_obj_dollar_efpo
            ; ExtStr = ".o",                    ExtObj = ext_obj_o
            ; ExtStr = ".pic_o",                ExtObj = ext_obj_pic_o
            ),
            NewExt0 = newext_target_obj(ExtObj)
        ;
            ExtStr = "_init.c", ExtIC = ext_init_c,
            NewExt0 = newext_target_init_c(ExtIC)
        ;
            ( ExtStr = "_init.$O",              ExtIO = ext_init_obj_dollar_o
            ; ExtStr = "_init.o",               ExtIO = ext_init_obj_o
            ; ExtStr = "_init.pic_o",           ExtIO = ext_init_obj_pic_o
            ),
            NewExt0 = newext_target_init_obj(ExtIO)
        ;
            ( ExtStr = ".mbc",                  ExtB = ext_bc_mbc
            ; ExtStr = ".bytedebug",            ExtB = ext_bc_bytedebug
            ),
            NewExt0 = newext_bytecode(ExtB)
        ;
            ( ExtStr = ".analysis",             ExtA = ext_an_analysis
            ; ExtStr = ".analysis_date",        ExtA = ext_an_date
            ; ExtStr = ".analysis_status",      ExtA = ext_an_status
            ; ExtStr = ".imdg",                 ExtA = ext_an_imdg
            ; ExtStr = ".request",              ExtA = ext_an_request
            ),
            NewExt0 = newext_analysis(ExtA)
        ;
            ( ExtStr = ".module_dep",           ExtM = ext_misc_ngs_module_dep
            ; ExtStr = ".err_date",             ExtM = ext_misc_ngs_err_date
            ; ExtStr = ".prof",                 ExtM = ext_misc_ngs_prof
            ),
            NewExt0 = newext_misc_ngs(ExtM)
        ;
            ( ExtStr = ".used",                 ExtM = ext_misc_gs_used
            ; ExtStr = ".track_flags",          ExtM = ext_misc_gs_track_flags
            ),
            NewExt0 = newext_misc_gs(ExtM)
        )
    then
        NewExt = NewExt0
    else
        unexpected($pred, "unrecognized ExtStr " ++ ExtStr)
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
        (
            Ext = newext_int(ExtInt),
            ext_int_extension_dir(ExtInt, ExtStr, SubDirName)
        ;
            Ext = newext_user_ngs(ExtUserNgs),
            ext_user_ngs_extension_dir(ExtUserNgs, ExtStr, SubDirName)
        ;
            Ext = newext_bytecode(ExtByte),
            ext_bytecode_extension_dir(ExtByte, ExtStr, SubDirName)
        ;
            Ext = newext_misc_ngs(ExtMiscNgs), % XXX Probably never used.
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
        Ext = newext_opt(ExtOpt),
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
        (
            Ext = newext_mh(ExtMh),
            ext_mh_extension(ExtMh, ExtStr)
        ;
            Ext = newext_exec(ExtExec),
            ext_exec_extension(ExtExec, ExtStr)
        ;
            Ext = newext_lib(ExtLib),
            ext_lib_extension(ExtLib, ExtStr)
        ;
            Ext = newext_mmake_target(ExtMT),
            ext_mmake_target_extension(ExtMT, ExtStr)
        ;
            Ext = newext_user(ExtUser),
            ext_user_extension(ExtUser, ExtStr)
        ),
        % Output files intended for use by the user, and phony Mmake target
        % names go in the current directory. So do .mh files, and *some*,
        % but not all, kinds of executable and library files.
        % XXX Why is that?
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        FileName = BaseNameNoExt ++ ExtStr
    ;
        Ext = newext_mih(ExtMh),
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
                    make_grade_subdir_file_name_new(Globals, [SubDirName],
                        BaseNameNoExt, ExtStr, DirComponents, FileName)
                ),
                maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
            )
        )
    ;
        (
            Ext = newext_target_c_cs(ExtCCs),
            ext_target_c_cs_extension_dir(ExtCCs, ExtStr, SubDirName)
        ;
            Ext = newext_target_date(ExtTargetDate),
            ext_target_date_extension_dir(ExtTargetDate, ExtStr, SubDirName)
        ;
            Ext = newext_misc_gs(ExtMiscGs), % XXX Probably never used.
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
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_target_java(ExtJava),
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
                make_grade_subdir_file_name_new(Globals, SubDirNames,
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            )
        ),
        maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
    ;
        (
            Ext = newext_target_obj(ExtObj),
            ext_obj_extension_dir(Globals, ExtObj, ExtStr, SubDirName)
        ;
            Ext = newext_target_init_obj(ExtInitObj),
            ext_init_obj_extension_dir(Globals, ExtInitObj, ExtStr, SubDirName)
        ),
        BaseNameNoExt = sym_name_to_string_sep(ModuleName, "."),
        globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
        (
            UseSubdirs = no,
            FileName = BaseNameNoExt ++ ExtStr
        ;
            UseSubdirs = yes,
            % There are two pieces of code here:
            %
            % - the one that is live, and
            % - the one that is commented out.
            %
            % The version that is commented out is illogical, as expounded
            % by the commented-out comment, but it is what was needed to pass
            % test_file_name_extensions, i.e. for the new filename translation
            % code to handle e.g. newext_target_obj(ext_obj_o) the same way
            % the old code handles ext_other(other_ext(".o")). With the code
            % that is live, test_file_name_extensions fails for five object
            % file extensions, .$O, .o, .pic_o, _init.o and _init.pic_o,
            % if use_grade_subdirs = yes. (There were ten failures overall,
            % because each extension fails with both search and no_search.)
            %
            % In each case, the issue is that file_is_arch_or_grade_dependent_2
            % does not list any of these extensions, though strangely,
            % it *does* list _init.$O. However, this does not matter
            % for four of the failing extensions, .o, .pic_o, _init.o and
            % _init.pic_o, because the rest of the compiler never directly
            % refers to these extensions; it always refers to them indirectly,
            % by looking up the value of the object_file_extension or
            % the pic_object_file_extension options, and maybe putting
            % "_init" in front of them. file_is_arch_or_grade_dependent,
            % the caller of file_is_arch_or_grade_dependent_2, does handle
            % these.
            %
            % The fifth extension, .$O, or ext_obj_dollar_o, is also handled
            % the same way by the live code as the other newext_target_obj
            % extensions (in that it is put into a grade-specific subdir
            % if use_grade_subdirs = yes), which is different from the way
            % that the old code handled it. However, I (zs) don't know
            % for sure which is the right way, because that extension is used
            % only by code in write_deps_file.m that generate mmakefile
            % fragments, which means any failure is indirect and delayed.
            % However, the fact that _init.$O has an explicit entry in
            % file_is_arch_or_grade_dependent_2 suggests to me that the
            % absence of .$O from that same predicate was an oversight.
            %
            % The entries of these five extensions are commented out
            % in string_extenions to allow the now-live code to pass
            % test_file_name_extensions.
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
%           % XXX EXT Why aren't object files for grades that differ in e.g.
%           % whether debugging is enabled stored in grade-specific directories
%           % if --use-grade-subdirs is enabled? The .c files that they are
%           % derived from *are* stored in grade-specific directories.
%           % I (zs) expect that the reason why this hasn't been a problem
%           % is that we don't target C with --use-grade-subdirs.
%           ( if
%               UseGradeSubdirs = yes,
%               Ext = newext_target_init_obj(ext_init_obj_dollar_o)
%           then
%               make_grade_subdir_file_name_new(Globals, [SubDirName],
%                   BaseNameNoExt, ExtStr, DirComponents, FileName)
%           else
%               DirComponents = ["Mercury", SubDirName],
%               FileName = glue_dir_names_file_name(DirComponents,
%                   BaseNameNoExt, ExtStr)
%           ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_target_init_c(ExtInitC),
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
                make_grade_subdir_file_name_new(Globals, [SubDirName],
                    BaseNameNoExt, ExtStr, DirComponents, FileName)
            ),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        (
            Ext = newext_exec_gs(ExtExecGs),
            ext_exec_gs_extension_dir(Globals, ExtExecGs, ExtStr, SubDirName)
        ;
            Ext = newext_lib_gs(ExtLibGs),
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
            make_grade_subdir_file_name_new(Globals, [SubDirName],
                BaseNameNoExt, ExtStr, DirComponents, FileName),
            maybe_create_dirs_on_path(MkDir, DirComponents, !IO)
        )
    ;
        Ext = newext_mmake_fragment(ExtMf),
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
        Ext = newext_analysis(ExtAn),
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
                % just search % in Mercury/<ext>s. This is so that searches
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

%---------------------------------------------------------------------------%

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

valid_other_newext(_) :-
    semidet_fail.

% XXX END OF CODE DUPLICATION

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

:- pred ext_obj_extension(ext_obj::in, string::out) is det.

ext_obj_extension(ext_obj_dollar_o,   ".$O").
ext_obj_extension(ext_obj_dollar_efpo, ".$(EXT_FOR_PIC_OBJECTS)").
ext_obj_extension(ext_obj_o,          ".o").
ext_obj_extension(ext_obj_pic_o,      ".pic_o").
ext_obj_extension(ext_obj_obj_opt, _) :-
    unexpected($pred, "ext_obj_obj_opt").
ext_obj_extension(ext_obj_pic_obj_opt, _) :-
    unexpected($pred, "ext_obj_pic_obj_opt").

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

:- pred ext_init_obj_extension(ext_init_obj::in, string::out) is det.

ext_init_obj_extension(ext_init_obj_dollar_o,  "_init.$O").
ext_init_obj_extension(ext_init_obj_o,         "_init.o").
ext_init_obj_extension(ext_init_obj_pic_o,     "_init.pic_o").
ext_init_obj_extension(ext_init_obj_obj_opt, _) :-
    unexpected($pred, "ext_init_obj_obj_opt").
ext_init_obj_extension(ext_init_obj_pic_obj_opt, _) :-
    unexpected($pred, "ext_init_obj_pic_obj_opt").

:- pred ext_exec_extension(ext_exec::in, string::out) is det.

ext_exec_extension(ext_exec_exe, ".exe").

:- pred ext_exec_gs_extension_dir(globals::in, ext_exec_gs::in,
    string::out, string::out) is det.

% Launcher scripts go in the `bin' subdirectory.
ext_exec_gs_extension_dir(_, ext_exec_gs_noext,    "",     "bin").
ext_exec_gs_extension_dir(_, ext_exec_gs_bat,      ".bat", "bats").
ext_exec_gs_extension_dir(Globals, ext_exec_exec_opt, ExtStr, "bin") :-
    globals.lookup_string_option(Globals, executable_file_extension, ExtStr).

:- pred ext_exec_gs_extension(ext_exec_gs::in, string::out) is det.

ext_exec_gs_extension(ext_exec_gs_noext,    "").
ext_exec_gs_extension(ext_exec_gs_bat,      ".bat").
ext_exec_gs_extension(ext_exec_exec_opt, _) :-
    unexpected($pred, "ext_exec_exec_opt").

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

:- pred ext_lib_gs_extension(ext_lib_gs::in, string::out) is det.

ext_lib_gs_extension(ext_lib_gs_dollar_a,   ".$A").
ext_lib_gs_extension(ext_lib_gs_archive,    ".a").
ext_lib_gs_extension(ext_lib_gs_dll,        ".dll").
ext_lib_gs_extension(ext_lib_gs_init,       ".init").
ext_lib_gs_extension(ext_lib_gs_jar,        ".jar").
ext_lib_gs_extension(ext_lib_gs_lib_opt, _) :-
    unexpected($pred, "ext_lib_gs_lib_opt").
ext_lib_gs_extension(ext_lib_gs_sh_lib_opt, _) :-
    unexpected($pred, "ext_lib_gs_sh_lib_opt").

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
%
% We don't test ".$(EXT_FOR_PIC_OBJECTS)", because with --use-subdirs
% and with --use-grade-subdirs, the OLD code produces nonsense results,
% while the new code does fine.
string_extensions =
    ["",
    % ".$(EXT_FOR_PIC_OBJECTS)",
    ".$(EXT_FOR_SHARED_LIB)",
    ".$A",
%   ".$O",
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
%   ".o",
    ".opt",
    ".optdate",
    ".opts",
    ".order",
    ".order_trans_opt",
%   ".pic_o",
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
    "_init.c"].
%   "_init.o",
%   "_init.pic_o"].

:- func option_extensions = list(option).

option_extensions =
    [executable_file_extension,
    library_extension,
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
