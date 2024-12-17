%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% Copyright (C) 2023-2024 The Mercury team.
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
:- import_module parse_tree.find_module.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % For some kinds of files, we know exactly in which directory
    % they should be; for other kinds, we may have to search several
    % directories. For the latter, our clients will need to call the
    % module_name_to_search_file_name predicate, which internally sets
    % Search to for_search.
    %
    % Note that do_create_dirs is not compatible with for_search; if you
    % know that one of several directories should contain a file, but don't
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
% The extensions in each category are treated the same by the predicates below.
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
% - a non-grade-specific subdirectory, which usually will be "Mercury/<X>s"
%   for some string X,
% - a grade-specific subdirectory, which will be
%   "Mercury/<grade>/<arch>/Mercury/<X>s" for some string X.
%   (See the comment in make_gs_dir_names for the rationale for this scheme.)
%
% (Some Java extensions are an exception; they include an extra "jmercury"
% component in the path.)
%
% Note that while some grade-specific files are also architecture-specific
% (i.e. their contents depend on the instruction set architecture (ISA)
% of the target platform), some are not. Our current LEGACY directory structure
% places both of these kinds of files into a directory whose name includes
% an <arch> component specifying the architecture. The PROPOSED structure
% we intend to replace it however makes a distinction between
%
% - a subdirectory that stores grade-specific files that are NOT
%   architecture-specific, and
% - a subdirectory that stores files that are both grade-specific
%   and architecture-specific.
%
% We include "gs" in the names of extensions whose files are grade-specific
% but not architecture-specific, while we use "gas" in the names of extensions
% whose files are both grade-specific and architecture-specific.
%
% We include "pgs" in the names of extensions whose files' *existence*
% is grade-specific, but whole files' *contents* is not. The one such
% extension is .mh. They exist only in C grades, but since they consists
% of the C declarations of the Mercury predicates and functions that
% the program exports to C, their content will be the same in *all* C grades.
% Because of this latter property, we treat pgs extensions the same way
% as we treat ngs extensions, but we do not expect to handle then at all
% in all grades.
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
% we omit either some, or (usually) all directory name components from
% the filename we return.
%
% (Yes, there are extension classes for which we *do* return some directory
% name components even when constructing a filename to be searched for.
% It is not (yet) clear to me (zs) whether this is an intentional choice,
% or whether what we do with for_search is immaterial because we *always*
% translate those extensions with not_for_search.)
%
% In the function symbols below,
% - "ngs" stands for the use of a non-grade-specific directory;
% - "pgs" stands for the use of a pseudo-grade-specific directory;
% - "gs" stands for the use of a grade-specific but not
%   architecture-specific directory; and
% - "gas" stands for the use of a grade-and-architecture-specific directory.
%
% NOTE The current decisions on what algorithm we use to decide the directory
% we use to store the files of any given extension were made when the code
% here was an over-complex mess, piling patch upon patch. Those decisions
% cannot be considered to represent the result of explicit deliberation.
% We should
%
% - decide *explicitly* for each extension exactly what directory
%   (or director*ies*) we want to put files with that extension into, and then
%
% - we should have a "flag day" when we flip the switch from the storing
%   extensions in their old, sometimes-chosen-by accident directories
%   to storing them in their new, deliberately chosen directories.

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

    ;       ext_cur_gas(ext_cur_gas)
            % All extensions whose files can get put either into the current
            % directory, or into a grade-and-architecture-specific
            % subdirectory.

    ;       ext_cur_ngs_gs(ext_cur_ngs_gs)
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory, or into
            % a grade-specific subdirectory, with search being irrelevant.

    ;       ext_cur_ngs_gas(ext_cur_ngs_gas)
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory, or into
            % a grade-and-architecture-specific subdirectory,
            % with search being irrelevant.

    ;       ext_cur_ngs_gs_err(ext_cur_ngs_gs_err)
            % Like the ext_cur_ngs_gs category, but whether or not .err files
            % are placed in the current directory, or into a non-grade-specific
            % subdirectory, or into a grade-specific subdirectory, also depends
            % on the --error-files-in-subdir option.

    ;       ext_cur_ngs_gs_java(ext_cur_ngs_gs_java)
            % All extensions using a java-specific set of rules.
            % With respect to directory structure, they use the same sort
            % of rules as the ext_cur_ngs_gs extensions, but the specifics
            % differ, and they also use a different algorithm for converting
            % module names to file names.

    ;       ext_cur_pgs_max_cur(ext_cur_pgs_max_cur)
            % All extensions whose files can get put either into the current
            % directory, or into a non-grade-specific subdirectory, with
            % search being specified restricting the options to just the first
            % alternative.

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
            % Compiler-generated files that are intended to be read
            % by the programmer.
    --->    ext_cur_user_defn_ext               % ".defn_extents"
    ;       ext_cur_user_defn_lc                % ".defn_line_counts"
    ;       ext_cur_user_defns                  % ".defns"
    ;       ext_cur_user_depgraph               % ".dependency_graph"
    ;       ext_cur_user_hlds_dump              % ".hlds_dump"
    ;       ext_cur_user_imports_graph          % ".imports_graph"
    ;       ext_cur_user_lct                    % ".local_call_tree"
    ;       ext_cur_user_lct_full               % ".local_call_tree_full"
    ;       ext_cur_user_lct_order              % ".local_call_tree_order"
    ;       ext_cur_user_mlds_dump              % ".mlds_dump"
    ;       ext_cur_user_mode_constr            % ".mode_constraints"
    ;       ext_cur_user_order                  % ".order"
    ;       ext_cur_user_order_to               % ".order_trans_opt"
    ;       ext_cur_user_type_repns             % ".type_repns"
    ;       ext_cur_user_ugly                   % ".ugly"
    ;       ext_cur_user_xml.                   % ".xml"

:- type ext_cur_ngs
            % Compiler-generated interface files, and the timestamp files
            % showing when they were last checked.
    --->    ext_cur_ngs_int_int0                % ".int0"
    ;       ext_cur_ngs_int_int1                % ".int"
    ;       ext_cur_ngs_int_int2                % ".int2"
    ;       ext_cur_ngs_int_int3                % ".int3"
    ;       ext_cur_ngs_int_date_int0           % ".date0"
    ;       ext_cur_ngs_int_date_int12          % ".date"
    ;       ext_cur_ngs_int_date_int3           % ".date3"

            % Compiler-generated files that are designed to be bodily included
            % in Mmakefiles.
    ;       ext_cur_ngs_mf_d                    % ".d"
    ;       ext_cur_ngs_mf_dv                   % ".dv"
    ;       ext_cur_ngs_mf_dep                  % ".dep"

            % Misc extensions.
    ;       ext_cur_ngs_misc_module_dep         % ".module_dep"
            % XXX DODGY What is the correctness argument for making this
            % a NON-grade-specific extension? If *anything* in a .module_dep
            % file can *ever* be grade dependent, this should be a
            % grade-specific extension.
    ;       ext_cur_ngs_misc_call_graph_for_prof.   % ".prof"
            % Despite the extension name, .prof files do not actually contain
            % profiling information. Instead, they contain a description of
            % a given module's call graph, with the intention being that
            % the user gives a list of the .prof files of all the modules
            % in the program to the mprof program, allowing mprof to build up
            % the call graph of the entire program.
            %
            % XXX This extension should be renamed. In a post to m-rev on
            % 2024 Aug 10, Julien suggested some possible replacements:
            % .callgraph, .staticcg, .stcg and .scg. I (zs) think that
            % the name should include a reference to profiling as well.
            % Finding a name that is short *as well as* descriptive
            % is not easy. I (zs) updated the internal name of the extension;
            % the user-facing extension, and the name of the directory
            % in which files with that extension are stored, still await
            % renaming.
            %
            % A module's call graph in general depends on the target language.
            % This is because a predicate may have a definition that is
            % Mercury clauses for some target languages and a foreign proc
            % for some other target languages, and the Mercury clauses
            % will in general include calls that the foreign proc does not.
            % However, we support mprof only when targeting C, so the only
            % grade differences that count are differences between *C* grades.
            %
            % We support foreign_procs for C that are specific to either
            % the LLDS or the MLDS backend, but this capabiity is intended
            % only for Mercury developers; it is neither documented nor
            % intended for use by for anyone else. Given that fact,
            % the contents of .prof files will be the same for all grades
            % for all programs by non-Mercury-developers, so it is ok
            % to classify them as effectively a non-grade-specific files.

:- type ext_cur_gs
    --->    ext_cur_gs_lib_init                 % ".init"
    ;       ext_cur_gs_lib_jar                  % ".jar"
            % ext_cur_gs_lib_cil_dll applies to .dll files constructed by
            % C# compilers that are told to generate CIL code. CIL, also
            % called MS-IL, is the "Common Intermediate Language" of the
            % .NET platform. Just as .jar files contain Java bytecodes
            % that are not target-specific, .dll files contain CIL bytecodes
            % that are also not target-specific. They are both bytecodes
            % for a *virtual* machine; they are not for any *real* machine.
    ;       ext_cur_gs_lib_cil_dll.            % ".dll"

:- type ext_cur_gas
            % Executables generated for a whole program.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.
            %
            % XXX According to the documentation of the --use-grade-subdirs
            % option, *all* executables and libraries *should* be put
            % into a grade subdir if that option is specified, not just some.
            % They should then be copied or linked to the current directory.
    --->    ext_cur_gas_exec_noext              % ""
            % XXX While an empty extension *usually means we are building
            % the name of an executable, it can also mean we are building
            % the name of a phony Mmakefile target for a library, such as
            % libmer_std in the library directory.

    ;       ext_cur_gas_exec_exe                % ".exe"
    ;       ext_cur_gas_exec_bat                % ".bat"
    ;       ext_cur_gas_exec_exec_opt           % executable_file_extension

            % Libraries, which may be statically or dynamically linked,
            % generated for a set of modules.
            %
            % Most of these extensions are intended to name real files,
            % but some are intended to name mmake targets.
    ;       ext_cur_gas_lib_dollar_efsl         % ".(EXT_FOR_SHARED_LIB)"
%   ;       ext_cur_gas_lib_lib                 % ".lib"
%   ;       ext_cur_gas_lib_so                  % ".so"
            % NOTE Neither ext_cur_gas_lib_lib nor ext_cur_gas_lib_so are
            % ever referred to by that name. All references to files with
            % those extensions use ext_cur_gas_lib_lib_opt and
            % ext_cur_gas_lib_sh_lib_opt.
    ;       ext_cur_gas_lib_dollar_a            % ".$A"
    ;       ext_cur_gas_lib_archive             % ".a"
    ;       ext_cur_gas_lib_lib_opt             % library_extension
    ;       ext_cur_gas_lib_sh_lib_opt.         % shared_library_extension

:- type ext_cur_ngs_gs
            % Compiler-generated optimization files.
    --->    ext_cur_ngs_gs_proposed_opt_plain    % ".opt"
    ;       ext_cur_ngs_gs_proposed_opt_trans    % ".trans_opt"

            % Timestamp files showing when their corresponding .*opt files
            % were last checked.
    ;       ext_cur_ngs_gs_opt_date_plain       % ".optdate"
    ;       ext_cur_ngs_gs_opt_date_trans       % ".trans_opt_date"

            % C and C# source files generated by the Mercury compiler.
    ;       ext_cur_ngs_gs_target_c             % ".c"
    ;       ext_cur_ngs_gs_target_cs            % ".cs"

            % Timestamp files that record the date and time when a target
            % language (C, C# or Java) source files was last logically remade.
            % (The "logically" parts means that if the new, up-to-date version
            % is bit-for-bit identical to the old version, then we update
            % the timestamp file, but not the file it refers to.)
    ;       ext_cur_ngs_gs_target_date_c        % ".c_date"
    ;       ext_cur_ngs_gs_target_date_cs       % ".cs_date"
    ;       ext_cur_ngs_gs_target_date_java     % ".java_date"

            % C files associated not with a module but with a whole program,
            % containing the code needed to initialize various tables for
            % the runtime system.
    ;       ext_cur_ngs_gs_init_c               % ".init_c"

            % Compiler-generated files that are part of the incomplete
            % attempt at an intermodule analysis and optimization framework
            % in the analysis.m package and its clients.
    ;       ext_cur_ngs_gs_an_analysis_date     % ".analysis_date"
    ;       ext_cur_ngs_gs_an_analysis_status   % ".analysis_status"

            % Misc extensions.
    ;       ext_cur_ngs_gs_misc_err_date        % ".err_date"
    ;       ext_cur_ngs_gs_misc_used            % ".used"
    ;       ext_cur_ngs_gs_misc_track_flags.    % ".track_flags"

:- type ext_cur_ngs_gas
            % Object files generated for C source files generated for a module
            % by the Mercury compiler.
    --->    ext_cur_ngs_gas_obj_dollar_o        % ".$O"
    ;       ext_cur_ngs_gas_obj_dollar_efpo     % ".$(EXT_FOR_PIC_OBJECTS)"
    ;       ext_cur_ngs_gas_obj_o               % ".o"
    ;       ext_cur_ngs_gas_obj_pic_o           % ".pic_o"
    ;       ext_cur_ngs_gas_obj_obj_opt         % object_file_extension option
    ;       ext_cur_ngs_gas_obj_pic_obj_opt  % pic_object_file_extension option

            % Object files associated not with a module but with
            % a whole program, containing the code needed to initialize
            % various tables for the runtime system.
    ;       ext_cur_ngs_gas_init_obj_dollar_o   % ".init.$O"
    ;       ext_cur_ngs_gas_init_obj_o          % ".init.c"
    ;       ext_cur_ngs_gas_init_obj_pic_o      % ".init.pic_o"
    ;       ext_cur_ngs_gas_init_obj_obj_opt
                                        % "_init" ++ object_file_extension
    ;       ext_cur_ngs_gas_init_obj_pic_obj_opt.
                                        % "_init" ++ pic_object_file_extension

:- type ext_cur_ngs_gs_err
    --->    ext_cur_ngs_gs_err_err.             % ".err"

:- type ext_cur_ngs_gs_java
            % Java source files generated by the Mercury compiler.
            % The names of these files, and of the directories
            % that store them, are computed by a different algorithm
            % from the ones applicable to C and C# source files.
    --->    ext_cur_ngs_gs_java_java            % ".java"
    ;       ext_cur_ngs_gs_java_class.          % ".class"

:- type ext_cur_pgs_max_cur
            % Compiler-generated C header file for a module that is intended
            % for inclusion by user-written C source files.
    --->    ext_cur_pgs_max_cur_mh.             % ".mh"

:- type ext_cur_ngs_gs_max_cur
            % Compiler-generated header file for a module that is intended
            % for inclusion by Mercury-generated C source files.
    --->    ext_cur_ngs_gs_max_cur_mih.         % ".mih"

:- type ext_cur_ngs_gs_max_ngs
            % Compiler-generated optimization files.
            % To be replaced by the "proposed" versions above.
    --->    ext_cur_ngs_gs_max_ngs_legacy_opt_plain    % ".opt"
    ;       ext_cur_ngs_gs_max_ngs_legacy_opt_trans    % ".trans_opt"

            % Compiler-generated files that are part of the incomplete
            % attempt at an intermodule analysis and optimization framework
            % in analysis.m and its clients.
    ;       ext_cur_ngs_gs_max_ngs_an_analysis  % ".analysis"
    ;       ext_cur_ngs_gs_max_ngs_an_imdg      % ".imdg"
            % "imdg" stands for "InterModule Dependency Graph".
    ;       ext_cur_ngs_gs_max_ngs_an_request.  % ".request"

%---------------------%

:- inst ext_int for ext/0
    --->    ext_cur_ngs(ext_int_0123).

:- inst ext_int_0123 for ext_cur_ngs/0
    --->    ext_cur_ngs_int_int0
    ;       ext_cur_ngs_int_int1
    ;       ext_cur_ngs_int_int2
    ;       ext_cur_ngs_int_int3.

:- inst ext_int_md for ext/0
    --->    ext_cur_ngs(ext_int_0123_md).

:- inst ext_int_0123_md for ext_cur_ngs/0
    --->    ext_cur_ngs_int_int0
    ;       ext_cur_ngs_int_int1
    ;       ext_cur_ngs_int_int2
    ;       ext_cur_ngs_int_int3
    ;       ext_cur_ngs_misc_module_dep.

:- inst ext_opt for ext/0
    --->    ext_cur_ngs_gs_max_ngs(ext_max_opt_pt)
    ;       ext_cur_ngs_gs(ext_opt_pt).

:- inst ext_max_opt_pt for ext_cur_ngs_gs_max_ngs/0
    --->    ext_cur_ngs_gs_max_ngs_legacy_opt_plain
    ;       ext_cur_ngs_gs_max_ngs_legacy_opt_trans.

:- inst ext_opt_pt for ext_cur_ngs_gs/0
    --->    ext_cur_ngs_gs_proposed_opt_plain
    ;       ext_cur_ngs_gs_proposed_opt_trans.

:- inst ext_analysis for ext/0
    --->    ext_cur_ngs_gs_max_ngs(ext_analysis_rai)
    ;       ext_cur_ngs_gs(ext_analysis_sd).

:- inst ext_analysis_rai for ext_cur_ngs_gs_max_ngs/0
    --->    ext_cur_ngs_gs_max_ngs_an_request
    ;       ext_cur_ngs_gs_max_ngs_an_imdg
    ;       ext_cur_ngs_gs_max_ngs_an_analysis.

:- inst ext_analysis_sd for ext_cur_ngs_gs/0
    --->    ext_cur_ngs_gs_an_analysis_status
    ;       ext_cur_ngs_gs_an_analysis_date.

    % The "mt" is short for make target, because ext_mt describes
    % the set of extensions that target_type_to_target_extension can return.
:- inst ext_mt for ext/0
    --->    ext_cur(mt_ext_cur)
    ;       ext_cur_ngs(mt_ext_cur_ngs)
    ;       ext_cur_ngs_gs(mt_ext_cur_ngs_gs)
    ;       ext_cur_ngs_gs_err(mt_ext_cur_ngs_gs_err)
    ;       ext_cur_ngs_gas(mt_ext_cur_ngs_gas)
    ;       ext_cur_ngs_gs_max_ngs(mt_ext_cur_ngs_gs_max_ngs)
    ;       ext_cur_ngs_gs_max_cur(mt_ext_cur_ngs_gs_max_cur)
    ;       ext_cur_ngs_gs_java(mt_ext_cur_ngs_gs_java)
    ;       ext_cur_pgs_max_cur(mt_ext_cur_pgs_max_cur).

:- inst mt_ext_cur for ext_cur/0
    --->    ext_cur_user_xml.
:- inst mt_ext_cur_ngs for ext_cur_ngs/0
    --->    ext_cur_ngs_int_int0
    ;       ext_cur_ngs_int_int1
    ;       ext_cur_ngs_int_int2
    ;       ext_cur_ngs_int_int3.
:- inst mt_ext_cur_ngs_gs for ext_cur_ngs_gs/0
    --->    ext_cur_ngs_gs_target_c
    ;       ext_cur_ngs_gs_target_cs
    ;       ext_cur_ngs_gs_misc_track_flags.
:- inst mt_ext_cur_ngs_gs_err for ext_cur_ngs_gs_err/0
    --->    ext_cur_ngs_gs_err_err.
:- inst mt_ext_cur_ngs_gs_java for ext_cur_ngs_gs_java/0
    --->    ext_cur_ngs_gs_java_java
    ;       ext_cur_ngs_gs_java_class.
:- inst mt_ext_cur_ngs_gs_max_ngs for ext_cur_ngs_gs_max_ngs/0
    --->    ext_cur_ngs_gs_max_ngs_legacy_opt_plain
    ;       ext_cur_ngs_gs_max_ngs_an_analysis.
:- inst mt_ext_cur_ngs_gs_max_cur for ext_cur_ngs_gs_max_cur/0
    --->    ext_cur_ngs_gs_max_cur_mih.
:- inst mt_ext_cur_pgs_max_cur for ext_cur_pgs_max_cur/0
    --->    ext_cur_pgs_max_cur_mh.
:- inst mt_ext_cur_ngs_gas for ext_cur_ngs_gas/0
    --->    ext_cur_ngs_gas_obj_obj_opt
    ;       ext_cur_ngs_gas_obj_pic_obj_opt.

%---------------------%

:- func extension_to_string(globals, ext) = string.

:- pred ext_cur_ngs_extension_dir(ext_cur_ngs::in,
    string::out, string::out) is det.
:- pred ext_cur_gs_extension_dir(ext_cur_gs::in,
    string::out, string::out, string::out) is det.
:- pred ext_cur_gas_extension_dir(globals::in, ext_cur_gas::in,
    string::out, string::out) is det.
:- pred ext_cur_ngs_gs_extension_dir(ext_cur_ngs_gs::in,
    string::out, string::out) is det.
:- pred ext_cur_pgs_max_cur_extension_dir(ext_cur_pgs_max_cur::in,
    string::out, string::out) is det.
:- pred ext_cur_ngs_gs_max_cur_extension_dir(ext_cur_ngs_gs_max_cur::in,
    string::out, string::out) is det.
:- pred ext_cur_ngs_gs_max_ngs_extension_dir(ext_cur_ngs_gs_max_ngs::in,
    string::out, string::out) is det.

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
    % The first, the "full" filename may be in a (non-grade-specific or
    % grade-specific) directory, or it can be in the current directory.
    % The second, the "curdir" filename will always be in the current
    % directory.
    %
    % The versions whose names include only "curdir" (without a "full")
    % return only the filename in the current directory.
    %
    % Note that these predicates are also used to create some "phony" Makefile
    % targets that do not have corresponding files, e.g. `<foo>.clean'.
    %
:- pred module_name_to_file_name_return_dirs(globals::in,
    string::in, ext::in, module_name::in,
    list(dir_name)::out, list(dir_name)::out,
    file_name::out, file_name::out) is det.
:- pred module_name_to_file_name(globals::in,
    string::in, ext::in, module_name::in, file_name::out, file_name::out)
    is det.
:- pred module_name_to_file_name_curdir(globals::in,
    string::in, ext::in, module_name::in, file_name::out) is det.
:- pred module_name_to_file_name_full_curdir(globals::in,
    string::in, ext::in, module_name::in, file_name::out, file_name::out,
    file_name::out) is det.
:- pred module_name_to_file_name_create_dirs(globals::in,
    string::in, ext::in, module_name::in, file_name::out, file_name::out,
    io::di, io::uo) is det.
:- pred module_name_to_file_name_full_curdir_create_dirs(globals::in,
    string::in, ext::in, module_name::in, file_name::out,
    file_name::out, file_name::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type search_which_dirs
    --->    search_cur_dir
    ;       search_this_dir(dir_name)
    ;       search_this_dir_and(dir_name, search_which_tail_dirs)
    ;       search_normal_dirs
    ;       search_intermod_dirs
    ;       search_dirs_for_ext.

    % This type differs from search_which_dirs by
    %
    % - not including search_{cur,this}_dir_and, thus limiting
    %   recursion to one level, and
    %
    % - by not including the search kinds that we do not actually use
    %   as an (either an actual or potential) tail search kind
    %   in search_{cur,this}_dir_and.
    %
:- type search_which_tail_dirs =< search_which_dirs
    --->    search_cur_dir
    ;       search_normal_dirs
    ;       search_intermod_dirs.

:- inst search_cur_or_normal for search_which_dirs/0
    --->    search_cur_dir
    ;       search_normal_dirs.

:- inst search_cur_or_intermod for search_which_dirs/0
    --->    search_cur_dir
    ;       search_intermod_dirs.

:- inst search_intermod for search_which_dirs/0
    --->    search_intermod_dirs.

:- inst search_ext for search_which_dirs/0
    --->    search_dirs_for_ext.

%---------------------%

    % module_name_to_search_file_name(Globals, From, Ext, ModuleName,
    %   SearchWhichDirs, SearchAuthDirs,
    %   FullFileNameLegacy, FullFileNameProposed):
    %
    % This predicate is a version of module_name_to_file_name whose job is
    % to return a file name (relative path) that we should *search for*
    % in a given list of directories, rather than the file name where the
    % Ext file for ModuleName definitely *should be*. This is why you should
    % use module_name_to_file_name if you want the file name that you should
    % write the file's contents to, or the file name you want to delete
    % as part of the "clean" or "realclean" mmake or mmc --make targets.
    % You should use this predicate if you want to read a file that may be
    % in a different source directory in a workspace, or in an installed
    % library.
    %
    % There are no variants with _return_dirs or _create_dirs suffixes,
    % because there is no point in creating the directories you are trying
    % to search; if you have to create a directory, it won't contain the file
    % you are looking for.
    %
    % The SearchWhichDirs argument should specify the list of directories
    % you want to search for the file name returned by this predicate.
    % There are rules about which extensions can be searched for in what
    % list of directories, because it does not make sense to e.g. search for
    % files with extensions other than .init in the directories specified
    % by the --init-file-directories option. The modes of this predicate
    % check that the insts (and therefore the values) of Ext and
    % SearchWhichDirs are compatible; if they are not compatible,
    % the call will get an intentional mode error.
    %
    % If the values of Ext and SearchWhichDirs *are* compatible, this
    % predicate will return in SearchAuthDirs a "search authorization".
    % All the predicates in find_module.m that do the searches for the
    % returned filenames insist on their callers providing them with one.
    %
:- pred module_name_to_search_file_name(globals, string, ext, module_name,
    search_which_dirs, search_auth_dirs, file_name, file_name).
:- mode module_name_to_search_file_name(in, in,
    in(ext_int_md), in, in(search_cur_or_normal), out, out, out) is det.
:- mode module_name_to_search_file_name(in, in,
    in(ext_opt), in, in(search_cur_or_intermod), out, out, out) is det.
:- mode module_name_to_search_file_name(in, in,
    in(ext_analysis), in, in(search_intermod), out, out, out) is det.
:- mode module_name_to_search_file_name(in, in,
    in(ext_mt), in, in(search_ext), out, out, out) is det.

%---------------------------------------------------------------------------%

    % module_name_to_lib_file_name_return_dirs(Globals, From, Prefix, Ext,
    %   Module, DirNames, FileName):
    %
    % Like module_name_to_file_name_return_dirs, but also allows a prefix.
    % The variants without the _return_dirs suffix and with the _create_dirs
    % suffix, and with _full_curdir, mean the same thing as with
    % module_name_to_file_name_return_dirs.
    %
    % Used for creating library names, e.g. `lib<foo>.$A' and `lib<foo>.so'.
    %
:- pred module_name_to_lib_file_name_return_dirs(globals::in, string::in,
    string::in, ext::in, module_name::in,
    list(dir_name)::out, list(dir_name)::out,
    file_name::out, file_name::out) is det.
:- pred module_name_to_lib_file_name(globals::in, string::in,
    string::in, ext::in, module_name::in,
    file_name::out, file_name::out) is det.
:- pred module_name_to_lib_file_name_full_curdir(globals::in, string::in,
    string::in, ext::in, module_name::in, file_name::out, file_name::out,
    file_name::out) is det.
:- pred module_name_to_lib_file_name_create_dirs(globals::in, string::in,
    string::in, ext::in, module_name::in, file_name::out,file_name::out,
    io::di, io::uo) is det.
:- pred module_name_to_lib_file_name_full_curdir_create_dirs(globals::in,
    string::in, string::in, ext::in, module_name::in,
    file_name::out, file_name::out, file_name::out, io::di, io::uo) is det.

    % A special case for the simplest kind of extensions, the extensions
    % for the files that are always put into the current directory.
    %
:- pred module_name_to_cur_dir_file_name(ext_cur::in, module_name::in,
    file_name::out) is det.

    % Take a module name and return a list of the file names
    % which need to be up-to-date to avoid recompilation.
    %
:- pred module_name_to_target_file_name_create_dirs(globals::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % Take a module name and return a list of the file names
    % which should be touched if the module does not need to be recompiled.
    %
:- pred module_name_to_target_timestamp_file_name_create_dirs(globals::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

    % fact_table_file_name(Globals, Ext, FactTableFileName,
    %   FullPathName):
    % fact_table_file_name_return_dirs(Globals, Ext, FactTableFileName,
    %   DirNames, FullPathName):
    %
    % Returns the full path name to use when compiling fact table files.
    % The second version returns the directory path in FullPathName
    % as DirNames. The caller can then create those directories
    % (with create_any_dirs_on_path below), or not, as they wish.
    %
:- pred fact_table_file_name(globals::in, string::in,
    ext::in, file_name::in, file_name::out, file_name::out) is det.
:- pred fact_table_file_name_return_dirs(globals::in, string::in,
    ext::in, file_name::in, list(dir_name)::out, list(dir_name)::out,
    file_name::out, file_name::out) is det.

%---------------------------------------------------------------------------%

    % Return the directory path of the directory into which files
    % with the given extension should be put.
    %
:- pred ext_to_dir_path(globals::in, maybe_for_search::in, ext::in,
    list(dir_name)::out, list(dir_name)::out) is det.

:- pred analysis_cache_dir_name(globals::in, string::out, string::out) is det.

%---------------------------------------------------------------------------%

    % make_selected_proposed_dir_name_gas(SubdirSetting, Grade, TargetArch,
    %   ExtSubDir, PrefixDir, Dir):
    %
    % The caller should pass us
    %
    % - SubdirSetting, the value of globals.get_subdir_setting;
    % - Grade, the value of globals.get_grade_dir;
    % - TargetArch, the value of the target_arch option,
    % - ExtSubDir, the subdir name in which the files of a given extension,
    %   which should be grade-specific and architecture-specific,
    %   should be stored, and
    % - PrefixDir, the name of either a workspace directory or an install
    %   directory,
    %
    % We will then return in Dir the name of the directory within PrefixDir
    % that contains files of that extension with SubdirSetting.
    %
:- pred make_selected_proposed_dir_name_gas(subdir_setting::in, dir_name::in,
    dir_name::in, dir_name::in, dir_name::in, dir_name::out) is det.

    % make_all_proposed_dir_names_gas(Grade, TargetArch, ExtSubDir,
    %   PrefixDir, Dirs):
    %
    % The meanings of the input arguments are the same as for
    % make_selected_proposed_dir_name_gas. But instead of returning the one
    % directory name selected by a given value of SubdirSetting, return
    % the dir names for all three possible values of SubdirSetting, with the
    % order being grade-specific dir, non-grade-specific dir, and current dir.
    %
:- pred make_all_proposed_dir_names_gas(dir_name::in, dir_name::in,
    dir_name::in, dir_name::in, list(dir_name)::out) is det.

%---------------------------------------------------------------------------%

    % make_selected_proposed_dir_name_gs(SubdirSetting, Grade, ExtSubDir,
    %   PrefixDir, Dir):
    %
    % The meanings of the input arguments are the same as for
    % make_selected_proposed_dir_name_gas, but we do not take the TargetArch
    % argument, and ExtSubDir should be the name of a subdir name containing
    % files which are grade-specific but not architecture-specific.
    %
:- pred make_selected_proposed_dir_name_gs(subdir_setting::in, dir_name::in,
    dir_name::in, dir_name::in, dir_name::out) is det.

    % make_all_proposed_dir_names_gs(Grade, ExtSubDir, PrefixDir, Dirs):
    %
    % The meanings of the input arguments are the same as for
    % make_selected_proposed_dir_name_gs. But instead of returning the one
    % directory name selected by a given value of SubdirSetting, return
    % the dir names for all three possible values of SubdirSetting, with the
    % order being grade-specific dir, non-grade-specific dir, and current dir.
    %
:- pred make_all_proposed_dir_names_gs(dir_name::in,
    dir_name::in, dir_name::in, list(dir_name)::out) is det.

%---------------------------------------------------------------------------%

    % make_selected_proposed_dir_name_ngs(SubdirSetting, ExtSubDir,
    %   PrefixDir, Dir):
    %
    % This predicate does the same job as make_selected_proposed_dir_name_gs,
    % but for non-grade-specific extensions.
    %
:- pred make_selected_proposed_dir_name_ngs(subdir_setting::in, dir_name::in,
    dir_name::in, dir_name::out) is det.

    % make_all_proposed_dir_names_ngs(ExtSubDir, PrefixDir, Dirs):
    %
    % This predicate does the same job as make_all_proposed_dir_names_gs,
    % but for non-grade-specific extensions.
    %
:- pred make_all_proposed_dir_names_ngs(dir_name::in, dir_name::in,
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
    % is specified by the given directory component names.
    %
:- pred maybe_create_any_dirs_on_path(maybe_create_dirs::in,
    list(string)::in, io::di, io::uo) is det.

    % create_any_dirs_on_path(DirNames, !IO):
    %
    % Create the directory whose name is specified by the given directory
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
    list(dir_name)::out, list(dir_name)::out) is det.

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
        ext_cur_gs_extension_dir(ExtCurGs, ExtStr,
            _LegacySubDirName, _ProposedSubDirName)
    ;
        Ext = ext_cur_gas(ExtCurGas),
        ext_cur_gas_extension_dir(Globals, ExtCurGas, ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gs(ExtCurNgsGs),
        ext_cur_ngs_gs_extension_dir(ExtCurNgsGs, ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gas(ExtCurNgsGas),
        ext_cur_ngs_gas_extension_dir(Globals, ExtCurNgsGas,
            ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gs_err(ExtCurNgsGsErr),
        ext_cur_ngs_gs_err_extension_dir(ExtCurNgsGsErr,
            ExtStr, _SubDirName)
    ;
        Ext = ext_cur_ngs_gs_java(ExtCurNgsGsJava),
        ext_cur_ngs_gs_java_extension_dir(ExtCurNgsGsJava,
            ExtStr, _SubDirName)
    ;
        Ext = ext_cur_pgs_max_cur(ExtCurNgsGsMaxCur),
        ext_cur_pgs_max_cur_extension_dir(ExtCurNgsGsMaxCur,
            ExtStr, _SubDirName)
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

ext_cur_extension(Ext, Str) :-
    ( Ext = ext_cur_user_defn_ext,            Str = ".defn_extents"
    ; Ext = ext_cur_user_defn_lc,             Str = ".defn_line_counts"
    ; Ext = ext_cur_user_defns,               Str = ".defns"
    ; Ext = ext_cur_user_depgraph,            Str = ".dependency_graph"
    ; Ext = ext_cur_user_hlds_dump,           Str = ".hlds_dump"
    ; Ext = ext_cur_user_imports_graph,       Str = ".imports_graph"
    ; Ext = ext_cur_user_lct,                 Str = ".local_call_tree"
    ; Ext = ext_cur_user_lct_full,            Str = ".local_call_tree_full"
    ; Ext = ext_cur_user_lct_order,           Str = ".local_call_tree_order"
    ; Ext = ext_cur_user_mlds_dump,           Str = ".mlds_dump"
    ; Ext = ext_cur_user_mode_constr,         Str = ".mode_constraints"
    ; Ext = ext_cur_user_order,               Str = ".order"
    ; Ext = ext_cur_user_order_to,            Str = ".order_trans_opt"
    ; Ext = ext_cur_user_type_repns,          Str = ".type_repns"
    ; Ext = ext_cur_user_ugly,                Str = ".ugly"
    ; Ext = ext_cur_user_xml,                 Str = ".xml"
    ).

ext_cur_ngs_extension_dir(Ext, Str, Dir) :-
    ( Ext = ext_cur_ngs_int_int0,       Str = ".int0",      Dir = "int0s"
    ; Ext = ext_cur_ngs_int_int1,       Str = ".int",       Dir = "ints"
    ; Ext = ext_cur_ngs_int_int2,       Str = ".int2",      Dir = "int2s"
    ; Ext = ext_cur_ngs_int_int3,       Str = ".int3",      Dir = "int3s"
    ; Ext = ext_cur_ngs_int_date_int0,  Str = ".date0",     Dir = "date0s"
    ; Ext = ext_cur_ngs_int_date_int12, Str = ".date",      Dir = "dates"
    ; Ext = ext_cur_ngs_int_date_int3,  Str = ".date3",     Dir = "date3s"
    ; Ext = ext_cur_ngs_mf_d,           Str = ".d",         Dir = "ds"
    % The next deviation below from the "delete initial dot, add final 's'"
    % rule is intentional, though I (zs) don't know the full reason.
    ; Ext = ext_cur_ngs_mf_dv,          Str = ".dv",        Dir = "deps"
    ; Ext = ext_cur_ngs_mf_dep,         Str = ".dep",       Dir = "deps"
    ; Ext = ext_cur_ngs_misc_module_dep,
                                        Str = ".module_dep",Dir = "module_deps"
    ; Ext = ext_cur_ngs_misc_call_graph_for_prof,
                                        Str = ".prof",      Dir = "profs"
    ).

ext_cur_gs_extension_dir(Ext, Str, LegacyDir, ProposedDir) :-
    ( Ext = ext_cur_gs_lib_init,
        Str = ".init",  LegacyDir = "inits", ProposedDir = "inits"
    ; Ext = ext_cur_gs_lib_jar,
        Str = ".jar",   LegacyDir = "lib",   ProposedDir = "jars"
    ; Ext = ext_cur_gs_lib_cil_dll,
        Str = ".dll",   LegacyDir = "lib",   ProposedDir = "dlls"
    ).

ext_cur_gas_extension_dir(Globals, Ext, Str, Dir) :-
    % Launcher scripts go in the `bin' subdirectory.
    ( Ext = ext_cur_gas_exec_noext,     Str = "",       Dir = "bin"
    ; Ext = ext_cur_gas_exec_exe,       Str = ".exe",   Dir = "bin"
    ; Ext = ext_cur_gas_exec_bat,       Str = ".bat",   Dir = "bin"
    ; Ext = ext_cur_gas_exec_exec_opt,
        globals.lookup_string_option(Globals, executable_file_extension, Str),
        Dir = "bin"
    ; Ext = ext_cur_gas_lib_dollar_efsl,
        Str = ".$(EXT_FOR_SHARED_LIB)", Dir = "lib"
%   ; Ext = ext_cur_gas_lib_lib,        Str = ".lib",   Dir = "lib"
%   ; Ext = ext_cur_gas_lib_so,         Str = ".so",    Dir = "lib"
    ; Ext = ext_cur_gas_lib_dollar_a,   Str = ".$A",    Dir = "lib"
    ; Ext = ext_cur_gas_lib_archive,    Str = ".a",     Dir = "lib"
    ; Ext = ext_cur_gas_lib_lib_opt,
        globals.lookup_string_option(Globals, library_extension, Str),
        Dir = "lib"
    ; Ext = ext_cur_gas_lib_sh_lib_opt,
        globals.lookup_string_option(Globals, shared_library_extension, Str),
        Dir = "lib"
    ).

ext_cur_ngs_gs_extension_dir(Ext, Str, Dir) :-
    ( Ext = ext_cur_ngs_gs_proposed_opt_plain,
        Str = ".opt",       Dir = "opts"
    ; Ext = ext_cur_ngs_gs_proposed_opt_trans,
        Str = ".trans_opt", Dir = "trans_opts"
    ; Ext = ext_cur_ngs_gs_opt_date_plain,
        Str = ".optdate",   Dir = "optdates"
    ; Ext = ext_cur_ngs_gs_opt_date_trans,
        Str = ".trans_opt_date", Dir = "trans_opt_dates"
    ; Ext = ext_cur_ngs_gs_target_c,            Str = ".c",         Dir = "cs"
    ; Ext = ext_cur_ngs_gs_target_cs,           Str = ".cs",        Dir = "css"
    ; Ext = ext_cur_ngs_gs_target_date_c,
        Str = ".c_date",    Dir = "c_dates"
    ; Ext = ext_cur_ngs_gs_target_date_cs,
        Str = ".cs_date",   Dir = "cs_dates"
    ; Ext = ext_cur_ngs_gs_target_date_java,
        Str = ".java_date", Dir = "java_dates"
    % The deviation from the "delete initial dot, add final 's'" rule
    % is intentional.
    ; Ext = ext_cur_ngs_gs_init_c,              Str = "_init.c",    Dir = "cs"

    ; Ext = ext_cur_ngs_gs_an_analysis_date,
        Str = ".analysis_date",   Dir = "analysis_dates"
    ; Ext = ext_cur_ngs_gs_an_analysis_status,
        Str = ".analysis_status", Dir = "analysis_statuss"
    ; Ext = ext_cur_ngs_gs_misc_err_date,
        Str = ".err_date",  Dir = "err_dates"
    ; Ext = ext_cur_ngs_gs_misc_used,
        Str = ".used",        Dir = "useds"
    ; Ext = ext_cur_ngs_gs_misc_track_flags,
        Str = ".track_flags", Dir = "track_flags"
    ).

:- pred ext_cur_ngs_gas_extension_dir(globals::in, ext_cur_ngs_gas::in,
    string::out, string::out) is det.

ext_cur_ngs_gas_extension_dir(Globals, Ext, Str, Dir) :-
    % The following deviations from the "delete initial dot, add final 's'"
    % rule are intentional.
    ( Ext = ext_cur_ngs_gas_obj_dollar_o,       Str = ".$O"
    ; Ext = ext_cur_ngs_gas_obj_dollar_efpo,    Str = ".$(EXT_FOR_PIC_OBJECTS)"
    ; Ext = ext_cur_ngs_gas_obj_o,              Str = ".o"
    ; Ext = ext_cur_ngs_gas_obj_pic_o,          Str = ".pic_o"
    ; Ext = ext_cur_ngs_gas_obj_obj_opt,
        globals.lookup_string_option(Globals, object_file_extension, Str)
    ; Ext = ext_cur_ngs_gas_obj_pic_obj_opt,
        globals.lookup_string_option(Globals, pic_object_file_extension, Str)
    ; Ext = ext_cur_ngs_gas_init_obj_dollar_o,  Str = "_init.$O"
    ; Ext = ext_cur_ngs_gas_init_obj_o,         Str = "_init.o"
    ; Ext = ext_cur_ngs_gas_init_obj_pic_o,     Str = "_init.pic_o"
    ; Ext = ext_cur_ngs_gas_init_obj_obj_opt,
        globals.lookup_string_option(Globals, object_file_extension, Str0),
        Str = "_init" ++ Str0
    ; Ext = ext_cur_ngs_gas_init_obj_pic_obj_opt,
        globals.lookup_string_option(Globals, pic_object_file_extension, Str0),
        Str = "_init" ++ Str0
    ),
    Dir = "os".

:- pred ext_cur_ngs_gs_err_extension_dir(ext_cur_ngs_gs_err::in,
    string::out, string::out) is det.

ext_cur_ngs_gs_err_extension_dir(Ext, Str, Dir) :-
    Ext = ext_cur_ngs_gs_err_err, Str = ".err",  Dir = "errs".

:- pred ext_cur_ngs_gs_java_extension_dir(ext_cur_ngs_gs_java::in,
    string::out, string::out) is det.

ext_cur_ngs_gs_java_extension_dir(Ext, Str, Dir) :-
    ( Ext = ext_cur_ngs_gs_java_java,   Str = ".java",  Dir = "javas"
    ; Ext = ext_cur_ngs_gs_java_class,  Str = ".class", Dir = "classes"
    ).

ext_cur_pgs_max_cur_extension_dir(Ext, Str, Dir) :-
    Ext = ext_cur_pgs_max_cur_mh, Str = ".mh", Dir = "mhs".

ext_cur_ngs_gs_max_cur_extension_dir(Ext, Str, Dir) :-
    Ext = ext_cur_ngs_gs_max_cur_mih, Str = ".mih", Dir = "mihs".

ext_cur_ngs_gs_max_ngs_extension_dir(Ext, Str, Dir) :-
    ( Ext = ext_cur_ngs_gs_max_ngs_legacy_opt_plain,
        Str = ".opt",       Dir = "opts"
    ; Ext = ext_cur_ngs_gs_max_ngs_legacy_opt_trans,
        Str = ".trans_opt", Dir = "trans_opts"
    ; Ext = ext_cur_ngs_gs_max_ngs_an_analysis,
        Str = ".analysis",  Dir = "analyses"
    ; Ext = ext_cur_ngs_gs_max_ngs_an_imdg,
        Str = ".imdg",      Dir = "imdgs"
    ; Ext = ext_cur_ngs_gs_max_ngs_an_request,
        Str = ".request",   Dir = "requests"
    ).

%---------------------------------------------------------------------------%

:- pragma inline(func(module_name_to_base_file_name_no_ext/2)).

module_name_to_base_file_name_no_ext(Ext, ModuleName) = BaseNameNoExt :-
    (
        ( Ext = ext_cur(_)
        ; Ext = ext_cur_ngs(_)
        ; Ext = ext_cur_gs(_)
        ; Ext = ext_cur_gas(_)
        ; Ext = ext_cur_ngs_gs(_)
        ; Ext = ext_cur_ngs_gas(_)
        ; Ext = ext_cur_ngs_gs_err(_)
        ; Ext = ext_cur_pgs_max_cur(_)
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
        ModuleName, DirNamesLegacy, DirNamesProposed,
        FullFileNameLegacy, FullFileNameProposed) :-
    module_name_to_file_name_ext(Globals, From, not_for_search, no,
        Ext, ModuleName, DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

module_name_to_file_name(Globals, From, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, ModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

module_name_to_file_name_curdir(Globals, From, Ext,
        ModuleName, CurDirFileName) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, ModuleName,
        _DirNamesLegacy, _DirNamesProposed, CurDirFileName).

module_name_to_file_name_full_curdir(Globals, From, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed,
        CurDirFileName) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, ModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

module_name_to_file_name_create_dirs(Globals, From, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed, !IO) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, ModuleName, DirNamesLegacy, DirNamesProposed,
        CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName),
    create_any_dirs_on_path(DirNamesLegacy, !IO).
    % XXX LEGACY create_any_dirs_on_path(DirNamesProposed, !IO).

module_name_to_file_name_full_curdir_create_dirs(Globals, From, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed,
        CurDirFileName, !IO) :-
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, ModuleName, DirNamesLegacy, DirNamesProposed,
        CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName),
    create_any_dirs_on_path(DirNamesLegacy, !IO).
    % XXX LEGACY create_any_dirs_on_path(DirNamesProposed, !IO).

%---------------------%

module_name_to_search_file_name(Globals, From, Ext, ModuleName,
        SearchWhichDirs, SearchAuthDirs,
        FullFileNameLegacy, FullFileNameProposed) :-
    module_name_to_file_name_ext(Globals, From, for_search,
        yes(do_not_create_dirs), Ext, ModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName),

    % This nested switch on SearchWhichDirs and Ext intentionally covers
    % *only* the combinations of their values that we want to allow,
    % as expressed by the several modes of this predicate.
    (
        SearchWhichDirs = search_cur_dir,
        (
            Ext = ext_cur_ngs(ExtCurNgs),
            ( ExtCurNgs = ext_cur_ngs_int_int0
            ; ExtCurNgs = ext_cur_ngs_int_int1
            ; ExtCurNgs = ext_cur_ngs_int_int2
            ; ExtCurNgs = ext_cur_ngs_int_int3
            ; ExtCurNgs = ext_cur_ngs_misc_module_dep
            ),
            SearchAuthDirs = search_auth_cur_dir
        ;
            Ext = ext_cur_ngs_gs_max_ngs(ExtCurNgsGsMaxCur),
            ( ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_legacy_opt_plain
            ; ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_legacy_opt_trans
            ),
            SearchAuthDirs = search_auth_cur_dir
        ;
            Ext = ext_cur_ngs_gs(ExtCurNgsGs),
            ( ExtCurNgsGs = ext_cur_ngs_gs_proposed_opt_plain
            ; ExtCurNgsGs = ext_cur_ngs_gs_proposed_opt_trans
            ),
            SearchAuthDirs = search_auth_cur_dir
        )
    ;
        SearchWhichDirs = search_normal_dirs,
        (
            Ext = ext_cur_ngs(ExtCurNgs),
            ext_cur_ngs_to_normal_ext(ExtCurNgs, NormalExt),
            SearchAuthDirs = search_auth_private(
                private_auth_normal_dirs(NormalExt, Globals))
        )
    ;
        SearchWhichDirs = search_intermod_dirs,
        (
            Ext = ext_cur_ngs_gs_max_ngs(ExtCurNgsGsMaxCur),
            (
                ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_legacy_opt_plain,
                IntermodExt = ie_opt_plain
            ;
                ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_legacy_opt_trans,
                IntermodExt = ie_opt_trans
            ;
                ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_an_request,
                IntermodExt = ie_an_request
            ;
                ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_an_imdg,
                IntermodExt = ie_an_imdg
            ;
                ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_ngs_an_analysis,
                IntermodExt = ie_an_analysis
            )
        ;
            Ext = ext_cur_ngs_gs(ExtCurNgsGs),
            (
                ExtCurNgsGs = ext_cur_ngs_gs_proposed_opt_plain,
                IntermodExt = ie_opt_plain
            ;
                ExtCurNgsGs = ext_cur_ngs_gs_proposed_opt_trans,
                IntermodExt = ie_opt_trans
            ;
                ExtCurNgsGs = ext_cur_ngs_gs_an_analysis_status,
                IntermodExt = ie_an_analysis_status
            ;
                ExtCurNgsGs = ext_cur_ngs_gs_an_analysis_date,
                IntermodExt = ie_an_analysis_date
            )
        ),
        SearchAuthDirs = search_auth_private(
            private_auth_intermod_dirs(IntermodExt, Globals))
    ;
        SearchWhichDirs = search_dirs_for_ext,
        (
            Ext = ext_cur(ExtCur),
            ExtCur = ext_cur_user_xml,
            SearchAuthDirs = search_auth_cur_dir
        ;
            Ext = ext_cur_ngs(ExtCurNgs),
            ext_cur_ngs_to_normal_ext(ExtCurNgs, NormalExt),
            SearchAuthDirs = search_auth_private(
                private_auth_normal_dirs(NormalExt, Globals))
        ;
            Ext = ext_cur_ngs_gs(ExtCurNgsGs),
            ( ExtCurNgsGs = ext_cur_ngs_gs_target_c
            ; ExtCurNgsGs = ext_cur_ngs_gs_target_cs
            ; ExtCurNgsGs = ext_cur_ngs_gs_misc_track_flags
            ),
            SearchAuthDirs = search_auth_cur_dir
        ;
            Ext = ext_cur_ngs_gs_err(ExtCurNgsGsErr),
            ExtCurNgsGsErr = ext_cur_ngs_gs_err_err,
            SearchAuthDirs = search_auth_cur_dir
        ;
            Ext = ext_cur_ngs_gs_java(ExtCurNgsGsJava),
            ( ExtCurNgsGsJava = ext_cur_ngs_gs_java_java
            ; ExtCurNgsGsJava = ext_cur_ngs_gs_java_class
            ),
            SearchAuthDirs = search_auth_cur_dir
        ;
            Ext = ext_cur_ngs_gs_max_ngs(ExtCurNgsGsMaxNgs),
            (
                ExtCurNgsGsMaxNgs = ext_cur_ngs_gs_max_ngs_legacy_opt_plain,
                IntermodExt = ie_opt_plain
            ;
                ExtCurNgsGsMaxNgs = ext_cur_ngs_gs_max_ngs_an_analysis,
                IntermodExt = ie_an_analysis
            ),
            PrivateDirs = private_auth_intermod_dirs(IntermodExt, Globals),
            SearchAuthDirs =
                search_auth_cur_dir_and(search_auth_private(PrivateDirs))
        ;
            Ext = ext_cur_ngs_gs_max_cur(ExtCurNgsGsMaxCur),
            ExtCurNgsGsMaxCur = ext_cur_ngs_gs_max_cur_mih,
            CInclDirs = private_auth_c_include_dirs(cie_mih, Globals),
            SearchAuthDirs =
                search_auth_cur_dir_and(search_auth_private(CInclDirs))
        ;
            Ext = ext_cur_pgs_max_cur(ExtCurPgsMaxCur),
            ExtCurPgsMaxCur = ext_cur_pgs_max_cur_mh,
            CInclDirs = private_auth_c_include_dirs(cie_mh, Globals),
            SearchAuthDirs =
                search_auth_cur_dir_and(search_auth_private(CInclDirs))
        ;
            Ext = ext_cur_ngs_gas(ExtCurNgsGas),
            ( ExtCurNgsGas = ext_cur_ngs_gas_obj_obj_opt
            ; ExtCurNgsGas = ext_cur_ngs_gas_obj_pic_obj_opt
            ),
            SearchAuthDirs = search_auth_cur_dir
        )
    ).

:- inst ext_cur_ngs_search for ext_cur_ngs/0
    --->    ext_cur_ngs_int_int0
    ;       ext_cur_ngs_int_int1
    ;       ext_cur_ngs_int_int2
    ;       ext_cur_ngs_int_int3
    ;       ext_cur_ngs_misc_module_dep.

:- pred ext_cur_ngs_to_normal_ext(ext_cur_ngs::in(ext_cur_ngs_search),
    normal_ext::out) is det.

ext_cur_ngs_to_normal_ext(ExtCurNgs, NormalExt) :-
    ( ExtCurNgs = ext_cur_ngs_int_int0,         NormalExt = ne_int0
    ; ExtCurNgs = ext_cur_ngs_int_int1,         NormalExt = ne_int1
    ; ExtCurNgs = ext_cur_ngs_int_int2,         NormalExt = ne_int2
    ; ExtCurNgs = ext_cur_ngs_int_int3,         NormalExt = ne_int3
    ; ExtCurNgs = ext_cur_ngs_misc_module_dep,  NormalExt = ne_module_dep
    ).

%---------------------%

module_name_to_lib_file_name_return_dirs(Globals, From, Prefix, Ext,
        ModuleName, DirNamesLegacy, DirNamesProposed,
        FullFileNameLegacy, FullFileNameProposed) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        no, Ext, FakeModuleName, DirNamesLegacy, DirNamesProposed,
        CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

module_name_to_lib_file_name(Globals, From, Prefix, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, FakeModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

module_name_to_lib_file_name_full_curdir(Globals, From, Prefix, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed,
        CurDirFileName) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_not_create_dirs), Ext, FakeModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

module_name_to_lib_file_name_create_dirs(Globals, From, Prefix, Ext,
        ModuleName, FullFileNameLegacy, FullFileNameProposed, !IO) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, FakeModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName),
    create_any_dirs_on_path(DirNamesLegacy, !IO).
    % XXX LEGACY create_any_dirs_on_path(DirNamesProposed, !IO).

module_name_to_lib_file_name_full_curdir_create_dirs(Globals, From, Prefix,
        Ext, ModuleName, FullFileNameLegacy, FullFileNameProposed,
        CurDirFileName, !IO) :-
    FakeModuleName = make_fake_module_name(Prefix, ModuleName),
    module_name_to_file_name_ext(Globals, From, not_for_search,
        yes(do_create_dirs), Ext, FakeModuleName,
        DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName),
    create_any_dirs_on_path(DirNamesLegacy, !IO).
    % XXX LEGACY create_any_dirs_on_path(DirNamesProposed, !IO).

:- func make_fake_module_name(string, module_name) = module_name.

make_fake_module_name(Prefix, ModuleName) = FakeModuleName :-
    BaseFileName = sym_name_to_string(ModuleName),
    BaseNameNoExt = Prefix ++ BaseFileName,
    FakeModuleName = unqualified(BaseNameNoExt).

%---------------------%

module_name_to_cur_dir_file_name(ExtCur, ModuleName, CurDirFileName) :-
    BaseNameNoExt = module_name_to_base_file_name_no_ext_non_java(ModuleName),
    ext_cur_extension(ExtCur, ExtStr),
    CurDirFileName = BaseNameNoExt ++ ExtStr.

%---------------------%

module_name_to_target_file_name_create_dirs(Globals, ModuleName,
        TargetFileName, !IO) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        TargetExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_c)
    ;
        CompilationTarget = target_csharp,
        TargetExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_cs)
    ;
        CompilationTarget = target_java,
        TargetExt = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java)
    ),
    % XXX Should we check the generated header files?
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred, TargetExt,
        ModuleName, TargetFileName, _TargetFileNameProposed, !IO).

module_name_to_target_timestamp_file_name_create_dirs(Globals, ModuleName,
        TimestampFileName, !IO) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        TimestampExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_c)
    ;
        CompilationTarget = target_csharp,
        TimestampExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_cs)
    ;
        CompilationTarget = target_java,
        TimestampExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_java)
    ),
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred, TimestampExt,
            ModuleName, TimestampFileName, _TimestampFileNameProposed, !IO).

%---------------------%

fact_table_file_name(Globals, From, Ext,
        FactTableFileName, FullFileNameLegacy, FullFileNameProposed) :-
    fact_table_file_name_return_dirs(Globals, From, Ext,
        FactTableFileName, _DirNamesLegacy, _DirNamesProposed,
        FullFileNameLegacy, FullFileNameProposed).

fact_table_file_name_return_dirs(Globals, From, Ext,
        FactTableFileName, DirNamesLegacy, DirNamesProposed,
        FullFileNameLegacy, FullFileNameProposed) :-
    FakeModuleName = unqualified(FactTableFileName),
    module_name_to_file_name_ext(Globals, From, not_for_search, no, Ext,
        FakeModuleName, DirNamesLegacy, DirNamesProposed, CurDirFileName),
    FullFileNameLegacy =
        glue_dir_names_base_name(DirNamesLegacy, CurDirFileName),
    FullFileNameProposed =
        glue_dir_names_base_name(DirNamesProposed, CurDirFileName).

%---------------------------------------------------------------------------%

:- pred module_name_to_file_name_ext(globals::in, string::in,
    maybe_for_search::in, maybe(maybe_create_dirs)::in, ext::in,
    module_name::in, list(dir_name)::out, list(dir_name)::out,
    file_name::out) is det.

module_name_to_file_name_ext(Globals, From, Search, StatOnlyMkdir, Ext,
        ModuleName, DirNamesLegacy, DirNamesProposed, CurDirFileName) :-
    ext_to_dir_path(Globals, Search, Ext, DirNamesLegacy, DirNamesProposed),
    BaseNameNoExt = module_name_to_base_file_name_no_ext(Ext, ModuleName),
    ExtStr = extension_to_string(Globals, Ext),
    CurDirFileName = BaseNameNoExt ++ ExtStr,
    trace [compile_time(flag("file_name_translations")),
        runtime(env("FILE_NAME_TRANSLATIONS")), io(!TIO)]
    (
        FullFileName =
            glue_dir_names_base_name(DirNamesProposed, CurDirFileName),
        record_translation(From, Search, StatOnlyMkdir,
            Ext, ModuleName, FullFileName, !TIO)
    ).

:- pragma inline(pred(ext_to_dir_path/5)).

ext_to_dir_path(Globals, Search, Ext, DirNamesLegacy, DirNamesProposed) :-
    (
        Ext = ext_cur(_ExtCur),
        % Output files intended for use by the user, and phony Mmake target
        % names go in the current directory, and so do .mh files,
        DirNamesLegacy = [],
        DirNamesProposed = []
    ;
        Ext = ext_cur_ngs(ExtCurNgs),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            ( SubdirSetting = use_cur_ngs_subdir
            ; SubdirSetting = use_cur_ngs_gs_subdir
            ),
            ext_cur_ngs_extension_dir(ExtCurNgs, _ExtStr, SubDirName),
            make_ngs_dir_names(SubDirName, DirNamesLegacy, DirNamesProposed)
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
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_gs_extension_dir(ExtCurGs, _ExtStr,
                LegacySubDirName, ProposedSubDirName),
            make_gs_dir_names(Globals, LegacySubDirName,
                DirNamesLegacy, _DirNamesProposed),
            make_gs_dir_names(Globals, ProposedSubDirName,
                _DirNamesLegacy, DirNamesProposed)
        )
    ;
        Ext = ext_cur_gas(ExtCurGas),
        % Executables and library files go in the current directory
        % only with --no-use-grade-subdirs; with --use-grade-subdirs,
        % they go in a grade subdir.
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            ( SubdirSetting = use_cur_dir
            ; SubdirSetting = use_cur_ngs_subdir
            ),
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_gas_extension_dir(Globals, ExtCurGas, _ExtStr, SubDirName),
            make_gas_dir_names(Globals, SubDirName,
                DirNamesLegacy, DirNamesProposed)
        )
    ;
        Ext = ext_cur_ngs_gs(ExtCurNgsGs),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            SubdirSetting = use_cur_ngs_subdir,
            ext_cur_ngs_gs_extension_dir(ExtCurNgsGs, _ExtStr, SubDirName),
            make_ngs_dir_names(SubDirName, DirNamesLegacy, DirNamesProposed)
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_ngs_gs_extension_dir(ExtCurNgsGs, _ExtStr, SubDirName),
            make_gs_dir_names(Globals, SubDirName,
                DirNamesLegacy, DirNamesProposed)
        )
    ;
        Ext = ext_cur_ngs_gas(ExtCurNgsGas),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            SubdirSetting = use_cur_ngs_subdir,
            ext_cur_ngs_gas_extension_dir(Globals, ExtCurNgsGas,
                _ExtStr, SubDirName),
            make_ngs_dir_names(SubDirName, DirNamesLegacy, DirNamesProposed)
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_ngs_gas_extension_dir(Globals, ExtCurNgsGas,
                _ExtStr, SubDirName),
            make_gas_dir_names(Globals, SubDirName,
                DirNamesLegacy, DirNamesProposed)
        )
    ;
        Ext = ext_cur_ngs_gs_err(ExtCurNgsGsErr),
        globals.lookup_bool_option(Globals, error_files_in_subdir,
            ErrorFilesInSubdir),
        (
            ErrorFilesInSubdir = no,
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            ErrorFilesInSubdir = yes,
            globals.get_subdir_setting(Globals, SubdirSetting),
            (
                SubdirSetting = use_cur_dir,
                DirNamesLegacy = [],
                DirNamesProposed = []
            ;
                SubdirSetting = use_cur_ngs_subdir,
                ext_cur_ngs_gs_err_extension_dir(ExtCurNgsGsErr,
                    _ExtStr, SubDirName),
                make_ngs_dir_names(SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            ;
                SubdirSetting = use_cur_ngs_gs_subdir,
                ext_cur_ngs_gs_err_extension_dir(ExtCurNgsGsErr,
                    _ExtStr, SubDirName),
                make_gs_dir_names(Globals, SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            )
        )
    ;
        Ext = ext_cur_ngs_gs_java(ExtCurNgsGsJava),
        % The Java code generator starts every .java file it creates with
        % "package jmercury;". When we look for .java (or .class) files
        % of Mercury modules, we have to specify this module component;
        % when creating directories, we don't (since the Java compiler
        % will do that). Code that needs directory paths that include
        % "jmercury" should call our ancestors; code that needs directory
        % paths that do not include it should call get_java_dir_path.
        get_java_dir_path(Globals, ExtCurNgsGsJava,
            DirNamesLegacy0, DirNamesProposed0),
        DirNamesLegacy = DirNamesLegacy0 ++ ["jmercury"],
        DirNamesProposed = DirNamesProposed0 ++ ["jmercury"]
    ;
        Ext = ext_cur_pgs_max_cur(ExtCurPgsMaxCur),
        (
            Search = for_search,
            % If we are searching for (rather than writing) a `.mh' file,
            % use the plain file name. This is so that searches for files
            % in installed libraries will work. `--c-include-directory' is set
            % so that searches for files in the current directory will work.
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            Search = not_for_search,
            globals.get_subdir_setting(Globals, SubdirSetting),
            (
                SubdirSetting = use_cur_dir,
                DirNamesLegacy = [],
                DirNamesProposed = []
            ;
                ( SubdirSetting = use_cur_ngs_subdir
                ; SubdirSetting = use_cur_ngs_gs_subdir
                ),
                ext_cur_pgs_max_cur_extension_dir(ExtCurPgsMaxCur,
                    _ExtStr, SubDirName),
                make_ngs_dir_names(SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            )
        )
    ;
        Ext = ext_cur_ngs_gs_max_cur(ExtCurNgsGsMaxCur),
        (
            Search = for_search,
            % If we are searching for (rather than writing) a `.mih' file,
            % use the plain file name. This is so that searches for files
            % in installed libraries will work. `--c-include-directory' is set
            % so that searches for files in the current directory will work.
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            Search = not_for_search,
            globals.get_subdir_setting(Globals, SubdirSetting),
            (
                SubdirSetting = use_cur_dir,
                DirNamesLegacy = [],
                DirNamesProposed = []
            ;
                SubdirSetting = use_cur_ngs_subdir,
                ext_cur_ngs_gs_max_cur_extension_dir(ExtCurNgsGsMaxCur,
                    _ExtStr, SubDirName),
                make_ngs_dir_names(SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            ;
                SubdirSetting = use_cur_ngs_gs_subdir,
                ext_cur_ngs_gs_max_cur_extension_dir(ExtCurNgsGsMaxCur,
                    _ExtStr, SubDirName),
                make_gs_dir_names(Globals, SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            )
        )
    ;
        Ext = ext_cur_ngs_gs_max_ngs(ExtCurNgsGsMaxNgs),
        globals.get_subdir_setting(Globals, SubdirSetting),
        (
            SubdirSetting = use_cur_dir,
            DirNamesLegacy = [],
            DirNamesProposed = []
        ;
            SubdirSetting = use_cur_ngs_subdir,
            ext_cur_ngs_gs_max_ngs_extension_dir(ExtCurNgsGsMaxNgs,
                _ExtStr, SubDirName),
            make_ngs_dir_names(SubDirName, DirNamesLegacy, DirNamesProposed)
        ;
            SubdirSetting = use_cur_ngs_gs_subdir,
            ext_cur_ngs_gs_max_ngs_extension_dir(ExtCurNgsGsMaxNgs,
                _ExtStr, SubDirName),
            (
                Search = for_search,
                make_ngs_dir_names(SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            ;
                Search = not_for_search,
                make_gs_dir_names(Globals, SubDirName,
                    DirNamesLegacy, DirNamesProposed)
            )
        )
    ).

%---------------------------------------------------------------------------%

analysis_cache_dir_name(Globals, DirNameLegacy, DirNameProposed) :-
    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        ( SubdirSetting = use_cur_dir
        ; SubdirSetting = use_cur_ngs_subdir
        ),
        make_ngs_dir_names("analysis_cache",
            DirComponentsLegacy, DirComponentsProposed)
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        make_gs_dir_names(Globals, "analysis_cache",
            DirComponentsLegacy, DirComponentsProposed)
    ),
    DirNameLegacy =
        dir.relative_path_name_from_components(DirComponentsLegacy),
    DirNameProposed =
        dir.relative_path_name_from_components(DirComponentsProposed).

%---------------------------------------------------------------------------%
%
% Return the directory path of the non-grade-specific and the
% grade-specific directory whose last component is the given string.
%

:- pred make_ngs_dir_names(dir_name::in,
    list(dir_name)::out, list(dir_name)::out) is det.

make_ngs_dir_names(SubDirName, NgsSubDirNamesLegacy, NgsSubDirNamesProposed) :-
    NgsSubDirNamesLegacy = ["Mercury", SubDirName],
    NgsSubDirNamesProposed = ["MercurySystem", SubDirName].

:- pred make_gs_dir_names(globals::in, dir_name::in,
    list(dir_name)::out, list(dir_name)::out) is det.
:- pragma inline(pred(make_gs_dir_names/4)).

make_gs_dir_names(Globals, SubDirName,
        GsSubDirNamesLegacy, GsSubDirNamesProposed) :-
    globals.get_grade_dir(Globals, Grade),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    % The extra "Mercury" is needed so we can use
    % `--intermod-directory Mercury/<grade>/<target_arch>' and
    % `--c-include Mercury/<grade>/<target_arch>'
    % to find the local `.opt' and `.mih' files without messing up
    % the search for the files for installed libraries.
    % XXX This seems ... suboptimal to me (zs).
    GsSubDirNamesLegacy =
        ["Mercury", Grade, TargetArch, "Mercury", SubDirName],
    GsSubDirNamesProposed =
        ["MercurySystem", SubDirName, Grade].

:- pred make_gas_dir_names(globals::in, dir_name::in,
    list(dir_name)::out, list(dir_name)::out) is det.

make_gas_dir_names(Globals, SubDirName,
        GasSubDirNamesLegacy, GasSubDirNamesProposed) :-
    globals.get_grade_dir(Globals, Grade),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    GasSubDirNamesLegacy =
        ["Mercury", Grade, TargetArch, "Mercury", SubDirName],
    GasSubDirNamesProposed =
        ["MercurySystem", SubDirName, Grade, TargetArch].

%---------------------%

make_selected_proposed_dir_name_gas(SubdirSetting, Grade, TargetArch,
        ExtSubDir, PrefixDir, Dir) :-
    (
        SubdirSetting = use_cur_dir,
        Dir = PrefixDir
    ;
        SubdirSetting = use_cur_ngs_subdir,
        Dir = PrefixDir / "MercurySystem" / ExtSubDir
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        Dir = PrefixDir / "MercurySystem" / ExtSubDir / Grade / TargetArch
    ).

make_all_proposed_dir_names_gas(Grade, TargetArch, ExtSubDir,
        PrefixDir, Dirs) :-
    make_selected_proposed_dir_name_gas(use_cur_ngs_gs_subdir, Grade,
        TargetArch, ExtSubDir, PrefixDir, GsDir),
    make_selected_proposed_dir_name_gas(use_cur_ngs_subdir, Grade,
        TargetArch, ExtSubDir, PrefixDir, NgsDir),
    make_selected_proposed_dir_name_gas(use_cur_dir, Grade,
        TargetArch, ExtSubDir, PrefixDir, CurDir),
    Dirs = [GsDir, NgsDir, CurDir].

%---------------------------------------------------------------------------%

make_selected_proposed_dir_name_gs(SubdirSetting, Grade, ExtSubDir, PrefixDir,
        Dir) :-
    (
        SubdirSetting = use_cur_dir,
        Dir = PrefixDir
    ;
        SubdirSetting = use_cur_ngs_subdir,
        Dir = PrefixDir / "MercurySystem" / ExtSubDir
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        Dir = PrefixDir / "MercurySystem" / ExtSubDir / Grade
    ).

make_all_proposed_dir_names_gs(Grade, ExtSubDir, PrefixDir, Dirs) :-
    make_selected_proposed_dir_name_gs(use_cur_ngs_gs_subdir, Grade, ExtSubDir,
        PrefixDir, GsDir),
    make_selected_proposed_dir_name_gs(use_cur_ngs_subdir, Grade, ExtSubDir,
        PrefixDir, NgsDir),
    make_selected_proposed_dir_name_gs(use_cur_dir, Grade, ExtSubDir,
        PrefixDir, CurDir),
    Dirs = [GsDir, NgsDir, CurDir].

%---------------------------------------------------------------------------%

make_selected_proposed_dir_name_ngs(SubdirSetting, ExtSubDir, PrefixDir,
        Dir) :-
    (
        SubdirSetting = use_cur_dir,
        Dir = PrefixDir
    ;
        ( SubdirSetting = use_cur_ngs_subdir
        ; SubdirSetting = use_cur_ngs_gs_subdir
        ),
        Dir = PrefixDir / "MercurySystem" / ExtSubDir
    ).

make_all_proposed_dir_names_ngs(ExtSubDir, PrefixDir, Dirs) :-
    make_selected_proposed_dir_name_ngs(use_cur_ngs_subdir, ExtSubDir,
        PrefixDir, NgsDir),
    make_selected_proposed_dir_name_ngs(use_cur_dir, ExtSubDir,
        PrefixDir, CurDir),
    Dirs = [NgsDir, CurDir].

%---------------------------------------------------------------------------%

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

get_java_dir_path(Globals, ExtCurNgsGsJava,
        DirNamesLegacy, DirNamesProposed) :-
    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        SubdirSetting = use_cur_dir,
        DirNamesLegacy = [],
        DirNamesProposed = []
    ;
        SubdirSetting = use_cur_ngs_subdir,
        ext_cur_ngs_gs_java_extension_dir(ExtCurNgsGsJava,
            _ExtStr, SubDirName),
        make_ngs_dir_names(SubDirName, DirNamesLegacy, DirNamesProposed)
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        ext_cur_ngs_gs_java_extension_dir(ExtCurNgsGsJava,
            _ExtStr, SubDirName),
        make_gs_dir_names(Globals, SubDirName,
            DirNamesLegacy, DirNamesProposed)
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
