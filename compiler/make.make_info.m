%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the main data structure used by mmc --make,
% as well as a few utility types.
%
%---------------------------------------------------------------------------%

:- module make.make_info.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.dependencies.
:- import_module make.deps_set.
:- import_module make.options_file.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.read_modules.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

:- type make_info
    --->    make_info(
                % For modules whose sources we have read in, the
                % maybe_module_dep_info will contain module_and_imports.
                % For modules for which we have read only the .dep file,
                % maybe_module_dep_info will contain a module_dep_summary.
                %
                % An old comment about the former says:
                %
                % "The items field of each module_and_imports structure should
                % be empty -- we are not trying to cache the items here",
                % but I (zs) don't whether that is actually true.
                mki_module_dependencies :: map(module_name,
                                            maybe_module_dep_info),

                mki_file_timestamps     :: file_timestamps,

                % Cache chosen file names for a module name and extension.
                mki_search_file_name_cache :: map(module_name_ext, file_name),

                % Any flags required to set detected library grades.
                mki_detected_grade_flags :: list(string),

                % The original set of options passed to mmc, not including
                % the targets to be made.
                mki_option_args         :: list(string),

                % The contents of the Mercury.options file.
                mki_options_variables   :: options_variables,

                % The mapping between module_names and indices.
                mki_module_index_map    :: module_index_map,

                % The mapping between dependency_files and indices.
                mki_dep_file_index_map  :: dependency_file_index_map,

                mki_dependency_status   :: version_hash_table(dependency_file,
                                            dependency_status),

                % For each module, the set of modules for which the `.int'
                % files are read, excluding those read as a result of reading
                % `.opt' files. The bool records whether there was an error
                % in the dependencies.
                % XXX Use a better representation for the sets.
                mki_cached_direct_imports :: cached_direct_imports,

                mki_cached_non_intermod_direct_imports
                                        :: cached_direct_imports,

                % The boolean is `yes' if the result is complete.
                % XXX Use a better representation for the sets.
                mki_cached_transitive_dependencies
                                        :: cached_transitive_dependencies,

                mki_cached_foreign_imports :: cached_foreign_imports,

                % Should the `.module_dep' files be rebuilt?
                % Set to `do_not_rebuild_module_deps' for `mmc --make clean'.
                mki_rebuild_module_deps :: rebuild_module_deps,

                mki_keep_going          :: maybe_keep_going,

                % Modules for which we have redirected output
                % to a `.err' file during this invocation of mmc.
                mki_error_file_modules  :: set(module_name),

                % Used for reporting which module imported a nonexistent
                % module.
                mki_importing_module    :: maybe(module_name),

                % Targets specified on the command line.
                mki_command_line_targets
                                        :: set(pair(module_name, target_type)),

                % The remaining number of analysis passes that we will allow
                % on `suboptimal' modules. It starts at the value of
                % `--analysis-repeat' and decrements to zero as analysis passes
                % on `suboptimal' modules are performed. `invalid' modules
                % are not affected as they will always be reanalysed.
                mki_reanalysis_passes   :: int,

                % An inter-process lock to prevent multiple processes
                % interleaving their output to standard output.
                mki_maybe_stdout_lock   :: maybe(stdout_lock),

                % The parse trees of the files we have read so far,
                % so we never have to read and parse each file more than once.
                mki_mi_read_module_maps :: have_read_module_maps
            ).

%---------------------------------------------------------------------------%

:- type maybe_module_dep_info
    --->    no_module_dep_info
    ;       some_module_dep_info(module_dep_info).

:- type file_timestamps == map(string, maybe_error(timestamp)).

:- type module_name_ext
    --->    module_name_ext(module_name, ext).

:- type module_index_map
    --->    module_index_map(
                mim_forward_map         :: version_hash_table(module_name,
                                            module_index),
                mim_reverse_map         :: version_array(module_name),
                mim_counter             :: int
            ).

:- type dependency_file_index_map
    --->    dependency_file_index_map(
                dfim_forward_map        :: version_hash_table(dependency_file,
                                            dependency_file_index),
                dfim_reverse_map        :: version_array(dependency_file),
                dfim_counter            :: int
            ).

:- type dependency_status
    --->    deps_status_not_considered
    ;       deps_status_being_built
    ;       deps_status_up_to_date
    ;       deps_status_error.

:- type rebuild_module_deps
    --->    do_rebuild_module_deps
    ;       do_not_rebuild_module_deps.

:- type maybe_keep_going
    --->    do_not_keep_going
    ;       do_keep_going.

:- type target_type
    --->    module_target(module_target_type)
    ;       linked_target(linked_target_type)
    ;       misc_target(misc_target_type).

:- type module_target_type
    --->    module_target_source
    ;       module_target_errors
    ;       module_target_int0
    ;       module_target_int1
    ;       module_target_int2
    ;       module_target_int3
    ;       module_target_opt
    ;       module_target_analysis_registry
    ;       module_target_track_flags
    ;       module_target_c_header(c_header_type)
    ;       module_target_c_code
    ;       module_target_csharp_code
    ;       module_target_java_code
    ;       module_target_java_class_code
    ;       module_target_object_code(pic)
    ;       module_target_foreign_object(pic, foreign_language)
    ;       module_target_fact_table_object(pic, file_name)
    ;       module_target_xml_doc.

:- type c_header_type
    --->    header_mh    % For `:- pragma foreign_export' declarations.
    ;       header_mih.  % Declarations for hlc grades, for compiler use only.

% :- type linked_target_type is in compile_target_code.m.

:- type misc_target_type
    --->    misc_target_clean
    ;       misc_target_realclean
    ;       misc_target_build_all(module_target_type)
    ;       misc_target_build_analyses
    ;       misc_target_build_library
    ;       misc_target_install_library
    ;       misc_target_build_xml_docs.

%---------------------------------------------------------------------------%

:- type compilation_task_type
    --->    process_module(module_compilation_task_type)
            % The `pic' argument is only used for `--target c'.
    ;       target_code_to_object_code(pic)
    ;       foreign_code_to_object_code(pic, foreign_language)
    ;       fact_table_code_to_object_code(pic, file_name).

:- type module_compilation_task_type
    --->    task_errorcheck
    ;       task_make_int0
    ;       task_make_int12     % makes both .int and .int2
    ;       task_make_int3
    ;       task_make_opt
    ;       task_make_analysis_registry
    ;       task_compile_to_target_code
    ;       task_make_xml_doc.

%---------------------------------------------------------------------------%

:- type target_file
    --->    target_file(
                target_file_name    :: module_name,
                target_file_type    :: module_target_type
            ).

:- type linked_target_file
    --->    linked_target_file(
                linked_tf_name      :: module_name,
                linked_tf_type      :: linked_target_type
            ).

%---------------------------------------------------------------------------%

:- type make_error
    --->    make_error_target(target_file)
    ;       make_error_dependencies(module_name)
    ;       make_error_other(string).

%---------------------------------------------------------------------------%
:- end_module make.make_info.
%---------------------------------------------------------------------------%
