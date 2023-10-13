%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.deps_cache.m.
%
% This module defines the types of the caches used by make.dependencies.m.
% The caches themselves are stored in make_infos.
%
%---------------------------------------------------------------------------%

:- module make.deps_cache.
:- interface.

:- import_module libs.
:- import_module libs.maybe_util.
:- import_module make.deps_set.

:- import_module map.

%---------------------------------------------------------------------------%

:- type deps_result(T)
    --->    deps_result(
                dr_success  :: maybe_succeeded,
                dr_set      :: deps_set(T)
            ).

:- type module_deps_result == deps_result(module_index).
:- type dependency_file_deps_result == deps_result(dependency_file_index).

%---------------------------------------------------------------------------%

:- type transitive_dependencies_root
    --->    transitive_dependencies_root(
                module_index,
                transitive_dependencies_type,
                process_modules_where
            ).

:- type transitive_dependencies_type
    --->    interface_imports
    ;       all_imports             % every import_module and use_module
    ;       all_dependencies.       % all_imports plus every include_module

:- type process_modules_where
    --->    process_only_modules_in_cur_dir
            % The source file for the module is in the current directory.
    ;       process_modules_anywhere.

:- type cached_transitive_dependencies ==
    map(transitive_dependencies_root, module_deps_result).

:- func init_cached_transitive_dependencies = cached_transitive_dependencies.

%---------------------------------------------------------------------------%

:- type cached_direct_imports == map(module_index, module_deps_result).

:- func init_cached_direct_imports = cached_direct_imports.

%---------------------%

:- type cached_indirect_imports == map(module_index, module_deps_result).

:- func init_cached_indirect_imports = cached_indirect_imports.

%---------------------%

:- type cached_transitive_foreign_imports
    == map(module_index, module_deps_result).

:- func init_cached_transitive_foreign_imports =
    cached_transitive_foreign_imports.

%---------------------%

:- type cached_computed_module_deps ==
    map(computed_module_deps_key, dependency_file_deps_result).

:- type computed_module_deps_key
    --->    computed_module_deps_key(
                module_index,
                computed_module_deps_label
            ).

:- type computed_module_deps_label
    --->    computed_module_deps_import_012.

:- func init_cached_computed_module_deps = cached_computed_module_deps.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

init_cached_transitive_dependencies = map.init.

init_cached_direct_imports = map.init.

init_cached_indirect_imports = map.init.

init_cached_transitive_foreign_imports = map.init.

init_cached_computed_module_deps = map.init.

%---------------------------------------------------------------------------%
:- end_module make.deps_cache.
%---------------------------------------------------------------------------%
