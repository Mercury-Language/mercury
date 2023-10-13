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
:- import_module make.make_info.

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

:- type trans_deps_key
    --->    trans_deps_key(
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
    map(trans_deps_key, module_deps_result).

:- func init_cached_transitive_dependencies = cached_transitive_dependencies.

:- pred search_transitive_deps_cache(make_info::in, trans_deps_key::in,
    module_deps_result::out) is semidet.
:- pred add_to_transitive_deps_cache(trans_deps_key::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------------------------------------------------------------%

:- type cached_direct_imports == map(module_index, module_deps_result).

:- func init_cached_direct_imports = cached_direct_imports.

:- pred search_direct_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_direct_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

:- pred search_non_intermod_direct_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_non_intermod_direct_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- type cached_indirect_imports == map(module_index, module_deps_result).

:- func init_cached_indirect_imports = cached_indirect_imports.

:- pred search_indirect_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_indirect_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- type cached_transitive_foreign_imports
    == map(module_index, module_deps_result).

:- func init_cached_transitive_foreign_imports =
    cached_transitive_foreign_imports.

:- pred search_transitive_foreign_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_transitive_foreign_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

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

:- pred search_computed_module_deps_cache(make_info::in,
    computed_module_deps_key::in, dependency_file_deps_result::out) is semidet.
:- pred add_to_computed_module_deps_cache(computed_module_deps_key::in,
    dependency_file_deps_result::in, make_info::in, make_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

init_cached_transitive_dependencies = map.init.

search_transitive_deps_cache(Info, DepsRoot, Result) :-
    CacheMap = make_info_get_cached_transitive_dependencies(Info),
    map.search(CacheMap, DepsRoot, Result).

add_to_transitive_deps_cache(DepsRoot, Result, !Info) :-
    CacheMap0 = make_info_get_cached_transitive_dependencies(!.Info),
    map.det_insert(DepsRoot, Result, CacheMap0, CacheMap),
    make_info_set_cached_transitive_dependencies(CacheMap, !Info).

%---------------------%

init_cached_direct_imports = map.init.

search_direct_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_cached_direct_imports(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_direct_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_cached_direct_imports(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_cached_direct_imports(CacheMap, !Info).

search_non_intermod_direct_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_cached_non_intermod_direct_imports(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_non_intermod_direct_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_cached_non_intermod_direct_imports(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_cached_non_intermod_direct_imports(CacheMap, !Info).

%---------------------%

init_cached_indirect_imports = map.init.

search_indirect_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_cached_indirect_imports(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_indirect_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_cached_indirect_imports(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_cached_indirect_imports(CacheMap, !Info).

%---------------------%

init_cached_transitive_foreign_imports = map.init.

search_transitive_foreign_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_cached_transitive_foreign_imports(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_transitive_foreign_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_cached_transitive_foreign_imports(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_cached_transitive_foreign_imports(CacheMap, !Info).

%---------------------%

init_cached_computed_module_deps = map.init.

search_computed_module_deps_cache(Info, Key, Result) :-
    CacheMap = make_info_get_cached_computed_module_deps(Info),
    map.search(CacheMap, Key, Result).

add_to_computed_module_deps_cache(Key, Result, !Info) :-
    CacheMap0 = make_info_get_cached_computed_module_deps(!.Info),
    map.det_insert(Key, Result, CacheMap0, CacheMap),
    make_info_set_cached_computed_module_deps(CacheMap, !Info).

%---------------------------------------------------------------------------%
:- end_module make.deps_cache.
%---------------------------------------------------------------------------%
