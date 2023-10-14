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

:- type module_to_module_set_cache
    == map(module_index, module_deps_result).
:- type module_to_dep_file_set_cache
    == map(module_index, dependency_file_deps_result).

:- func init_module_to_module_set_cache = module_to_module_set_cache.
:- func init_module_to_dep_file_set_cache = module_to_dep_file_set_cache.

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

:- type trans_deps_cache == map(trans_deps_key, module_deps_result).

:- func init_trans_deps_cache = trans_deps_cache.

%---------------------------------------------------------------------------%

:- pred search_direct_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_direct_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

:- pred search_non_intermod_direct_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_non_intermod_direct_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- pred search_indirect_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_indirect_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- pred search_trans_foreign_imports_cache(make_info::in,
    module_index::in, module_deps_result::out) is semidet.
:- pred add_to_trans_foreign_imports_cache(module_index::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- pred search_anc0_dir1_indir2_cache(make_info::in,
    module_index::in, dependency_file_deps_result::out) is semidet.
:- pred add_to_anc0_dir1_indir2_cache(module_index::in,
    dependency_file_deps_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- pred search_trans_deps_cache(make_info::in,
    trans_deps_key::in, module_deps_result::out) is semidet.
:- pred add_to_trans_deps_cache(trans_deps_key::in,
    module_deps_result::in, make_info::in, make_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

init_module_to_module_set_cache = map.init.

init_module_to_dep_file_set_cache = map.init.

init_trans_deps_cache = map.init.

%---------------------------------------------------------------------------%

search_direct_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_direct_imports_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_direct_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_direct_imports_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_direct_imports_cache(CacheMap, !Info).

search_non_intermod_direct_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_non_intermod_direct_imports_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_non_intermod_direct_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_non_intermod_direct_imports_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_non_intermod_direct_imports_cache(CacheMap, !Info).

%---------------------%

search_indirect_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_indirect_imports_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_indirect_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_indirect_imports_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_indirect_imports_cache(CacheMap, !Info).

%---------------------%

search_trans_foreign_imports_cache(Info, ModuleIndex, Result) :-
    CacheMap = make_info_get_trans_foreign_imports_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_trans_foreign_imports_cache(ModuleIndex, Result, !Info) :-
    CacheMap0 = make_info_get_trans_foreign_imports_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_trans_foreign_imports_cache(CacheMap, !Info).

%---------------------------------------------------------------------------%

search_anc0_dir1_indir2_cache(Info, Key, Result) :-
    CacheMap = make_info_get_anc0_dir1_indir2_cache(Info),
    map.search(CacheMap, Key, Result).

add_to_anc0_dir1_indir2_cache(Key, Result, !Info) :-
    CacheMap0 = make_info_get_anc0_dir1_indir2_cache(!.Info),
    map.det_insert(Key, Result, CacheMap0, CacheMap),
    make_info_set_anc0_dir1_indir2_cache(CacheMap, !Info).

%---------------------------------------------------------------------------%

search_trans_deps_cache(Info, DepsRoot, Result) :-
    CacheMap = make_info_get_trans_deps_cache(Info),
    map.search(CacheMap, DepsRoot, Result).

add_to_trans_deps_cache(DepsRoot, Result, !Info) :-
    CacheMap0 = make_info_get_trans_deps_cache(!.Info),
    map.det_insert(DepsRoot, Result, CacheMap0, CacheMap),
    make_info_set_trans_deps_cache(CacheMap, !Info).

%---------------------------------------------------------------------------%
:- end_module make.deps_cache.
%---------------------------------------------------------------------------%
