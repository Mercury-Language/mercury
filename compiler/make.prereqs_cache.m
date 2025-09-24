%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.prereqs_cache.m.
%
% This module defines the types of the caches used by make.prereqs.m.
% The caches themselves are stored in make_infos.
%
%---------------------------------------------------------------------------%

:- module make.prereqs_cache.
:- interface.

:- import_module libs.
:- import_module libs.maybe_util.
:- import_module make.index_set.
:- import_module make.make_info.

:- import_module io.
:- import_module map.

%---------------------------------------------------------------------------%

:- type prereqs_result(T)
    --->    prereqs_result(
                pr_success  :: maybe_succeeded,
                pr_set      :: index_set(T)
            ).

:- type module_prereqs_result == prereqs_result(module_index).
:- type target_id_prereqs_result == prereqs_result(target_id_index).

:- type module_to_module_set_cache.
:- type module_to_target_id_set_cache.

:- func init_module_to_module_set_cache = module_to_module_set_cache.
:- func init_module_to_target_id_set_cache = module_to_target_id_set_cache.

%---------------------------------------------------------------------------%

:- type trans_prereqs_key
    --->    trans_prereqs_key(
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

:- type trans_prereqs_cache == map(trans_prereqs_key, module_prereqs_result).

:- func init_trans_prereqs_cache = trans_prereqs_cache.

%---------------------------------------------------------------------------%

:- pred search_direct_imports_non_intermod_cache(make_info::in,
    module_index::in, module_prereqs_result::out) is semidet.
:- pred add_to_direct_imports_non_intermod_cache(module_index::in,
    module_prereqs_result::in, make_info::in, make_info::out) is det.

:- pred search_direct_imports_intermod_cache(make_info::in,
    module_index::in, module_prereqs_result::out) is semidet.
:- pred add_to_direct_imports_intermod_cache(module_index::in,
    module_prereqs_result::in, make_info::in, make_info::out) is det.

%---------------------%

% :- pred search_indirect_imports_non_intermod_cache(make_info::in,
%     module_index::in, module_prereqs_result::out) is semidet.
% :- pred add_to_indirect_imports_non_intermod_cache(module_index::in,
%     module_prereqs_result::in, make_info::in, make_info::out) is det.

:- pred search_indirect_imports_intermod_cache(make_info::in,
    module_index::in, module_prereqs_result::out) is semidet.
:- pred add_to_indirect_imports_intermod_cache(module_index::in,
    module_prereqs_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- pred search_foreign_imports_non_intermod_trans_cache(make_info::in,
    module_index::in, module_prereqs_result::out) is semidet.
:- pred add_to_foreign_imports_non_intermod_trans_cache(module_index::in,
    module_prereqs_result::in, make_info::in, make_info::out) is det.

%---------------------%

% :- pred search_anc0_dir1_indir2_non_intermod_cache(make_info::in,
%     module_index::in, target_id_prereqs_result::out) is semidet.
% :- pred add_to_anc0_dir1_indir2_non_intermod_cache(module_index::in,
%     target_id_prereqs_result::in, make_info::in, make_info::out) is det.

:- pred search_anc0_dir1_indir2_intermod_cache(make_info::in,
    module_index::in, target_id_prereqs_result::out) is semidet.
:- pred add_to_anc0_dir1_indir2_intermod_cache(module_index::in,
    target_id_prereqs_result::in, make_info::in, make_info::out) is det.

%---------------------%

:- pred search_trans_prereqs_cache(make_info::in,
    trans_prereqs_key::in, module_prereqs_result::out) is semidet.
:- pred add_to_trans_prereqs_cache(trans_prereqs_key::in,
    module_prereqs_result::in, make_info::in, make_info::out) is det.

%---------------------------------------------------------------------------%

:- pred record_make_prereqs_cache_stats(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type module_to_module_set_cache
    == map(module_index, module_prereqs_result).
:- type module_to_target_id_set_cache
    == map(module_index, target_id_prereqs_result).

%---------------------------------------------------------------------------%

init_module_to_module_set_cache = map.init.

init_module_to_target_id_set_cache = map.init.

init_trans_prereqs_cache = map.init.

%---------------------------------------------------------------------------%

search_direct_imports_non_intermod_cache(Info, ModuleIndex, Result) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Acc0 = Stats0 ^ direct_non_intermod_accesses,
        Stats = Stats0 ^ direct_non_intermod_accesses := Acc0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap = make_info_get_direct_imports_non_intermod_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_direct_imports_non_intermod_cache(ModuleIndex, Result, !Info) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Miss0 = Stats0 ^ direct_non_intermod_misses,
        Stats = Stats0 ^ direct_non_intermod_misses := Miss0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap0 = make_info_get_direct_imports_non_intermod_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_direct_imports_non_intermod_cache(CacheMap, !Info).

search_direct_imports_intermod_cache(Info, ModuleIndex, Result) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Acc0 = Stats0 ^ direct_intermod_accesses,
        Stats = Stats0 ^ direct_intermod_accesses := Acc0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap = make_info_get_direct_imports_intermod_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_direct_imports_intermod_cache(ModuleIndex, Result, !Info) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Miss0 = Stats0 ^ direct_intermod_misses,
        Stats = Stats0 ^ direct_intermod_misses := Miss0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap0 = make_info_get_direct_imports_intermod_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_direct_imports_intermod_cache(CacheMap, !Info).

%---------------------%

% search_indirect_imports_non_intermod_cache(Info, ModuleIndex, Result) :-
%     trace [
%         compile_time(flag("prereqs_cache_stats")),
%         run_time(env("PREREQS_CACHE_STATS")),
%         io(!TIO)
%     ] (
%         get_prereqs_cache_stats(Stats0, !TIO),
%         Acc0 = Stats0 ^ indirect_non_intermod_accesses,
%         Stats = Stats0 ^ indirect_non_intermod_accesses := Acc0 + 1u,
%         set_prereqs_cache_stats(Stats, !TIO)
%     ),
%     CacheMap = make_info_get_indirect_imports_non_intermod_cache(Info),
%     map.search(CacheMap, ModuleIndex, Result).
%
% add_to_indirect_imports_non_intermod_cache(ModuleIndex, Result, !Info) :-
%     trace [
%         compile_time(flag("prereqs_cache_stats")),
%         run_time(env("PREREQS_CACHE_STATS")),
%         io(!TIO)
%     ] (
%         get_prereqs_cache_stats(Stats0, !TIO),
%         Miss0 = Stats0 ^ indirect_non_intermod_misses,
%         Stats = Stats0 ^ indirect_non_intermod_misses := Miss0 + 1u,
%         set_prereqs_cache_stats(Stats, !TIO)
%     ),
%     CacheMap0 = make_info_get_indirect_imports_non_intermod_cache(!.Info),
%     map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
%     make_info_set_indirect_imports_non_intermod_cache(CacheMap, !Info).

search_indirect_imports_intermod_cache(Info, ModuleIndex, Result) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Acc0 = Stats0 ^ indirect_intermod_accesses,
        Stats = Stats0 ^ indirect_intermod_accesses := Acc0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap = make_info_get_indirect_imports_intermod_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_indirect_imports_intermod_cache(ModuleIndex, Result, !Info) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Miss0 = Stats0 ^ indirect_intermod_misses,
        Stats = Stats0 ^ indirect_intermod_misses := Miss0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap0 = make_info_get_indirect_imports_intermod_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_indirect_imports_intermod_cache(CacheMap, !Info).

%---------------------%

search_foreign_imports_non_intermod_trans_cache(Info, ModuleIndex, Result) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Acc0 = Stats0 ^ foreign_non_intermod_accesses,
        Stats = Stats0 ^ foreign_non_intermod_accesses := Acc0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap = make_info_get_foreign_imports_non_intermod_trans_cache(Info),
    map.search(CacheMap, ModuleIndex, Result).

add_to_foreign_imports_non_intermod_trans_cache(ModuleIndex, Result, !Info) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Miss0 = Stats0 ^ foreign_non_intermod_misses,
        Stats = Stats0 ^ foreign_non_intermod_misses := Miss0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap0 = make_info_get_foreign_imports_non_intermod_trans_cache(!.Info),
    map.det_insert(ModuleIndex, Result, CacheMap0, CacheMap),
    make_info_set_foreign_imports_non_intermod_trans_cache(CacheMap, !Info).

%---------------------------------------------------------------------------%

% search_anc0_dir1_indir2_non_intermod_cache(Info, Key, Result) :-
%     trace [
%         compile_time(flag("prereqs_cache_stats")),
%         run_time(env("PREREQS_CACHE_STATS")),
%         io(!TIO)
%     ] (
%         get_prereqs_cache_stats(Stats0, !TIO),
%         Acc0 = Stats0 ^ anc0_dir1_indir2_non_intermod_accesses,
%         Stats = Stats0 ^ anc0_dir1_indir2_non_intermod_accesses := Acc0 + 1u,
%         set_prereqs_cache_stats(Stats, !TIO)
%     ),
%     CacheMap = make_info_get_anc0_dir1_indir2_non_intermod_cache(Info),
%     map.search(CacheMap, Key, Result).
%
% add_to_anc0_dir1_indir2_non_intermod_cache(Key, Result, !Info) :-
%     trace [
%         compile_time(flag("prereqs_cache_stats")),
%         run_time(env("PREREQS_CACHE_STATS")),
%         io(!TIO)
%     ] (
%         get_prereqs_cache_stats(Stats0, !TIO),
%         Miss0 = Stats0 ^ anc0_dir1_indir2_non_intermod_misses,
%         Stats = Stats0 ^ anc0_dir1_indir2_non_intermod_misses := Miss0 + 1u,
%         set_prereqs_cache_stats(Stats, !TIO)
%     ),
%     CacheMap0 = make_info_get_anc0_dir1_indir2_non_intermod_cache(!.Info),
%     map.det_insert(Key, Result, CacheMap0, CacheMap),
%     make_info_set_anc0_dir1_indir2_non_intermod_cache(CacheMap, !Info).

search_anc0_dir1_indir2_intermod_cache(Info, Key, Result) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Acc0 = Stats0 ^ anc0_dir1_indir2_intermod_accesses,
        Stats = Stats0 ^ anc0_dir1_indir2_intermod_accesses := Acc0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap = make_info_get_anc0_dir1_indir2_intermod_cache(Info),
    map.search(CacheMap, Key, Result).

add_to_anc0_dir1_indir2_intermod_cache(Key, Result, !Info) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Miss0 = Stats0 ^ anc0_dir1_indir2_intermod_misses,
        Stats = Stats0 ^ anc0_dir1_indir2_intermod_misses := Miss0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap0 = make_info_get_anc0_dir1_indir2_intermod_cache(!.Info),
    map.det_insert(Key, Result, CacheMap0, CacheMap),
    make_info_set_anc0_dir1_indir2_intermod_cache(CacheMap, !Info).

%---------------------------------------------------------------------------%

search_trans_prereqs_cache(Info, Key, Result) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Acc0 = Stats0 ^ trans_prereqs_accesses,
        Stats = Stats0 ^ trans_prereqs_accesses := Acc0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap = make_info_get_trans_prereqs_cache(Info),
    map.search(CacheMap, Key, Result).

add_to_trans_prereqs_cache(Key, Result, !Info) :-
    trace [
        compile_time(flag("prereqs_cache_stats")),
        run_time(env("PREREQS_CACHE_STATS")),
        io(!TIO)
    ] (
        get_prereqs_cache_stats(Stats0, !TIO),
        Miss0 = Stats0 ^ trans_prereqs_misses,
        Stats = Stats0 ^ trans_prereqs_misses := Miss0 + 1u,
        set_prereqs_cache_stats(Stats, !TIO)
    ),
    CacheMap0 = make_info_get_trans_prereqs_cache(!.Info),
    map.det_insert(Key, Result, CacheMap0, CacheMap),
    make_info_set_trans_prereqs_cache(CacheMap, !Info).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% The results of a bootcheck in csharp grade on 2023 oct 16
% were as follows. The zero hit rate of the indirect_imports_non_intermod
% and anc0_dir1_indir2_non_intermod caches is why their code is commented out,
% here, in make.make_info, and in make.prereqs.m.
%
% number of lookups:           2012939
% number of hits:              1962569
% number of misses:              50370
% hit %:                         97.50
%
% ----------------------------------------------------------
%
% cache                           #exec  #lookup     #hit    #miss   hit%
%
% direct_imports_non_intermod       100   850145   838363    11782  98.61
% direct_imports_intermod           100    17300    10965     6335  63.38
% indirect_imports_non_intermod      57     4112        0     4112   0.00
% indirect_imports_intermod         100    10965     4630     6335  42.23
% anc0_dir1_indir2_non_intermod      57     4112        0     4112   0.00
% anc0_dir1_indir2_intermod          84    41386    36891     4495  89.14
% trans_prereqs                     100  1084919  1071720    13199  98.78

:- mutable(prereqs_cache_stats, prereqs_cache_stats,
    init_prereqs_cache_stats, ground, [untrailed, attach_to_io_state]).

:- type prereqs_cache_stats
    --->    prereqs_cache_stats(
                direct_non_intermod_accesses            :: uint,
                direct_non_intermod_misses              :: uint,

                direct_intermod_accesses                :: uint,
                direct_intermod_misses                  :: uint,

                indirect_non_intermod_accesses          :: uint,
                indirect_non_intermod_misses            :: uint,

                indirect_intermod_accesses              :: uint,
                indirect_intermod_misses                :: uint,

                foreign_non_intermod_accesses           :: uint,
                foreign_non_intermod_misses             :: uint,

                anc0_dir1_indir2_non_intermod_accesses  :: uint,
                anc0_dir1_indir2_non_intermod_misses    :: uint,

                anc0_dir1_indir2_intermod_accesses      :: uint,
                anc0_dir1_indir2_intermod_misses        :: uint,

                trans_prereqs_accesses                  :: uint,
                trans_prereqs_misses                    :: uint
            ).

:- func init_prereqs_cache_stats = prereqs_cache_stats.

init_prereqs_cache_stats =
    prereqs_cache_stats(0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u,
        0u, 0u, 0u, 0u).

record_make_prereqs_cache_stats(!IO) :-
    get_prereqs_cache_stats(Stats, !IO),
    Stats = prereqs_cache_stats(
        DirectNIAcc,     DirectNIMiss,
        DirectIAcc,      DirectIMiss,
        IndirectNIAcc,   IndirectNIMiss,
        IndirectIAcc,    IndirectIMiss,
        ForeignNIAcc,    ForeignNIMiss,
        Import012NIAcc,  Import012NIMiss,
        Import012IAcc,   Import012IMiss,
        TransPrereqsAcc, TransPrereqsMiss
    ),
    StatTuples = [
        {"direct_imports_non_intermod",    DirectNIAcc,     DirectNIMiss},
        {"direct_imports_intermod",        DirectIAcc,      DirectIMiss},
        {"indirect_imports_non_intermod",  IndirectNIAcc,   IndirectNIMiss},
        {"indirect_imports_intermod",      IndirectIAcc,    IndirectIMiss},
        {"foreign_imports_non_intermod",   ForeignNIAcc,    ForeignNIMiss},
        {"anc0_dir1_indir2_non_intermod",  Import012NIAcc,  Import012NIMiss},
        {"anc0_dir1_indir2_intermod",      Import012IAcc,   Import012IMiss},
        {"trans_prereqs",                  TransPrereqsAcc, TransPrereqsMiss}
    ],
    list.map(desc_cache_stat, StatTuples, DescStrs),
    string.append_list(DescStrs, DescsStr),
    ( if DescsStr = "" then
        true
    else
        io.open_append("/tmp/MAKE_PREREQS_CACHE_STATS", Result, !IO),
        (
            Result = error(_)
        ;
            Result = ok(OutStream),
            io.write_string(OutStream, DescsStr, !IO),
            io.close_output(OutStream, !IO)
        )
    ).

:- pred desc_cache_stat({string, uint, uint}::in, string::out) is det.

desc_cache_stat({Name, Acc, Miss}, Desc) :-
    ( if Acc = 0u, Miss = 0u then
        Desc = ""
    else
        string.format("%-36s %12u %12u\n", [s(Name), u(Acc), u(Miss)], Desc)
    ).

%---------------------------------------------------------------------------%
:- end_module make.prereqs_cache.
%---------------------------------------------------------------------------%
