%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_module_file_names.m.
%
% This module converts module_name/extension pairs into filenames
% for generate_mmakefile_fragments.m.
%
% Since many filenames occur in more than one mmake entry, its main job
% is to manage the *cache* of such filenames.
%
%---------------------------------------------------------------------------%

:- module parse_tree.make_module_file_names.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.mmakefiles.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type module_file_name_cache.

:- func init_module_file_name_cache = module_file_name_cache.

%---------------------------------------------------------------------------%

:- pred convert_module_name_set_to_file_name_group(globals::in,
    string::in, ext::in,
    set(module_name)::in, list(mmake_file_name_group)::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

:- pred convert_module_name_set_to_file_names(globals::in, ext::in,
    set(module_name)::in, list(mmake_file_name)::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.
:- pred convert_module_name_list_to_file_names(globals::in, ext::in,
    list(module_name)::in, list(mmake_file_name)::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

:- pred convert_module_name_to_file_name(globals::in, string::in, ext::in,
    module_name::in, file_name::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

%---------------------------------------------------------------------------%

:- pred record_module_ext_cache_stats(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%
%
% The cache we use to reduce the allocations needed to constructed file names.
%
% We used to use a map from extensions to module names to file names,
% but we now use a map from module names to extensions to file names.
% This can be, and is, a bit faster, because it allows us to convert
% each module name to a string just once, at least when we want to
% put a non-java extension after it. (For Java extensions, we put "__"
% between module name components; for all other extensions, we use "."
% as the separator.)
%
% At the moment, all the exported predicates of this module convert
% all the module names that appear in the input arguments to strings.
% However, the callers of those predicates pass those module names
% in *many* calls to those predicates. With a suitable change to the
% interface of this module, it should be possible to eliminate even
% the step of checking the module-name-to-string part of the cache.
%

    % Conceptually, this is a map from module_names to module_name_infos,
    % but adding an initial stage that operates on the base name of each
    % module name speeds up access, because comparisons being faster on
    % base names (which are plain strings) being faster then comparisons
    % on module names gains in more time than we lose on having to compare
    % the base names again as part of the second stage.
:- type module_file_name_cache ==
    map(string, map(module_name, module_name_info)).

:- type module_name_info
    --->    module_name_info(
                base_file_name_no_ext_non_java  :: string,
                ext_map                         :: map(ext, file_name)
            ).

init_module_file_name_cache = map.init.

%---------------------------------------------------------------------------%

convert_module_name_set_to_file_name_group(Globals, GroupName, Ext,
        ModuleNameSet, Groups, !Cache) :-
    convert_module_name_set_to_file_names(Globals, Ext,
        ModuleNameSet, FileNames, !Cache),
    Groups = construct_file_name_maybe_group(GroupName, FileNames).

convert_module_name_set_to_file_names(Globals, Ext,
        ModuleNameSet, FileNames, !Cache) :-
    % Compute the module-name-independent parts of the final FileNames
    % just once.
    % XXX LEGACY
    ext_to_dir_path_extstr(Globals, not_for_search, Ext,
        ExtDirNamesLegacy, _ExtDirNamesProposed, ExtStr),
    list.map_foldl(
        convert_module_name_to_file_name_base(Ext, ExtStr, ExtDirNamesLegacy),
        set.to_sorted_list(ModuleNameSet), FileNames, !Cache).

convert_module_name_list_to_file_names(Globals, Ext,
        ModuleNames, FileNames, !Cache) :-
    % Compute the module-name-independent parts of the final FileNames
    % just once.
    % XXX LEGACY
    ext_to_dir_path_extstr(Globals, not_for_search, Ext,
        ExtDirNamesLegacy, _ExtDirNamesProposed, ExtStr),
    list.map_foldl(
        convert_module_name_to_file_name_base(Ext, ExtStr, ExtDirNamesLegacy),
        ModuleNames, FileNames, !Cache).

%---------------------------------------------------------------------------%

convert_module_name_to_file_name(Globals, _From, Ext, ModuleName, FileName,
        !Cache) :-
    % XXX LEGACY
    ext_to_dir_path_extstr(Globals, not_for_search, Ext,
        ExtDirNamesLegacy, _ExtDirNamesProposed, ExtStr),
    convert_module_name_to_file_name_base(Ext, ExtStr, ExtDirNamesLegacy,
        ModuleName, FileName, !Cache).

:- pred convert_module_name_to_file_name_base(ext::in, string::in,
    list(dir_name)::in, module_name::in, file_name::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

convert_module_name_to_file_name_base(Ext, ExtStr, ExtDirNames,
        ModuleName, FileName, !Cache) :-
    ( ModuleName = unqualified(ModuleBaseName)
    ; ModuleName = qualified(_, ModuleBaseName)
    ),
    ( if map.search(!.Cache, ModuleBaseName, ModuleNameMap0Prime) then
        ModuleNameMap0 = ModuleNameMap0Prime,
        ( if map.search(ModuleNameMap0, ModuleName, ModuleNameInfo0) then
            trace [
                compile_time(flag("write_deps_file_cache")),
                run_time(env("WRITE_DEPS_FILE_CACHE")),
                io(!TIO)
            ] (
                record_module_name_cache_hit(!TIO)
            ),
            ModuleNameInfo0 =
                module_name_info(BaseFileNameNoExtNonJava, ExtMap0)
        else
            trace [
                compile_time(flag("write_deps_file_cache")),
                run_time(env("WRITE_DEPS_FILE_CACHE")),
                io(!TIO)
            ] (
                record_module_name_cache_miss(!TIO)
            ),
            BaseFileNameNoExtNonJava =
                module_name_to_base_file_name_no_ext_non_java(ModuleName),
            map.init(ExtMap0)
        )
    else
        trace [
            compile_time(flag("write_deps_file_cache")),
            run_time(env("WRITE_DEPS_FILE_CACHE")),
            io(!TIO)
        ] (
            record_module_name_cache_miss(!TIO)
        ),
        BaseFileNameNoExtNonJava =
            module_name_to_base_file_name_no_ext_non_java(ModuleName),
        map.init(ModuleNameMap0),
        map.init(ExtMap0)
    ),

    % We cache result of the translation, in order to save on
    % temporary string construction.
    % See the analysis of gathered statistics below for why we use the cache
    % for filenames with *all* extensions.
    %
    % XXX The cache would be more effective if we separately cached the results
    % of the call to module_name_to_base_file_name_no_ext_non_java
    % embedded inside the call to module_name_to_file_name, *provided*
    % that we add a version of module_name_to_file_name that can take
    % such cached results as input.
    ( if map.search(ExtMap0, Ext, CachedFileName) then
        trace [
            compile_time(flag("write_deps_file_cache")),
            run_time(env("WRITE_DEPS_FILE_CACHE")),
            io(!TIO)
        ] (
            record_ext_cache_hit(Ext, !TIO)
        ),
        FileName = CachedFileName
    else
        trace [
            compile_time(flag("write_deps_file_cache")),
            run_time(env("WRITE_DEPS_FILE_CACHE")),
            io(!TIO)
        ] (
            record_ext_cache_miss(Ext, !TIO)
        ),
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
            BaseFileNameNoExt = BaseFileNameNoExtNonJava
        ;
            Ext = ext_cur_ngs_gs_java(_),
            BaseFileNameNoExt =
                module_name_to_base_file_name_no_ext_java(ModuleName)
        ),

        % These two lines do the job of module_name_to_file_name().
        CurDirFileName = BaseFileNameNoExt ++ ExtStr,
        FileName = glue_dir_names_base_name(ExtDirNames, CurDirFileName),

        map.det_insert(Ext, FileName, ExtMap0, ExtMap),
        ModuleNameInfo = module_name_info(BaseFileNameNoExtNonJava, ExtMap),
        map.set(ModuleName, ModuleNameInfo, ModuleNameMap0, ModuleNameMap),
        map.set(ModuleBaseName, ModuleNameMap, !Cache)
    ).

%---------------------------------------------------------------------------%
%
% The code in this section is invoked only if both the compile time and
% the runtime conditions of the trace goals above are met.
% Its job is to gather statistics about
%
% - how many times we try to translate filenames with each extension, and
% - what the hit rate of the cache is for each extension.
%
% The data gathered here, written out by record_write_deps_file_cache_stats,
% can be summarized by tools/write_deps_file_stats.
% (The code of this cache was originally in write_deps_file.m.)
%
% The output of that tool from one bootcheck (which was an asm_fast.gc
% bootcheck with intermodule optimization) is as follows.
%
% number of lookups:           4334750
% number of hits:              3020344
% number of misses:            1314406
% hit %:                         69.68
%
% ----------------------------------------------------------
%
% extension                                                #exec #lookup  hit%
%
% ext_cur(ext_cur_mh)                                       5649 1716651 60.18
% ext_cur_gs(ext_cur_gs_exec_noext)                         1680    4570  4.51
% ext_cur_ngs(ext_cur_ngs_int_date_int0)                    5649   12345 17.20
% ext_cur_ngs(ext_cur_ngs_int_date_int12)                   5649   12345 17.20
% ext_cur_ngs(ext_cur_ngs_int_date_int3)                    5649    8408  0.00
% ext_cur_ngs(ext_cur_ngs_int_int0)                         5649   53435 71.04
% ext_cur_ngs(ext_cur_ngs_int_int1)                         5649  584321 87.35
% ext_cur_ngs(ext_cur_ngs_int_int2)                         5649  126253  0.00
% ext_cur_ngs(ext_cur_ngs_int_int3)                         5649  689821 72.47
% ext_cur_ngs(ext_cur_ngs_misc_module_dep)                  5649    8408  0.00
% ext_cur_ngs_gs(ext_cur_ngs_gs_obj_dollar_o)               5649    8408  0.00
% ext_cur_ngs_gs(ext_cur_ngs_gs_obj_pic_o)                  5649    8408  0.00
% ext_cur_ngs_gs(ext_cur_ngs_gs_opt_date_plain)             5649    8622  1.61
% ext_cur_ngs_gs(ext_cur_ngs_gs_opt_date_trans)             5649    8622  1.61
% ext_cur_ngs_gs(ext_cur_ngs_gs_target_c)                   5649    8408  0.00
% ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_c)              5649    8622  1.61
% ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_java)           5649    8622  1.61
% ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java)             5649    8408  0.00
% ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih)        5649    8491  0.00
% ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain)  5649  572752 87.54
% ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_trans)  5649  468830 92.29
%
% This indicates that for each extension we actually invoke
% make_module_file_name with, either
%
% - the cache has a good hit rate for that extension's translations, or
% - the extension is translated too few times for its hit rate to matter,
%

:- type cache_stats
    --->    cache_stats(
                lookups     :: uint,
                misses      :: uint
            ).

:- type ext_stats == map(ext, cache_stats).

:- mutable(module_name_cache_stats, cache_stats, cache_stats(0u, 0u),
    ground, [untrailed, attach_to_io_state]).

:- mutable(ext_cache_stats, ext_stats, map.init,
    ground, [untrailed, attach_to_io_state]).

%---------------------%

:- pred record_module_name_cache_miss(io::di, io::uo) is det.

record_module_name_cache_miss(!IO) :-
    get_module_name_cache_stats(ModuleNameStats0, !IO),
    ModuleNameStats0 = cache_stats(Lookups0, Misses0),
    ModuleNameStats = cache_stats(Lookups0 + 1u, Misses0 + 1u),
    set_module_name_cache_stats(ModuleNameStats, !IO).

:- pred record_module_name_cache_hit(io::di, io::uo) is det.

record_module_name_cache_hit(!IO) :-
    get_module_name_cache_stats(ModuleNameStats0, !IO),
    ModuleNameStats0 = cache_stats(Lookups0, Misses0),
    ModuleNameStats = cache_stats(Lookups0 + 1u, Misses0),
    set_module_name_cache_stats(ModuleNameStats, !IO).

%---------------------%

:- pred record_ext_cache_miss(ext::in, io::di, io::uo) is det.

record_ext_cache_miss(Ext, !IO) :-
    get_ext_cache_stats(Map0, !IO),
    % The first access can, and will, be a miss.
    ( if map.search(Map0, Ext, Stats0) then
        Stats0 = cache_stats(Lookups0, Misses0),
        Stats = cache_stats(Lookups0 + 1u, Misses0 + 1u),
        map.det_update(Ext, Stats, Map0, Map)
    else
        Stats = cache_stats(1u, 1u),
        map.det_insert(Ext, Stats, Map0, Map)
    ),
    set_ext_cache_stats(Map, !IO).

:- pred record_ext_cache_hit(ext::in, io::di, io::uo) is det.

record_ext_cache_hit(Ext, !IO) :-
    get_ext_cache_stats(Map0, !IO),
    % A hit cannot be the first reference to Ext;
    % it must be preceded by a miss.
    map.lookup(Map0, Ext, Stats0),
    Stats0 = cache_stats(Lookups0, Misses0),
    Stats = cache_stats(Lookups0 + 1u, Misses0),
    map.det_update(Ext, Stats, Map0, Map),
    set_ext_cache_stats(Map, !IO).

%---------------------%

record_module_ext_cache_stats(!IO) :-
    get_module_name_cache_stats(ModuleNameStats, !IO),
    ModuleNameStats = cache_stats(MNLookups, MNMisses),
    get_ext_cache_stats(ExtMap, !IO),
    ( if
        MNLookups = 0u,
        MNMisses = 0u,
        map.is_empty(ExtMap)
    then
        true
    else
        io.open_append("/tmp/MODULE_EXT_CACHE_STATS", Result, !IO),
        (
            Result = error(_)
        ;
            Result = ok(OutStream),
            io.format(OutStream, "%-55s %8u %8u\n",
                [s("module_name"), u(MNLookups), u(MNMisses)], !IO),
            map.foldl(write_cache_stats_entry(OutStream), ExtMap, !IO),
            io.close_output(OutStream, !IO)
        )
    ).

:- pred write_cache_stats_entry(io.text_output_stream::in,
    ext::in, cache_stats::in, io::di, io::uo) is det.

write_cache_stats_entry(OutStream, Ext, Stats, !IO) :-
    % Strip off the outer wrapper to shorten ExtStr.
    % This does not lose any information, because the function symbols
    % of the argument types of ext_cur, ext_cur_ngs etc all start with
    % a prefix that is identical to the name of their wrapper.
    ( Ext = ext_cur(ExtSub),                ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs(ExtSub),            ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_gs(ExtSub),             ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_gas(ExtSub),            ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs_gs(ExtSub),         ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs_gas(ExtSub),        ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs_gs_err(ExtSub),     ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs_gs_java(ExtSub),    ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_pgs_max_cur(ExtSub),    ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs_gs_max_cur(ExtSub), ExtStr = string.string(ExtSub)
    ; Ext = ext_cur_ngs_gs_max_ngs(ExtSub), ExtStr = string.string(ExtSub)
    ),
    Stats = cache_stats(Lookups, Misses),
    io.format(OutStream, "%-55s %8u %8u\n",
        [s(ExtStr), u(Lookups), u(Misses)], !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.make_module_file_names.
%---------------------------------------------------------------------------%
