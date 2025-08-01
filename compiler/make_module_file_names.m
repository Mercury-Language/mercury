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
% This module constructs filenames for generate_mmakefile_fragments.m.
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
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

:- type module_file_name_cache.

:- func init_module_file_name_cache = module_file_name_cache.

%---------------------------------------------------------------------------%

:- pred make_module_file_name_group_with_ext(globals::in, string::in,
    ext::in, set(module_name)::in, list(mmake_file_name_group)::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

:- pred make_module_file_names_with_ext(globals::in, ext::in,
    list(module_name)::in, list(mmake_file_name)::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

:- pred make_module_file_name(globals::in, string::in, ext::in,
    module_name::in, file_name::out,
    module_file_name_cache::in, module_file_name_cache::out) is det.

%---------------------------------------------------------------------------%

:- pred record_write_deps_file_cache_stats(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type module_file_name_cache == map(ext, map(module_name, file_name)).

init_module_file_name_cache = map.init.

%---------------------------------------------------------------------------%
%
% The call to module_name_to_file_name in make_module_file_name below
% constructs the filename from three components: the directory path,
% the module name itself, and the extension string. The first and third
% components depend only on Ext, not on ModuleName, which means that
% we *could* compute those parts here with:
%
%   ext_to_dir_path(Globals, not_for_search, Ext, DirNames),
%   (
%       DirNames = [],
%       MaybeDirPath = no
%   ;
%       DirNames = [_ | _],
%       DirPath = dir.relative_path_name_from_components(DirNames),
%       MaybeDirPath = yes(DirPath)
%   ),
%   ExtStr = extension_to_string(Globals, Ext),
%
% and then use
%
%   % This code performs the job of
%   %   module_name_to_file_name(Globals, From, Ext, ModuleName, FileName)
%   % but with the directory path and extension string parts done just
%   % once in make_module_file_names_with_ext, instead of being repeated
%   % each time here.
%   BaseNameNoExt = module_name_to_base_file_name_no_ext(Ext, ModuleName),
%   BaseName = BaseNameNoExt ++ ExtStr,
%   (
%       MaybeDirPath = no,
%       FileName = BaseName
%   ;
%       MaybeDirPath = yes(DirPath),
%       FileName =
%           dir.relative_path_name_from_components([DirPath, BaseName])
%   ),
%
% in make_module_file_name to construct FileName.
%
% We have tried this out. It works, but the performance of the updated
% compiler is indistinguishable from the performance of the pre-update
% compiler. (See the thread on m-rev on 2023 Sep 7.) And since this
% approach requires two versions of make_module_file_name, one that has
% the first and third components as arguments (for the call here)
% and one that does not (for all the other calls above) would be
% a double maintenance burden. This is why we don't use this approach.
%

make_module_file_name_group_with_ext(Globals, GroupName, Ext,
        ModuleSet, Groups, !Cache) :-
    set.to_sorted_list(ModuleSet, Modules),
    make_module_file_names_with_ext(Globals, Ext, Modules, FileNames, !Cache),
    Groups = make_file_name_group(GroupName, FileNames).

make_module_file_names_with_ext(Globals, Ext, Modules, FileNames,
        !Cache) :-
    list.map_foldl(make_module_file_name(Globals, $pred, Ext),
        Modules, FileNames, !Cache).

make_module_file_name(Globals, From, Ext, ModuleName, FileName, !Cache) :-
    % We cache result of the translation, in order to save on
    % temporary string construction.
    % See the analysis of gathered statistics below for why we use the cache
    % for filenames with *all* extensions.
    ( if map.search(!.Cache, Ext, ExtMap0) then
        ( if map.search(ExtMap0, ModuleName, CachedFileName) then
            trace [
                compile_time(flag("write_deps_file_cache")),
                run_time(env("WRITE_DEPS_FILE_CACHE")),
                io(!TIO)
            ] (
                record_cache_hit(Ext, !TIO)
            ),
            FileName = CachedFileName
        else
            trace [
                compile_time(flag("write_deps_file_cache")),
                run_time(env("WRITE_DEPS_FILE_CACHE")),
                io(!TIO)
            ] (
                record_cache_miss(Ext, !TIO)
            ),
            % XXX LEGACY
            module_name_to_file_name(Globals, From, Ext, ModuleName,
                FileName, _FileNameProposed),
            map.det_insert(ModuleName, FileName, ExtMap0, ExtMap),
            map.det_update(Ext, ExtMap, !Cache)
        )
    else
        trace [
            compile_time(flag("write_deps_file_cache")),
            run_time(env("WRITE_DEPS_FILE_CACHE")),
            io(!TIO)
        ] (
            record_cache_miss(Ext, !TIO)
        ),
        % XXX LEGACY
        module_name_to_file_name(Globals, From, Ext, ModuleName,
            FileName, _FileNameProposed),
        ExtMap = map.singleton(ModuleName, FileName),
        map.det_insert(Ext, ExtMap, !Cache)
    ).

%---------------------------------------------------------------------------%
%
% The code in this section is invoked only if both the compile time and
% the runtime conditions of the trace goals in make_module_file_name are met.
% Its job is to gather statistics about
%
% - how many times we try to translate filenames with each extension, and
% - what the hit rate of the cache is for each extension.
%
% The data gathered here, written out by record_write_deps_file_cache_stats,
% can be summarized by tools/write_deps_file_stats.
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

:- type file_name_cache_stats == map(ext, cache_stats).

:- mutable(module_file_name_cache_stats, file_name_cache_stats, map.init,
    ground, [untrailed, attach_to_io_state]).

:- pred record_cache_miss(ext::in, io::di, io::uo) is det.

record_cache_miss(Ext, !IO) :-
    get_module_file_name_cache_stats(Map0, !IO),
    % The first access can, and will, be a miss.
    ( if map.search(Map0, Ext, Stats0) then
        Stats0 = cache_stats(Lookups0, Misses0),
        Stats = cache_stats(Lookups0 + 1u, Misses0 + 1u),
        map.det_update(Ext, Stats, Map0, Map)
    else
        Stats = cache_stats(1u, 1u),
        map.det_insert(Ext, Stats, Map0, Map)
    ),
    set_module_file_name_cache_stats(Map, !IO).

:- pred record_cache_hit(ext::in, io::di, io::uo) is det.

record_cache_hit(Ext, !IO) :-
    get_module_file_name_cache_stats(Map0, !IO),
    % A hit cannot be the first reference to Ext;
    % it must be preceded by a miss.
    map.lookup(Map0, Ext, Stats0),
    Stats0 = cache_stats(Lookups0, Misses0),
    Stats = cache_stats(Lookups0 + 1u, Misses0),
    map.det_update(Ext, Stats, Map0, Map),
    set_module_file_name_cache_stats(Map, !IO).

%---------------------%

record_write_deps_file_cache_stats(!IO) :-
    get_module_file_name_cache_stats(Map, !IO),
    ( if map.is_empty(Map) then
        true
    else
        io.open_append("/tmp/WRITE_DEPS_FILE_CACHE_STATS", Result, !IO),
        (
            Result = error(_)
        ;
            Result = ok(OutStream),
            map.foldl(write_cache_stats_entry(OutStream), Map, !IO),
            io.close_output(OutStream, !IO)
        )
    ).

:- pred write_cache_stats_entry(io.text_output_stream::in,
    ext::in, cache_stats::in, io::di, io::uo) is det.

write_cache_stats_entry(OutStream, Ext, Stats, !IO) :-
    Stats = cache_stats(Lookups, Misses),
    io.format(OutStream, "%-55s %8u %8u\n",
        [s(string.string(Ext)), u(Lookups), u(Misses)], !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.make_module_file_names.
%---------------------------------------------------------------------------%
