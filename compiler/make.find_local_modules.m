%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.find_local_modules.m.
%
% This module contains code to find the modules in the current directory
% that are reachable from a given module.
%
%---------------------------------------------------------------------------%

:- module make.find_local_modules.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.deps_cache.
:- import_module make.deps_set.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module set.

%---------------------------------------------------------------------------%

    % Find all modules in the current directory which are reachable
    % (by import or include) from the given module.
    %
:- pred find_reachable_local_modules(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, set(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred find_transitive_module_dependencies(io.text_output_stream::in,
    globals::in, transitive_dependencies_type::in,
    process_modules_where::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module make.get_module_dep_info.
:- import_module make.util.
:- import_module parse_tree.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.prog_data_foreign.

:- import_module dir.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module sparse_bitset.
:- import_module string.

%---------------------------------------------------------------------------%

find_reachable_local_modules(ProgressStream, Globals, ModuleName, Succeeded,
        ModuleNameSet, !Info, !IO) :-
    module_name_to_index(ModuleName, ModuleIndex, !Info),
    find_transitive_module_dependencies(ProgressStream, Globals,
        all_dependencies, process_only_modules_in_cur_dir, ModuleIndex,
        Succeeded, ModuleIndexSet, !Info, !IO),
    module_index_set_to_plain_set(!.Info, ModuleIndexSet, ModuleNameSet).

find_transitive_module_dependencies(ProgressStream, Globals, WhichDeps,
        ProcessModulesWhere, ModuleIndex, Succeeded, ModuleIndexSet,
        !Info, !IO) :-
    Key = trans_deps_key(ModuleIndex, WhichDeps, ProcessModulesWhere),
    ( if search_trans_deps_cache(!.Info, Key, Result0) then
        Result0 = deps_result(Succeeded, ModuleIndexSet)
    else
        KeepGoing = make_info_get_keep_going(!.Info),
        find_transitive_module_dependencies_uncached(ProgressStream, KeepGoing,
            WhichDeps, ProcessModulesWhere, Globals, ModuleIndex,
            Succeeded, deps_set_init, ModuleIndexSet, !Info, !IO),
        Result = deps_result(Succeeded, ModuleIndexSet),
        add_to_trans_deps_cache(Key, Result, !Info)
    ).

:- pred find_transitive_module_dependencies_uncached(io.text_output_stream::in,
    maybe_keep_going::in, transitive_dependencies_type::in,
    process_modules_where::in, globals::in,
    module_index::in, maybe_succeeded::out,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_transitive_module_dependencies_uncached(ProgressStream, KeepGoing,
        WhichDeps, ProcessModulesWhere, Globals, ModuleIndex, Succeeded,
        ModuleIndexSet0, ModuleIndexSet, !Info, !IO) :-
    ( if
        deps_set_member(ModuleIndex, ModuleIndexSet0)
    then
        Succeeded = succeeded,
        ModuleIndexSet = ModuleIndexSet0
    else if
        Key = trans_deps_key(ModuleIndex, WhichDeps,
            ProcessModulesWhere),
        search_trans_deps_cache(!.Info, Key, Result0)
    then
        Result0 = deps_result(Succeeded, ModuleIndexSet1),
        deps_set_union(ModuleIndexSet0, ModuleIndexSet1, ModuleIndexSet)
    else
        module_index_to_name(!.Info, ModuleIndex, ModuleName),
        get_maybe_module_dep_info(ProgressStream, Globals,
            ModuleName, MaybeModuleDepInfo, !Info, !IO),
        (
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
            ( if
                (
                    ProcessModulesWhere = process_modules_anywhere
                ;
                    ProcessModulesWhere = process_only_modules_in_cur_dir,
                    ModuleDir = dir.this_directory
                )
            then
                % XXX MDNEW Pass ModuleDir to this call, and don't allow
                % later searches to look in the part of the search path
                % *before* ModuleDir.
                do_find_transitive_module_dependencies_uncached(ProgressStream,
                    KeepGoing, WhichDeps, ProcessModulesWhere, Globals,
                    ModuleIndex, ModuleName, ModuleDepInfo, Succeeded,
                    ModuleIndexSet0, ModuleIndexSet, !Info, !IO)
            else
                Succeeded = succeeded,
                ModuleIndexSet = ModuleIndexSet0
            )
        ;
            MaybeModuleDepInfo = no_module_dep_info,
            Succeeded = did_not_succeed,
            ModuleIndexSet = ModuleIndexSet0
        )
    ).

:- pred do_find_transitive_module_dependencies_uncached(
    io.text_output_stream::in, maybe_keep_going::in,
    transitive_dependencies_type::in, process_modules_where::in, globals::in,
    module_index::in, module_name::in, module_dep_info::in,
    maybe_succeeded::out,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

do_find_transitive_module_dependencies_uncached(ProgressStream, KeepGoing,
        WhichDeps, ProcessModulesWhere, Globals,
        ModuleIndex, ModuleName, ModuleDepInfo, Succeeded,
        ModuleIndexSet0, ModuleIndexSet, !Info, !IO) :-
    module_dep_info_get_fims(ModuleDepInfo, FIMSpecs),
    module_dep_info_get_module_name(ModuleDepInfo, MDI_ModuleName),
    expect(unify(ModuleName, MDI_ModuleName), $pred,
        "ModuleName != MDI_ModuleName"),
    module_dep_info_get_int_deps(ModuleDepInfo, IntImportedModuleNames),
    (
        % Ancestors don't need to be considered here.
        % Anywhere the interface of the child module is needed,
        % the ancestors must also have been imported.
        WhichDeps = interface_imports,
        ToCheckImportedModuleNames = IntImportedModuleNames,
        ToCheckIncludedModuleNames = set.init
    ;
        ( WhichDeps = all_dependencies
        ; WhichDeps = all_imports
        ),
        AncestorModuleNames = get_ancestors_set(ModuleName),
        module_dep_info_get_imp_deps(ModuleDepInfo, ImpImportedModuleNames),
        set.map((pred(fim_spec(_, Mod)::in, Mod::out) is det),
            FIMSpecs, ForeignImportedModuleNames),
        ToCheckImportedModuleNames = set.union_list([AncestorModuleNames,
            IntImportedModuleNames, ImpImportedModuleNames,
            ForeignImportedModuleNames]),
        (
            WhichDeps = all_dependencies,
            module_dep_info_get_children(ModuleDepInfo, ChildModuleNames),
            ToCheckIncludedModuleNames = ChildModuleNames
        ;
            WhichDeps = all_imports,
            ToCheckIncludedModuleNames = set.init
        )
    ),
    trace [
        compile_time(flag("find_trans_deps")),
        run_time(env("FIND_TRANS_DEPS")),
        io(!TIO)
    ] (
        PrintIndentedModuleName =
            ( pred(MN::in, TIO0::di, TIO::uo) is det :-
                io.format(ProgressStream, "    %s\n",
                    [s(sym_name_to_string(MN))], TIO0, TIO)
            ),
        ModuleNameStr = sym_name_to_string(ModuleName),
        io.format(ProgressStream, "imports by %s:\n",
            [s(ModuleNameStr)], !TIO),
        set.foldl(PrintIndentedModuleName, ToCheckImportedModuleNames, !TIO),
        io.format(ProgressStream, "includes by %s:\n",
            [s(ModuleNameStr)], !TIO),
        set.foldl(PrintIndentedModuleName, ToCheckIncludedModuleNames, !TIO),
        io.nl(ProgressStream, !TIO)
    ),
    module_names_to_index_set(set.to_sorted_list(ToCheckImportedModuleNames),
        ImportedModuleIndexSet, !Info),
    module_names_to_index_set(set.to_sorted_list(ToCheckIncludedModuleNames),
        IncludedModuleIndexSet, !Info),
    ToAccImportedModuleIndexes =
        deps_set_to_sorted_list(ImportedModuleIndexSet),
    ToAccIncludedModuleIndexes =
        deps_set_to_sorted_list(IncludedModuleIndexSet),

    deps_set_insert(ModuleIndex, ModuleIndexSet0, ModuleIndexSet1),
    OldImportingModule = make_info_get_importing_module(!.Info),
    make_info_set_importing_module(yes(ioi_import(ModuleName)), !Info),
    acc_module_index_trans_deps(ProgressStream, Globals, KeepGoing,
        WhichDeps, ProcessModulesWhere,
        ToAccImportedModuleIndexes, succeeded, SucceededImports,
        ModuleIndexSet1, ModuleIndexSet2, !Info, !IO),
    make_info_set_importing_module(yes(ioi_include(ModuleName)), !Info),
    acc_module_index_trans_deps(ProgressStream, Globals, KeepGoing,
        WhichDeps, ProcessModulesWhere,
        ToAccIncludedModuleIndexes, succeeded, SucceededIncludes,
        ModuleIndexSet2, ModuleIndexSet, !Info, !IO),
    make_info_set_importing_module(OldImportingModule, !Info),
    Succeeded = SucceededImports `and` SucceededIncludes.

:- pred acc_module_index_trans_deps(io.text_output_stream::in, globals::in,
    maybe_keep_going::in, transitive_dependencies_type::in,
    process_modules_where::in, list(module_index)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

acc_module_index_trans_deps(_, _, _, _, _, [], !Succeeded, !Deps, !Info, !IO).
acc_module_index_trans_deps(ProgressStream, Globals, KeepGoing,
        WhichDeps, ProcessModulesWhere, [HeadModuleIndex | TailModuleIndexes],
        !Succeeded, !ModuleIndexSet, !Info, !IO) :-
    find_transitive_module_dependencies_uncached(ProgressStream, KeepGoing,
        WhichDeps, ProcessModulesWhere, Globals,
        HeadModuleIndex, HeadSucceeded, !ModuleIndexSet, !Info, !IO),
    should_we_stop_or_continue(KeepGoing, HeadSucceeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        acc_module_index_trans_deps(ProgressStream, Globals, KeepGoing,
            WhichDeps, ProcessModulesWhere,
            TailModuleIndexes, !Succeeded, !ModuleIndexSet, !Info, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module make.find_local_modules.
%---------------------------------------------------------------------------%
