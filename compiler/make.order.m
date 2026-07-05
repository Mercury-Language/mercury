%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.order.m.
%
% Get lists of target modules. On request, sort them by name or by timestamp.
%
%---------------------------------------------------------------------------%

:- module make.order.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- pred order_target_modules(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Remove all nested modules from a list of modules.
    %
:- pred filter_out_nested_modules(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred get_target_modules(io.text_output_stream::in, globals::in,
    module_target_type::in, list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred get_nonnested_and_parent_modules(io.text_output_stream::in,
    globals::in, list(module_name)::in,
    list(module_name)::out, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Return a list of modules in reverse order of their dependencies,
    % i.e. the list is the module dependency graph from bottom-up. Mutually
    % dependent modules (modules which form a clique in the dependency graph)
    % are returned adjacent in the list in arbitrary order.
    %
:- pred get_bottom_up_ordered_modules(
    map(module_name, maybe_module_dep_info)::in,
    list(module_name)::in, list(module_name)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.get_module_dep_info.
:- import_module make.timestamp.
:- import_module parse_tree.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.

:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

order_target_modules(ProgressStream, Globals, Modules, OrderedModules,
        !Info, !IO) :-
    globals.lookup_bool_option(Globals, order_make_by_timestamp,
        OrderByTimestamp),
    (
        OrderByTimestamp = yes,
        list.map_foldl2(pair_module_with_timestamp(ProgressStream, Globals),
            Modules, PairedModules, !Info, !IO),
        list.sort(compare_paired_modules, PairedModules, RevOrderedPairs),
        % More recently touched files, i.e. files with *larger* timestamps,
        % should appear *earlier* in the list.
        list.reverse(RevOrderedPairs, OrderedPairs),
        list.map(pair.snd, OrderedPairs, OrderedModules)
    ;
        OrderByTimestamp = no,
        list.map(pair_module_with_name, Modules, PairedModules),
        list.sort(compare_paired_modules, PairedModules, OrderedPairs),
        list.map(pair.snd, OrderedPairs, OrderedModules)
    ).

:- pred pair_module_with_timestamp(io.text_output_stream::in, globals::in,
    module_name::in, pair(timestamp, module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

pair_module_with_timestamp(ProgressStream, Globals, Module,
        Timestamp - Module, !Info, !IO) :-
    Target = target_file(Module, module_target_source),
    get_target_timestamp(ProgressStream, Globals, Target, MaybeTimestamp,
        !Info, !IO),
    (
        MaybeTimestamp = ok(Timestamp)
    ;
        MaybeTimestamp = error(_),
        Timestamp = oldest_timestamp
    ).

:- pred pair_module_with_name(module_name::in,
    pair(string, module_name)::out) is det.

pair_module_with_name(Module, Name - Module) :-
    Name = sym_name_to_string(Module).

:- pred compare_paired_modules(pair(T, module_name)::in,
    pair(T, module_name)::in, comparison_result::out) is det.

compare_paired_modules(KeyA - ModuleA, KeyB - ModuleB, Result) :-
    compare(KeyResult, KeyA, KeyB),
    % More recently touched files should appear earlier in the list.
    (
        ( KeyResult = (<)
        ; KeyResult = (>)
        ),
        Result = KeyResult
    ;
        KeyResult = (=),
        ModuleAStr = sym_name_to_string(ModuleA),
        ModuleBStr = sym_name_to_string(ModuleB),
        compare(Result, ModuleAStr, ModuleBStr)
    ).

%---------------------------------------------------------------------------%

filter_out_nested_modules(ProgressStream, Globals, Modules0, Modules,
        !Info, !IO) :-
    list.foldl3(collect_nested_modules(ProgressStream, Globals), Modules0,
        set.init, NestedModules, !Info, !IO),
    list.negated_filter(set.contains(NestedModules), Modules0, Modules).

:- pred collect_nested_modules(io.text_output_stream::in, globals::in,
    module_name::in, set(module_name)::in, set(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

collect_nested_modules(ProgressStream, Globals, ModuleName,
        !NestedModules, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        NestedSubModules = get_nested_children_of_top_module(MaybeTopModule),
        set.union(NestedSubModules, !NestedModules)
    ;
        MaybeModuleDepInfo = no_module_dep_info
    ).

%---------------------------------------------------------------------------%

get_target_modules(ProgressStream, Globals, TargetType,
        AllModules, TargetModules, !Info, !IO) :-
    ( if TargetType = module_target_errors then
        % `.err' files are only produced for the top-level module
        % in each source file.
        list.foldl3(
            get_non_nested_target_modules(ProgressStream, Globals),
            AllModules, cord.init, TargetModulesCord, !Info, !IO),
        TargetModules = cord.list(TargetModulesCord)
    else
        TargetModules = AllModules
    ).

:- pred get_non_nested_target_modules(io.text_output_stream::in, globals::in,
    module_name::in, cord(module_name)::in, cord(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_non_nested_target_modules(ProgressStream, Globals, ModuleName,
        !TargetModulesCord, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    ( if
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_source_file_module_name(ModuleDepInfo,
            SourceFileModuleName),
        ModuleName = SourceFileModuleName
    then
        cord.snoc(ModuleName, !TargetModulesCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

get_nonnested_and_parent_modules(ProgressStream, Globals, ModuleNames,
        NonnestedModules, ParentModules, !Info, !IO) :-
    list.foldl4(
        acc_nonnested_and_parent_modules(ProgressStream, Globals),
        ModuleNames,
        [], NonnestedModules, [], ParentModules, !Info, !IO).

:- pred acc_nonnested_and_parent_modules(io.text_output_stream::in,
    globals::in, module_name::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

acc_nonnested_and_parent_modules(ProgressStream, Globals, ModuleName,
        !NonnestedModules, !ParentModules, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        (
            MaybeTopModule = top_module(_NestedSubModules),
            % don't include in NestedModules
            %   which means DO include in NonnestedModules
            !:NonnestedModules = [ModuleName | !.NonnestedModules],
            module_dep_info_get_children(ModuleDepInfo, Children),
            ( if set.is_empty(Children) then
                true
            else
                !:ParentModules = [ModuleName | !.ParentModules]
            )
        ;
            MaybeTopModule = not_top_module
            % do include in NestedModules
            %   which means DO NOT include in NonnestedModules
            %   which means DO NOT include in ParentModules
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        % don't include in NestedModules
        %   which means DO include in NonnestedModules
        % do not include in ParentModules
        %   due to absence of info about any children
        !:NonnestedModules = [ModuleName | !.NonnestedModules]
    ).

%---------------------------------------------------------------------------%

get_bottom_up_ordered_modules(ModuleDeps, Modules0, Modules) :-
    list.foldl2(
        add_module_relations(lookup_module_dep_info_in_maybe_map(ModuleDeps)),
        Modules0, digraph.init, _IntDepsGraph, digraph.init, ImpDepsGraph),
    SccSets = digraph.return_sccs_in_to_from_order(ImpDepsGraph),
    list.map(set.to_sorted_list, SccSets, SccLists),
    list.condense(SccLists, Modules).

:- func lookup_module_dep_info_in_maybe_map(
    map(module_name, maybe_module_dep_info), module_name)
    = module_dep_info.

lookup_module_dep_info_in_maybe_map(ModuleDeps, ModuleName) = ModuleDepInfo :-
    map.lookup(ModuleDeps, ModuleName, MaybeModuleDepInfo),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        unexpected($pred, "MaybeModuleDepInfo = no")
    ).

    % add_module_relations(LookupModuleImports, ModuleName,
    %   !IntDepsRel, !ImpDepsRel)
    %
    % Add a module's interface and implementation dependencies to IntDepsRel
    % and ImpDepsRel respectively. Dependencies are found using the
    % LookupModuleImports function.
    %
:- pred add_module_relations(lookup_module_dep_info_func::in, module_name::in,
    digraph(module_name)::in, digraph(module_name)::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

add_module_relations(LookupModuleImportsFunc, ModuleName,
        !IntDepsGraph, !ImpDepsGraph) :-
    ModuleDepInfo = LookupModuleImportsFunc(ModuleName),
    add_module_dep_info_to_deps_graph(ModuleDepInfo, LookupModuleImportsFunc,
        !IntDepsGraph, !ImpDepsGraph).

%---------------------------------------------------------------------------%
:- end_module make.order.
%---------------------------------------------------------------------------%
