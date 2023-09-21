%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% Copyright (C) 2020-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.deps_set.m.
%
% Dependency computation does a lot of unions so we use a set representation
% suited to that purpose, namely bitsets. We can't store module_names and
% dependency_files in those sets, so we keep two maps
%
%   module_name     <-> module_index, and
%   dependency_file <-> dependency_file_index
%
% in the make_info structure, and work with sets of indices instead.
%
%---------------------------------------------------------------------------%

:- module make.deps_set.
:- interface.

:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module enum.
:- import_module list.
:- import_module set.
:- import_module sparse_bitset.

%---------------------------------------------------------------------------%

    % sparse_bitset is faster than tree_bitset by my tests.
    %
:- type deps_set(T) == sparse_bitset(T).
% :- type deps_set(T) == tree_bitset(T).

:- func deps_set_init = deps_set(T).

:- pred deps_set_insert(T::in, deps_set(T)::in, deps_set(T)::out)
     is det <= uenum(T).
:- pred deps_set_delete(T::in, deps_set(T)::in, deps_set(T)::out)
     is det <= uenum(T).

:- func deps_set_union(deps_set(T), deps_set(T)) = deps_set(T).
:- pred deps_set_union(deps_set(T)::in, deps_set(T)::in, deps_set(T)::out)
    is det.

:- func deps_set_union_list(list(deps_set(T))) = deps_set(T).

:- func deps_set_difference(deps_set(T), deps_set(T)) = deps_set(T).

:- func list_to_deps_set(list(T)) = deps_set(T) <= uenum(T).
:- pred list_to_deps_set(list(T)::in, deps_set(T)::out) is det <= uenum(T).

:- func deps_set_to_sorted_list(deps_set(T)) = list(T) <= uenum(T).

:- pred deps_set_foldl(
    pred(T, A, A)::in(pred(in, in, out) is det),
    deps_set(T)::in, A::in, A::out) is det <= uenum(T).

:- pred deps_set_foldl2(
    pred(T, A, A, B, B)::in(pred(in, in, out, in, out) is det),
    deps_set(T)::in, A::in, A::out, B::in, B::out) is det <= uenum(T).

:- pred deps_set_member(T::in, deps_set(T)::in) is semidet <= uenum(T).

%---------------------------------------------------------------------------%

:- type module_index.
:- instance uenum(module_index).

:- type dependency_file_index.
:- instance uenum(dependency_file_index).

%---------------------------------------------------------------------------%

    % Convert a module_name to a module_index.
    %
:- pred module_name_to_index(module_name::in, module_index::out,
    make_info::in, make_info::out) is det.

    % Convert a list of module_names to a module_index set.
    %
:- pred module_names_to_index_set(list(module_name)::in,
    deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

    % Convert a module_index to a module_name.
    %
:- pred module_index_to_name(make_info::in,
    module_index::in, module_name::out) is det.

    % Convert a module_index set to a module_name set.
    %
:- pred module_index_set_to_plain_set(make_info::in,
    deps_set(module_index)::in, set(module_name)::out) is det.

%---------------------------------------------------------------------------%

    % Convert a dependency file to a dependency_file_index.
    %
:- pred dependency_file_to_index(dependency_file_with_module_index::in,
    dependency_file_index::out, make_info::in, make_info::out) is det.

    % Convert a list of dependency files to a dependency_file_index set.
    %
:- pred dependency_files_to_index_set(list(dependency_file)::in,
    deps_set(dependency_file_index)::out, make_info::in, make_info::out)
    is det.

    % Convert a dependency_file_index set to a dependency_file set.
    %
:- pred dependency_file_index_set_to_plain_set(make_info::in,
    deps_set(dependency_file_index)::in, set(dependency_file)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%
%
% Operations on deps_sets.
%

deps_set_init = sparse_bitset.init.

deps_set_insert(X, !Set) :-
    sparse_bitset.insert(X, !Set).

deps_set_delete(X, !Set) :-
    sparse_bitset.delete(X, !Set).

deps_set_union(SetA, SetB) = Set :-
    Set = sparse_bitset.union(SetA, SetB).

deps_set_union(SetA, SetB, Set) :-
    sparse_bitset.union(SetA, SetB, Set).

deps_set_union_list(Sets) = Set :-
    Set = sparse_bitset.union_list(Sets).

deps_set_difference(SetA, SetB) = Set :-
    Set = sparse_bitset.difference(SetA, SetB).

list_to_deps_set(List) = Set :-
    Set = sparse_bitset.list_to_set(List).

list_to_deps_set(List, Set) :-
    sparse_bitset.list_to_set(List, Set).

deps_set_to_sorted_list(Set) = SortedList :-
    SortedList = sparse_bitset.to_sorted_list(Set).

deps_set_foldl(Pred, Set, !Acc1) :-
    sparse_bitset.foldl(Pred, Set, !Acc1).

deps_set_foldl2(Pred, Set, !Acc1, !Acc2) :-
    sparse_bitset.foldl2(Pred, Set, !Acc1, !Acc2).

deps_set_member(Item, Set) :-
    sparse_bitset.member(Item, Set).

%---------------------------------------------------------------------------%
%
% Bitset indices.
%

:- type module_index
    --->    module_index(uint).

:- instance uenum(module_index) where [
    to_uint(module_index(U)) = U,
    from_uint(U, module_index(U))
].

:- type dependency_file_index
    --->    dependency_file_index(uint).

:- instance uenum(dependency_file_index) where [
    to_uint(dependency_file_index(U)) = U,
    from_uint(U, dependency_file_index(U))
].

%---------------------------------------------------------------------------%

module_name_to_index(ModuleName, Index, !Info) :-
    Map0 = make_info_get_module_index_map(!.Info),
    Map0 = module_index_map(ForwardMap0, _ReverseMap0, _USize0),
    ( if version_hash_table.search(ForwardMap0, ModuleName, Index0) then
        Index = Index0
    else
        Map0 = module_index_map(_ForwardMap0, ReverseMap0, USize0),
        Slot = USize0,
        Index = module_index(USize0),
        USize = USize0 + 1u,
        version_hash_table.det_insert(ModuleName, Index,
            ForwardMap0, ForwardMap),
        RevSize0 = version_array.size(ReverseMap0),
        ( if cast_to_int(USize) > RevSize0 then
            RevSize = increase_array_size(RevSize0),
            version_array.resize(RevSize, ModuleName, ReverseMap0, ReverseMap)
        else
            version_array.set(uint.cast_to_int(Slot), ModuleName,
                ReverseMap0, ReverseMap)
        ),
        Map = module_index_map(ForwardMap, ReverseMap, USize),
        make_info_set_module_index_map(Map, !Info)
    ).

:- func increase_array_size(int) = int.

increase_array_size(N) = (if N = 0 then 1 else N * 2).

%---------------------%

module_names_to_index_set(ModuleNames, IndexSet, !Info) :-
    module_names_to_index_set_loop(ModuleNames,
        deps_set_init, IndexSet, !Info).

:- pred module_names_to_index_set_loop(list(module_name)::in,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

module_names_to_index_set_loop([], !IndexSet, !Info).
module_names_to_index_set_loop([ModuleName | ModuleNames], !Set, !Info) :-
    module_name_to_index(ModuleName, ModuleIndex, !Info),
    deps_set_insert(ModuleIndex, !Set),
    module_names_to_index_set_loop(ModuleNames, !Set, !Info).

%---------------------------------------------------------------------------%

module_index_to_name(Info, Index, ModuleName) :-
    make_info_get_module_index_map(Info) =
        module_index_map(_ForwardMap, ReverseMap, _Size),
    Index = module_index(I),
    ModuleName = version_array.lookup(ReverseMap, uint.cast_to_int(I)).

%---------------------%

module_index_set_to_plain_set(Info, ModuleIndices, Modules) :-
    deps_set_foldl(acc_module_index_set_to_plain_set(Info), ModuleIndices,
        set.init, Modules).

:- pred acc_module_index_set_to_plain_set(make_info::in, module_index::in,
    set(module_name)::in, set(module_name)::out) is det.

acc_module_index_set_to_plain_set(Info, ModuleIndex, !Set) :-
    module_index_to_name(Info, ModuleIndex, ModuleName),
    set.insert(ModuleName, !Set).

%---------------------------------------------------------------------------%

dependency_file_to_index(DepFile, Index, !Info) :-
    Map0 = make_info_get_dep_file_index_map(!.Info),
    Map0 = dependency_file_index_map(ForwardMap0, _ReverseMap0, _USize0),
    ( if version_hash_table.search(ForwardMap0, DepFile, Index0) then
        Index = Index0
    else
        Map0 = dependency_file_index_map(_ForwardMap0, ReverseMap0, USize0),
        Slot = USize0,
        Index = dependency_file_index(USize0),
        USize = USize0 + 1u,
        version_hash_table.det_insert(DepFile, Index, ForwardMap0, ForwardMap),
        RevSize0 = version_array.size(ReverseMap0),
        ( if uint.cast_to_int(USize) > RevSize0 then
            RevSize = increase_array_size(RevSize0),
            version_array.resize(RevSize, DepFile, ReverseMap0, ReverseMap)
        else
            version_array.set(uint.cast_to_int(Slot), DepFile,
                ReverseMap0, ReverseMap)
        ),
        Map = dependency_file_index_map(ForwardMap, ReverseMap, USize),
        make_info_set_dep_file_index_map(Map, !Info)
    ).

%---------------------%

dependency_files_to_index_set(DepFiles, DepIndexSet, !Info) :-
    list.foldl2(acc_dependency_files_to_index_set, DepFiles,
        deps_set_init, DepIndexSet, !Info).

:- pred acc_dependency_files_to_index_set(dependency_file::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

acc_dependency_files_to_index_set(DepFile0, !Set, !Info) :-
    (
        DepFile0 = dep_target(target_file(ModuleName, TargetType)),
        module_name_to_index(ModuleName, ModuleIndex, !Info),
        DepFile = dfmi_target(ModuleIndex, TargetType)
    ;
        DepFile0 = dep_file(FileName),
        DepFile = dfmi_file(FileName)
    ),
    dependency_file_to_index(DepFile, DepIndex, !Info),
    deps_set_insert(DepIndex, !Set).

%---------------------------------------------------------------------------%

:- pred index_to_dependency_file(make_info::in, dependency_file_index::in,
    dependency_file::out) is det.

index_to_dependency_file(Info, Index, DepFile) :-
    make_info_get_dep_file_index_map(Info) =
        dependency_file_index_map(_ForwardMap, ReverseMap, _Size),
    Index = dependency_file_index(I),
    version_array.lookup(ReverseMap, cast_to_int(I), DepFile0),
    (
        DepFile0 = dfmi_target(ModuleIndex, FileType),
        module_index_to_name(Info, ModuleIndex, ModuleName),
        DepFile = dep_target(target_file(ModuleName, FileType))
    ;
        DepFile0 = dfmi_file(FileName),
        DepFile = dep_file(FileName)
    ).

%---------------------%

dependency_file_index_set_to_plain_set(Info, DepIndices, DepFiles) :-
    deps_set_foldl(acc_dependency_file_index_set_to_plain_set(Info),
        DepIndices, [], DepFilesList),
    DepFiles = set.list_to_set(DepFilesList).

:- pred acc_dependency_file_index_set_to_plain_set(make_info::in,
    dependency_file_index::in,
    list(dependency_file)::in, list(dependency_file)::out) is det.

acc_dependency_file_index_set_to_plain_set(Info, DepIndex, List0, List) :-
    index_to_dependency_file(Info, DepIndex, DepFile),
    List = [DepFile | List0].

%---------------------------------------------------------------------------%
:- end_module make.deps_set.
%---------------------------------------------------------------------------%
