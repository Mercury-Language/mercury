%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% Copyright (C) 2020-2023, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.index_set.m.
%
% Dependency computation does a lot of unions so we use a set representation
% suited to that purpose, namely bitsets. We can't store module_names and
% target_ids in those sets, so we keep two maps
%
%   module_name <-> module_index, and
%   target_id   <-> target_id_index
%
% in the make_info structure, and work with sets of indices instead.
%
%---------------------------------------------------------------------------%

:- module make.index_set.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
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
:- type index_set(T) == sparse_bitset(T).
% :- type index_set(T) == tree_bitset(T).

:- func index_set_init = index_set(T).

:- pred index_set_insert(T::in, index_set(T)::in, index_set(T)::out)
     is det <= uenum(T).
:- pred index_set_delete(T::in, index_set(T)::in, index_set(T)::out)
     is det <= uenum(T).

:- func index_set_union(index_set(T), index_set(T)) = index_set(T).
:- pred index_set_union(index_set(T)::in, index_set(T)::in, index_set(T)::out)
    is det.

:- func index_set_union_list(list(index_set(T))) = index_set(T).

:- func index_set_difference(index_set(T), index_set(T)) = index_set(T).

:- func list_to_index_set(list(T)) = index_set(T) <= uenum(T).
:- pred list_to_index_set(list(T)::in, index_set(T)::out) is det <= uenum(T).

:- func index_set_to_sorted_list(index_set(T)) = list(T) <= uenum(T).

:- pred index_set_foldl(
    pred(T, A, A)::in(pred(in, in, out) is det),
    index_set(T)::in, A::in, A::out) is det <= uenum(T).

:- pred index_set_foldl2(
    pred(T, A, A, B, B)::in(pred(in, in, out, in, out) is det),
    index_set(T)::in, A::in, A::out, B::in, B::out) is det <= uenum(T).

:- pred index_set_member(T::in, index_set(T)::in) is semidet <= uenum(T).

%---------------------------------------------------------------------------%

:- type module_index.
:- instance uenum(module_index).

:- type target_id_index.
:- instance uenum(target_id_index).

:- type module_index_set == index_set(module_index).
:- type target_id_index_set == index_set(target_id_index).

%---------------------------------------------------------------------------%

    % Convert a module_name to a module_index.
    %
:- pred module_name_to_index(module_name::in, module_index::out,
    make_info::in, make_info::out) is det.

    % Convert a list of module_names to a module_index set.
    %
:- pred module_names_to_index_set(list(module_name)::in, module_index_set::out,
    make_info::in, make_info::out) is det.

    % Convert a module_index to a module_name.
    %
:- pred module_index_to_name(make_info::in,
    module_index::in, module_name::out) is det.

    % Convert a module_index set to a module_name set.
    %
:- pred module_index_set_to_plain_set(make_info::in,
    module_index_set::in, set(module_name)::out) is det.

%---------------------------------------------------------------------------%

    % Convert a target id to a target_id_index.
    %
:- pred target_id_to_index(target_id_module_index::in, target_id_index::out,
    make_info::in, make_info::out) is det.

    % Convert a list of target ids, or raw filenames, to a target_id_index set.
    %
:- pred target_ids_to_index_set(list(target_id)::in, target_id_index_set::out,
    make_info::in, make_info::out) is det.
:- pred file_names_to_index_set(list(file_name)::in, target_id_index_set::out,
    make_info::in, make_info::out) is det.

    % Convert a target_id_index set to a target_id set.
    %
:- pred target_id_index_set_to_plain_set(make_info::in,
    target_id_index_set::in, set(target_id)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%
%
% Operations on index_sets.
%

index_set_init = sparse_bitset.init.

index_set_insert(X, !Set) :-
    sparse_bitset.insert(X, !Set).

index_set_delete(X, !Set) :-
    sparse_bitset.delete(X, !Set).

index_set_union(SetA, SetB) = Set :-
    Set = sparse_bitset.union(SetA, SetB).

index_set_union(SetA, SetB, Set) :-
    sparse_bitset.union(SetA, SetB, Set).

index_set_union_list(Sets) = Set :-
    Set = sparse_bitset.union_list(Sets).

index_set_difference(SetA, SetB) = Set :-
    Set = sparse_bitset.difference(SetA, SetB).

list_to_index_set(List) = Set :-
    Set = sparse_bitset.list_to_set(List).

list_to_index_set(List, Set) :-
    sparse_bitset.list_to_set(List, Set).

index_set_to_sorted_list(Set) = SortedList :-
    SortedList = sparse_bitset.to_sorted_list(Set).

index_set_foldl(Pred, Set, !Acc1) :-
    sparse_bitset.foldl(Pred, Set, !Acc1).

index_set_foldl2(Pred, Set, !Acc1, !Acc2) :-
    sparse_bitset.foldl2(Pred, Set, !Acc1, !Acc2).

index_set_member(Item, Set) :-
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

:- type target_id_index
    --->    target_id_index(uint).

:- instance uenum(target_id_index) where [
    to_uint(target_id_index(U)) = U,
    from_uint(U, target_id_index(U))
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
        index_set_init, IndexSet, !Info).

:- pred module_names_to_index_set_loop(list(module_name)::in,
    module_index_set::in, module_index_set::out,
    make_info::in, make_info::out) is det.

module_names_to_index_set_loop([], !IndexSet, !Info).
module_names_to_index_set_loop([ModuleName | ModuleNames], !Set, !Info) :-
    module_name_to_index(ModuleName, ModuleIndex, !Info),
    index_set_insert(ModuleIndex, !Set),
    module_names_to_index_set_loop(ModuleNames, !Set, !Info).

%---------------------------------------------------------------------------%

module_index_to_name(Info, Index, ModuleName) :-
    make_info_get_module_index_map(Info) =
        module_index_map(_ForwardMap, ReverseMap, _Size),
    Index = module_index(I),
    ModuleName = version_array.lookup(ReverseMap, uint.cast_to_int(I)).

%---------------------%

module_index_set_to_plain_set(Info, ModuleIndices, Modules) :-
    index_set_foldl(acc_module_index_set_to_plain_set(Info), ModuleIndices,
        set.init, Modules).

:- pred acc_module_index_set_to_plain_set(make_info::in, module_index::in,
    set(module_name)::in, set(module_name)::out) is det.

acc_module_index_set_to_plain_set(Info, ModuleIndex, !Set) :-
    module_index_to_name(Info, ModuleIndex, ModuleName),
    set.insert(ModuleName, !Set).

%---------------------------------------------------------------------------%

target_id_to_index(TargetId, Index, !Info) :-
    Map0 = make_info_get_target_id_index_map(!.Info),
    Map0 = target_id_index_map(ForwardMap0, _ReverseMap0, _USize0),
    ( if version_hash_table.search(ForwardMap0, TargetId, Index0) then
        Index = Index0
    else
        Map0 = target_id_index_map(_ForwardMap0, ReverseMap0, USize0),
        Slot = USize0,
        Index = target_id_index(USize0),
        USize = USize0 + 1u,
        version_hash_table.det_insert(TargetId, Index,
            ForwardMap0, ForwardMap),
        RevSize0 = version_array.size(ReverseMap0),
        ( if uint.cast_to_int(USize) > RevSize0 then
            RevSize = increase_array_size(RevSize0),
            version_array.resize(RevSize, TargetId, ReverseMap0, ReverseMap)
        else
            version_array.set(uint.cast_to_int(Slot), TargetId,
                ReverseMap0, ReverseMap)
        ),
        Map = target_id_index_map(ForwardMap, ReverseMap, USize),
        make_info_set_target_id_index_map(Map, !Info)
    ).

%---------------------%

target_ids_to_index_set(TargetIds, TargetIdIndexSet, !Info) :-
    list.foldl2(acc_target_ids_to_index_set, TargetIds,
        index_set_init, TargetIdIndexSet, !Info).

:- pred acc_target_ids_to_index_set(target_id::in,
    target_id_index_set::in, target_id_index_set::out,
    make_info::in, make_info::out) is det.

acc_target_ids_to_index_set(TargetId, !TargetIdIndexSet, !Info) :-
    (
        TargetId = merc_target(target_file(ModuleName, TargetType)),
        module_name_to_index(ModuleName, ModuleIndex, !Info),
        TargetIdMI = timi_merc(ModuleIndex, TargetType)
    ;
        TargetId = non_merc_target(FileName),
        TargetIdMI = timi_non_merc(FileName)
    ),
    target_id_to_index(TargetIdMI, TargetIdIndex, !Info),
    index_set_insert(TargetIdIndex, !TargetIdIndexSet).

%---------------------%

file_names_to_index_set(FileNames, TargetIdIndexSet, !Info) :-
    list.foldl2(acc_file_names_to_index_set, FileNames,
        index_set_init, TargetIdIndexSet, !Info).

:- pred acc_file_names_to_index_set(file_name::in,
    target_id_index_set::in, target_id_index_set::out,
    make_info::in, make_info::out) is det.

acc_file_names_to_index_set(FileName, !TargetIdIndexSet, !Info) :-
    TargetId = timi_non_merc(FileName),
    target_id_to_index(TargetId, TargetIdIndex, !Info),
    index_set_insert(TargetIdIndex, !TargetIdIndexSet).

%---------------------------------------------------------------------------%

:- pred index_to_target_id(make_info::in, target_id_index::in,
    target_id::out) is det.

index_to_target_id(Info, Index, TargetId) :-
    make_info_get_target_id_index_map(Info) =
        target_id_index_map(_ForwardMap, ReverseMap, _Size),
    Index = target_id_index(I),
    version_array.lookup(ReverseMap, cast_to_int(I), TargetId0),
    (
        TargetId0 = timi_merc(ModuleIndex, FileType),
        module_index_to_name(Info, ModuleIndex, ModuleName),
        TargetId = merc_target(target_file(ModuleName, FileType))
    ;
        TargetId0 = timi_non_merc(FileName),
        TargetId = non_merc_target(FileName)
    ).

%---------------------%

target_id_index_set_to_plain_set(Info, TargetIdIndexes, TargetIds) :-
    index_set_foldl(acc_target_id_index_set_to_plain_set(Info),
        TargetIdIndexes, [], TargetIdsList),
    TargetIds = set.list_to_set(TargetIdsList).

:- pred acc_target_id_index_set_to_plain_set(make_info::in,
    target_id_index::in, list(target_id)::in, list(target_id)::out) is det.

acc_target_id_index_set_to_plain_set(Info, TargetIndex, !TargetIds) :-
    index_to_target_id(Info, TargetIndex, TargetId),
    !:TargetIds = [TargetId | !.TargetIds].

%---------------------------------------------------------------------------%
:- end_module make.index_set.
%---------------------------------------------------------------------------%
