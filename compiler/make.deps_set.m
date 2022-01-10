%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
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

:- import_module make.dependencies.
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

:- type module_index.
:- instance enum(module_index).

:- type dependency_file_index.
:- instance enum(dependency_file_index).

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
:- pred dependency_file_to_index(dependency_file::in,
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

:- import_module libs.
:- import_module parse_tree.

:- import_module int.
:- import_module maybe.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%
%
% Bitset indices.
%

:- type module_index
    --->    module_index(int).

:- type dependency_file_index
    --->    dependency_file_index(int).

:- instance enum(module_index) where [
    to_int(module_index(I)) = I,
    from_int(I) = module_index(I)
].

:- instance enum(dependency_file_index) where [
    to_int(dependency_file_index(I)) = I,
    from_int(I) = dependency_file_index(I)
].

%---------------------------------------------------------------------------%

module_name_to_index(ModuleName, Index, !Info) :-
    Map0 = !.Info ^ mki_module_index_map,
    Map0 = module_index_map(Forward0, _Reverse0, _Size0),
    ( if version_hash_table.search(Forward0, ModuleName, Index0) then
        Index = Index0
    else
        Map0 = module_index_map(_Forward0, Reverse0, Size0),
        Index = module_index(Size0),
        Size = Size0 + 1,
        version_hash_table.det_insert(ModuleName, Index, Forward0, Forward),
        TrueSize = version_array.size(Reverse0),
        ( if Size > TrueSize then
            NewSize = increase_array_size(TrueSize),
            version_array.resize(NewSize, ModuleName, Reverse0, Reverse)
        else
            version_array.set(Size0, ModuleName, Reverse0, Reverse)
        ),
        Map = module_index_map(Forward, Reverse, Size),
        !Info ^ mki_module_index_map := Map
    ).

:- func increase_array_size(int) = int.

increase_array_size(N) = (if N = 0 then 1 else N * 2).

%---------------------%

module_names_to_index_set(ModuleNames, IndexSet, !Info) :-
    module_names_to_index_set_2(ModuleNames,
        sparse_bitset.init, IndexSet, !Info).

:- pred module_names_to_index_set_2(list(module_name)::in,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out) is det.

module_names_to_index_set_2([], !IndexSet, !Info).
module_names_to_index_set_2([ModuleName | ModuleNames], !Set, !Info) :-
    module_name_to_index(ModuleName, ModuleIndex, !Info),
    sparse_bitset.insert(ModuleIndex, !Set),
    module_names_to_index_set_2(ModuleNames, !Set, !Info).

%---------------------------------------------------------------------------%

module_index_to_name(Info, Index, ModuleName) :-
    Info ^ mki_module_index_map = module_index_map(_Forward, Reverse, _Size),
    Index = module_index(I),
    ModuleName = version_array.lookup(Reverse, I).

%---------------------%

module_index_set_to_plain_set(Info, ModuleIndices, Modules) :-
    foldl(module_index_set_to_plain_set_2(Info), ModuleIndices,
        set.init, Modules).

:- pred module_index_set_to_plain_set_2(make_info::in, module_index::in,
    set(module_name)::in, set(module_name)::out) is det.

module_index_set_to_plain_set_2(Info, ModuleIndex, !Set) :-
    module_index_to_name(Info, ModuleIndex, ModuleName),
    set.insert(ModuleName, !Set).

%---------------------------------------------------------------------------%

dependency_file_to_index(DepFile, Index, !Info) :-
    Map0 = !.Info ^ mki_dep_file_index_map,
    ForwardMap0 = Map0 ^ dfim_forward_map,
    ( if version_hash_table.search(ForwardMap0, DepFile, Index0) then
        Index = Index0
    else
        Map0 = dependency_file_index_map(Forward0, Reverse0, Size0),
        Index = dependency_file_index(Size0),
        Size = Size0 + 1,
        version_hash_table.det_insert(DepFile, Index, Forward0, Forward),
        TrueSize = version_array.size(Reverse0),
        ( if Size > TrueSize then
            NewSize = increase_array_size(TrueSize),
            version_array.resize(NewSize, DepFile, Reverse0, Reverse)
        else
            version_array.set(Size0, DepFile, Reverse0, Reverse)
        ),
        Map = dependency_file_index_map(Forward, Reverse, Size),
        !Info ^ mki_dep_file_index_map := Map
    ).

%---------------------%

dependency_files_to_index_set(DepFiles, DepIndexSet, !Info) :-
    list.foldl2(dependency_files_to_index_set_2, DepFiles,
        init, DepIndexSet, !Info).

:- pred dependency_files_to_index_set_2(dependency_file::in,
    deps_set(dependency_file_index)::in, deps_set(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

dependency_files_to_index_set_2(DepFiles, !Set, !Info) :-
    dependency_file_to_index(DepFiles, DepIndex, !Info),
    insert(DepIndex, !Set).

%---------------------------------------------------------------------------%

:- pred index_to_dependency_file(make_info::in, dependency_file_index::in,
    dependency_file::out) is det.

index_to_dependency_file(Info, Index, DepFile) :-
    Info ^ mki_dep_file_index_map =
        dependency_file_index_map(_Forward, Reverse, _Size),
    Index = dependency_file_index(I),
    DepFile = version_array.lookup(Reverse, I).

%---------------------%

dependency_file_index_set_to_plain_set(Info, DepIndices, DepFiles) :-
    foldl(dependency_file_index_set_to_plain_set_2(Info), DepIndices,
        [], DepFilesList),
    DepFiles = set.list_to_set(DepFilesList).

:- pred dependency_file_index_set_to_plain_set_2(make_info::in,
    dependency_file_index::in,
    list(dependency_file)::in, list(dependency_file)::out) is det.

dependency_file_index_set_to_plain_set_2(Info, DepIndex, List0, List) :-
    index_to_dependency_file(Info, DepIndex, DepFile),
    List = [DepFile | List0].

%---------------------------------------------------------------------------%
:- end_module make.deps_set.
%---------------------------------------------------------------------------%
