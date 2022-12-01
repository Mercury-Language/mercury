%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.dependencies.m.
% Author: stayl.
%
% Code to find the dependencies for a particular target,
% e.g. module.c depends on module.m, import.int, etc.
%
%---------------------------------------------------------------------------%

:- module make.dependencies.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_succeeded.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.deps_set.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

    % find_module_deps(Globals, ModuleIndex, Succeeded, Deps, !Info, !IO).
    %
    % The reason we don't return maybe(Deps) is that with `--keep-going'
    % we want to do as much work as possible.
    %
:- type find_module_deps(T) ==
    pred(globals, module_index, maybe_succeeded, deps_set(T),
        make_info, make_info, io, io).
:- inst find_module_deps ==
    (pred(in, in, out, out, in, out, di, uo) is det).

:- type find_module_deps_plain_set(T) ==
    pred(globals, module_index, maybe_succeeded, set(T),
        make_info, make_info, io, io).
:- inst find_module_deps_plain_set ==
    (pred(in, in, out, out, in, out, di, uo) is det).

:- type dependency_file
    --->    dep_target(target_file)
            % A target which could be made.

    ;       dep_file(file_name).
            % An ordinary file which `mmc --make' does not know how to rebuild.

    % Like dependency_file but refers to a module by index instead of by name,
    % which is more efficient when the name is not required.
    %
:- type dependency_file_with_module_index
    --->    dfmi_target(module_index, module_target_type)
    ;       dfmi_file(file_name).

    % Return a closure which will find the dependencies for a target type
    % given a module name.
    %
:- func target_dependencies(globals::in, module_target_type::in) =
    (find_module_deps(dependency_file_index)::out(find_module_deps)) is det.

%---------------------------------------------------------------------------%

    % Union the output set of dependencies for a given module
    % with the accumulated set. This is used with
    % foldl3_maybe_stop_at_error to iterate over a list of
    % module_names to find all target files for those modules.
    %
:- pred union_deps(find_module_deps(T)::in(find_module_deps), globals::in,
    module_index::in, maybe_succeeded::out, deps_set(T)::in, deps_set(T)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred union_deps_plain_set(
    find_module_deps_plain_set(T)::in(find_module_deps_plain_set),
    globals::in, module_index::in, maybe_succeeded::out,
    set(T)::in, set(T)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred deps_set_foldl3_maybe_stop_at_error_mi(maybe_keep_going::in,
    foldl3_pred_with_status(module_index, Acc, Info, IO)::
        in(foldl3_pred_with_status),
    globals::in, deps_set(module_index)::in, maybe_succeeded::out,
    Acc::in, Acc::out, Info::in, Info::out, IO::di, IO::uo) is det.
:- pred deps_set_foldl3_maybe_stop_at_error_fi(maybe_keep_going::in,
    foldl3_pred_with_status(dependency_file_index, Acc, Info, IO)::
        in(foldl3_pred_with_status),
    globals::in, deps_set(dependency_file_index)::in, maybe_succeeded::out,
    Acc::in, Acc::out, Info::in, Info::out, IO::di, IO::uo) is det.

    % Find all modules in the current directory which are reachable
    % (by import or include) from the given module.
    %
:- pred find_reachable_local_modules(globals::in, module_name::in,
    maybe_succeeded::out, set(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Remove all nested modules from a list of modules.
    %
:- pred remove_nested_modules(globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Find all modules in the current directory which are reachable (by import)
    % from the given module. Return a list of `--local-module-id' options
    % suitable for the command line.
    %
:- pred make_local_module_id_options(globals::in, module_name::in,
    maybe_succeeded::out, list(string)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred dependency_status(globals::in, dependency_file::in,
    dependency_status::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type dependencies_result
    --->    deps_up_to_date
    ;       deps_out_of_date
    ;       deps_error.

    % check_dependencies(Globals, TargetFileName, TargetFileTimestamp,
    %   BuildDepsSucceeded, Dependencies, Result, !IO)
    %
    % Check that all the dependency targets are up-to-date.
    %
:- pred check_dependencies(globals::in, file_name::in,
    maybe_error(timestamp)::in, maybe_succeeded::in, list(dependency_file)::in,
    dependencies_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % check_dependencies(Globals, TargetFileName, TargetFileTimestamp,
    %   BuildDepsSucceeded, Dependencies, Result, !IO)
    %
    % Check that all the dependency files are up-to-date.
    %
:- pred check_dependency_timestamps(globals::in, file_name::in,
    maybe_error(timestamp)::in, maybe_succeeded::in, list(File)::in,
    pred(File, io, io)::(pred(in, di, uo) is det),
    list(maybe_error(timestamp))::in, dependencies_result::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type cached_direct_imports.
:- func init_cached_direct_imports = cached_direct_imports.

:- type cached_indirect_imports.
:- func init_cached_indirect_imports = cached_indirect_imports.

:- type cached_transitive_foreign_imports.
:- func init_cached_transitive_foreign_imports =
    cached_transitive_foreign_imports.

:- type cached_transitive_dependencies.
:- func init_cached_transitive_dependencies = cached_transitive_dependencies.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.options.
:- import_module make.module_dep_file.
:- import_module make.util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.prog_data_foreign.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module sparse_bitset.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

target_dependencies(Globals, Target) = FindDeps :-
    (
        ( Target = module_target_source
        ; Target = module_target_track_flags
        ),
        FindDeps = no_deps
    ;
        ( Target = module_target_int0
        ; Target = module_target_int1
        ; Target = module_target_int2
        ),
        FindDeps = interface_file_dependencies
    ;
        Target = module_target_int3,
        FindDeps = module_target_source `of` self
    ;
        ( Target = module_target_c_code
        ; Target = module_target_csharp_code
        ; Target = module_target_java_code
        ; Target = module_target_errors
        ),
        FindDeps = compiled_code_dependencies(Globals)
    ;
        Target = module_target_c_header(_),
        FindDeps = target_dependencies(Globals, module_target_c_code)
    ;
        Target = module_target_java_class_code,
        FindDeps = module_target_java_code `of` self
    ;
        ( Target = module_target_foreign_object(PIC, _)
        ; Target = module_target_fact_table_object(PIC, _)
        ),
        FindDeps = get_foreign_deps(Globals, PIC)
    ;
        Target = module_target_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),
        TargetCode = target_to_module_target_code(CompilationTarget, PIC),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),

        % For --highlevel-code, the `.c' file will #include the header file
        % for all imported modules.
        ( if
            CompilationTarget = target_c,
            HighLevelCode = yes
        then
            HeaderDeps = combine_deps_list([
                module_target_c_header(header_mih) `of` direct_imports,
                module_target_c_header(header_mih) `of` indirect_imports,
                module_target_c_header(header_mih) `of` ancestors,
                module_target_c_header(header_mih) `of` intermod_imports
            ])
        else
            HeaderDeps = no_deps
        ),
        FindDeps = combine_deps_list([
            TargetCode `of` self,
            module_target_c_header(header_mh) `of` foreign_imports,
            HeaderDeps
        ])
    ;
        ( Target = module_target_opt
        ; Target = module_target_xml_doc
        ),
        FindDeps = combine_deps_list([
            module_target_source `of` self,
            module_target_int0 `of` ancestors,
            module_target_int1 `of` non_intermod_direct_imports,
            module_target_int2 `of` non_intermod_indirect_imports
        ])
    ;
        Target = module_target_analysis_registry,
        FindDeps = combine_deps_list([
            module_target_source `of` self,
            module_target_int0 `of` ancestors,
            module_target_int1 `of` non_intermod_direct_imports,
            module_target_int2 `of` non_intermod_indirect_imports,
            module_target_opt `of` direct_imports,
            module_target_opt `of` indirect_imports,
            module_target_opt `of` intermod_imports
        ])
    ).

:- func get_foreign_deps(globals::in, pic::in) =
    (find_module_deps(dependency_file_index)::out(find_module_deps)) is det.

get_foreign_deps(Globals, PIC) = Deps :-
    globals.get_target(Globals, CompilationTarget),
    TargetCode = target_to_module_target_code(CompilationTarget, PIC),
    Deps = combine_deps_list([
        TargetCode `of` self
    ]).

:- func target_to_module_target_code(compilation_target, pic)
    = module_target_type.

target_to_module_target_code(_CompilationTarget, _PIC) = TargetCode :-
    % XXX it looks wrong to be returning module_target_c_code for
    % all compilation targets.
    TargetCode = module_target_c_code.

:- func interface_file_dependencies =
    (find_module_deps(dependency_file_index)::out(find_module_deps)) is det.

interface_file_dependencies =
    combine_deps_list([
        module_target_source `of` self,
        module_target_int0 `of` ancestors,
        module_target_int3 `of` direct_imports,
        module_target_int3 `of` indirect_imports
    ]).

:- func compiled_code_dependencies(globals::in) =
    (find_module_deps(dependency_file_index)::out(find_module_deps)) is det.

compiled_code_dependencies(Globals) = Deps :-
    % We build up Deps in stages.
    % XXX The *order* of the dependencies this computes looks wrong to me (zs).
    % For example, why does Deps call for imported modules' .opt files
    % to be built before their .int files? The dependencies of the .opt files
    % won't let them be built before the files *they* depend on are ready,
    % but still ...

    % Stage 0: dependencies on flags.
    globals.lookup_bool_option(Globals, track_flags, TrackFlags),
    (
        TrackFlags = yes,
        Deps0 = module_target_track_flags `of` self
    ;
        TrackFlags = no,
        Deps0 = no_deps
    ),

    % Stage 1: dependencies on the source file, and on the fact table files,
    % foreign language files and Mercury interface files it imports.
    Deps1 = combine_deps_list([
        module_target_source `of` self,
        fact_table_files `files_of` self,
        foreign_include_files `files_of` self,
        module_target_int1 `of` self,
        module_target_int1 `of` ancestors,
        map_find_module_deps_fi(self, imports),
        Deps0
    ]),

    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    AnyIntermod = bool.or(IntermodOpt, IntermodAnalysis),

    % Stage 2: dependencies on optimization files.
    (
        AnyIntermod = yes,
        Deps2 = combine_deps_list([
            module_target_opt `of` self,
            module_target_opt `of` intermod_imports,
            map_find_module_deps_fi(
                map_find_module_deps_mi(intermod_imports, ancestors),
                imports),
            Deps1
        ])
    ;
        AnyIntermod = no,
        Deps2 = Deps1
    ),

    % Stage 3: dependencies on analysis result files.
    (
        IntermodAnalysis = yes,
        Deps = combine_deps_list([
            module_target_analysis_registry `of` self,
            module_target_analysis_registry `of` direct_imports,
            Deps2
        ])
    ;
        IntermodAnalysis = no,
        Deps = Deps2
    ).

:- func imports =
    (find_module_deps(dependency_file_index)::out(find_module_deps)) is det.

imports = combine_deps_list([
        module_target_int0 `of` ancestors,
        module_target_int1 `of` direct_imports,
        module_target_int2 `of` indirect_imports
    ]).

%---------------------------------------------------------------------------%

    % TargetType `of` F is function that returns the set of TargetType targets
    % based on the modules generated by F.
    %
    % e.g. module_target_int0 `of` ancestors takes a module and returns the
    % set of .int0 targets for the module's ancestor modules.
    %
:- func of(module_target_type, find_module_deps(module_index)) =
    find_module_deps(dependency_file_index).
:- mode of(in, in(find_module_deps)) = out(find_module_deps) is det.

of(FileType, FindDeps) =
    of_2(FileType, FindDeps).

:- pred of_2(module_target_type::in,
    find_module_deps(module_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

of_2(FileType, FindDeps, Globals, ModuleIndex, Succeeded, TargetFiles,
        !Info, !IO) :-
    FindDeps(Globals, ModuleIndex, Succeeded, ModuleIndexes, !Info, !IO),
    foldl2(of_3(FileType), ModuleIndexes, [], TargetFileIndexes, !Info),
    list_to_set(TargetFileIndexes, TargetFiles).

:- pred of_3(module_target_type::in, module_index::in,
    list(dependency_file_index)::in, list(dependency_file_index)::out,
    make_info::in, make_info::out) is det.

of_3(FileType, ModuleIndex, !List, !Info) :-
    TargetFile = dfmi_target(ModuleIndex, FileType),
    dependency_file_to_index(TargetFile, TargetFileIndex, !Info),
    !:List = [TargetFileIndex | !.List].

:- func files_of(find_module_deps_plain_set(dependency_file),
    find_module_deps(module_index)) = find_module_deps(dependency_file_index).
:- mode files_of(in(find_module_deps_plain_set), in(find_module_deps))
    = out(find_module_deps) is det.

files_of(FindFiles, FindDeps) =
    files_of_2(FindFiles, FindDeps).

:- pred files_of_2(
    find_module_deps_plain_set(dependency_file)::
        in(find_module_deps_plain_set),
    find_module_deps(module_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

files_of_2(FindFiles, FindDeps, Globals, ModuleIndex, Succeeded, DepIndices,
        !Info, !IO) :-
    KeepGoing = !.Info ^ mki_keep_going,
    FindDeps(Globals, ModuleIndex, Succeeded1, ModuleIndices, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        DepIndices = init
    else
        deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
            union_deps_plain_set(FindFiles),
            Globals, ModuleIndices, Succeeded2, init, FileNames, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2,
        dependency_files_to_index_set(set.to_sorted_list(FileNames),
            DepIndices, !Info)
    ).

:- pred map_find_module_deps_mi(
    find_module_deps(module_index)::in(find_module_deps),
    find_module_deps(module_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

map_find_module_deps_mi(FindDeps1, FindDeps2, Globals, ModuleIndex, Succeeded,
        Result, !Info, !IO) :-
    KeepGoing = !.Info ^ mki_keep_going,
    FindDeps1(Globals, ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        Result = init
    else
        deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
            union_deps(FindDeps2), Globals, Modules1, Succeeded2,
            init, Result, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ).

:- pred map_find_module_deps_fi(
    find_module_deps(module_index)::in(find_module_deps),
    find_module_deps(dependency_file_index)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out,
    deps_set(dependency_file_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

map_find_module_deps_fi(FindDeps1, FindDeps2, Globals, ModuleIndex, Succeeded,
        Result, !Info, !IO) :-
    KeepGoing = !.Info ^ mki_keep_going,
    FindDeps1(Globals, ModuleIndex, Succeeded1, Modules1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        Result = init
    else
        deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
            union_deps(FindDeps2), Globals, Modules1, Succeeded2,
            init, Result, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ).

%---------------------------------------------------------------------------%

:- pred no_deps(globals::in, module_index::in, maybe_succeeded::out,
    deps_set(T)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

no_deps(_, _, succeeded, init, !Info, !IO).

:- pred self(globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

self(_Globals, ModuleIndex, succeeded, make_singleton_set(ModuleIndex),
    !Info, !IO).

:- pred ancestors(globals::in, module_index::in, maybe_succeeded::out,
    deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

ancestors(_Globals, ModuleIndex, succeeded, AncestorIndices, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    Ancestors = get_ancestors(ModuleName),
    module_names_to_index_set(Ancestors, AncestorIndices, !Info).

%---------------------------------------------------------------------------%

:- pred direct_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

direct_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    CachedDirectImports0 = !.Info ^ mki_cached_direct_imports,
    ( if map.search(CachedDirectImports0, ModuleIndex, Result0) then
        Result0 = deps_result(Succeeded, Modules)
    else
        KeepGoing = !.Info ^ mki_keep_going,
        non_intermod_direct_imports(Globals, ModuleIndex, Succeeded0, Modules0,
            !Info, !IO),
        ( if
            Succeeded0 = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            Succeeded = did_not_succeed,
            Modules = init
        else
            % We also read `.int' files for the modules for which we read
            % `.opt' files, and for the modules imported by those modules.
            intermod_imports(Globals, ModuleIndex, Succeeded1, IntermodModules,
                !Info, !IO),
            ( if
                Succeeded1 = did_not_succeed,
                KeepGoing = do_not_keep_going
            then
                Succeeded = did_not_succeed,
                Modules = init
            else
                deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
                    union_deps(non_intermod_direct_imports), Globals,
                    IntermodModules, Succeeded2,
                    union(Modules0, IntermodModules), Modules1,
                    !Info, !IO),
                Succeeded = Succeeded0 `and` Succeeded1 `and` Succeeded2,
                Modules = delete(Modules1, ModuleIndex)
            )
        ),
        Result = deps_result(Succeeded, Modules),
        CachedDirectImports1 = !.Info ^ mki_cached_direct_imports,
        map.det_insert(ModuleIndex, Result,
            CachedDirectImports1, CachedDirectImports),
        !Info ^ mki_cached_direct_imports := CachedDirectImports
    ).

    % Return the modules for which `.int' files are read in a compilation
    % which does not use `--intermodule-optimization'.
    %
:- pred non_intermod_direct_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

non_intermod_direct_imports(Globals, ModuleIndex, Succeeded, Modules,
        !Info, !IO) :-
    CachedNonIntermodDirectImports0 =
        !.Info ^ mki_cached_non_intermod_direct_imports,
    ( if map.search(CachedNonIntermodDirectImports0, ModuleIndex, Result0) then
        Result0 = deps_result(Succeeded, Modules)
    else
        non_intermod_direct_imports_2(Globals, ModuleIndex, Succeeded, Modules,
            !Info, !IO),
        Result = deps_result(Succeeded, Modules),
        CachedNonIntermodDirectImports1 =
            !.Info ^ mki_cached_non_intermod_direct_imports,
        map.det_insert(ModuleIndex, Result,
            CachedNonIntermodDirectImports1, CachedNonIntermodDirectImports),
        !Info ^ mki_cached_non_intermod_direct_imports
            := CachedNonIntermodDirectImports
    ).

:- pred non_intermod_direct_imports_2(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

non_intermod_direct_imports_2(Globals, ModuleIndex, Succeeded, Modules,
        !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),

        % Find the direct imports of this module (modules for which we will
        % read the `.int' files).
        %
        % Note that we need to do this both for the imports of this module
        % and for the imports of its ancestors. This is because if this module
        % is a submodule, then it may depend on things imported only by its
        % ancestors.
        %
        module_dep_info_get_int_deps(ModuleDepInfo, IntDeps),
        module_dep_info_get_imp_deps(ModuleDepInfo, ImpDeps),
        module_names_to_index_set(set.to_sorted_list(IntDeps), DepsInt, !Info),
        module_names_to_index_set(set.to_sorted_list(ImpDeps), DepsImp, !Info),
        Modules0 = sparse_bitset.union(DepsInt, DepsImp),
        (
            ModuleName = qualified(ParentModule, _),
            module_name_to_index(ParentModule, ParentIndex, !Info),
            non_intermod_direct_imports(Globals, ParentIndex, Succeeded,
                ParentImports, !Info, !IO),
            Modules = union(ParentImports, Modules0)
        ;
            ModuleName = unqualified(_),
            Succeeded = succeeded,
            Modules = Modules0
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        Modules = init
    ).

%---------------------------------------------------------------------------%

    % Return the list of modules for which we should read `.int2' files.
    %
:- pred indirect_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

indirect_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    CachedIndirectImports0 = !.Info ^ mki_cached_indirect_imports,
    ( if map.search(CachedIndirectImports0, ModuleIndex, CachedResult) then
        CachedResult = deps_result(Succeeded, Modules)
    else
        indirect_imports_2(Globals, direct_imports, ModuleIndex,
            Succeeded, Modules, !Info, !IO),
        Result = deps_result(Succeeded, Modules),
        CachedIndirectImports1 = !.Info ^ mki_cached_indirect_imports,
        map.det_insert(ModuleIndex, Result,
            CachedIndirectImports1, CachedIndirectImports),
        !Info ^ mki_cached_indirect_imports := CachedIndirectImports
    ).

    % Return the list of modules for which we should read `.int2' files,
    % ignoring those which need to be read as a result of importing modules
    % imported by a `.opt' file.
    %
:- pred non_intermod_indirect_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

non_intermod_indirect_imports(Globals, ModuleIndex, Succeeded, Modules,
        !Info, !IO) :-
    indirect_imports_2(Globals, non_intermod_direct_imports, ModuleIndex,
        Succeeded, Modules, !Info, !IO).

:- pred indirect_imports_2(globals::in,
    find_module_deps(module_index)::in(find_module_deps),
    module_index::in, maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

indirect_imports_2(Globals, FindDirectImports, ModuleIndex, Succeeded,
        IndirectImports, !Info, !IO) :-
    FindDirectImports(Globals, ModuleIndex, DirectSucceeded, DirectImports,
        !Info, !IO),
    % XXX The original version of this code by stayl had the line assigning
    % to KeepGoing textually *before* the call to FindDirectImports, but
    % looked up the keep_going in the version of !Info *after* that call.
    KeepGoing = !.Info ^ mki_keep_going,
    ( if
        DirectSucceeded = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        IndirectImports = init
    else
        deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
            union_deps(find_transitive_implementation_imports), Globals,
            DirectImports, IndirectSucceeded,
            init, IndirectImports0, !Info, !IO),
        IndirectImports = difference(
            delete(IndirectImports0, ModuleIndex),
            DirectImports),
        Succeeded = DirectSucceeded `and` IndirectSucceeded
    ).

%---------------------------------------------------------------------------%

    % Return the list of modules for which we should read `.opt' files.
    %
:- pred intermod_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

intermod_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    globals.get_any_intermod(Globals, AnyIntermod),
    (
        AnyIntermod = yes,
        globals.lookup_bool_option(Globals, read_opt_files_transitively,
            Transitive),
        (
            Transitive = yes,
            find_transitive_implementation_imports(Globals, ModuleIndex,
                Succeeded, Modules, !Info, !IO)
        ;
            Transitive = no,
            non_intermod_direct_imports(Globals, ModuleIndex, Succeeded,
                Modules, !Info, !IO)
        )
    ;
        AnyIntermod = no,
        Succeeded = succeeded,
        Modules = init
    ).

%---------------------------------------------------------------------------%

:- pred foreign_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

foreign_imports(Globals, ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    % The object file depends on the header files for the modules
    % mentioned in `:- pragma foreign_import_module' declarations
    % in the current module and the `.opt' files it imports.

    globals.get_backend_foreign_languages(Globals, Languages),
    intermod_imports(Globals, ModuleIndex, IntermodSucceeded, IntermodModules,
        !Info, !IO),
    deps_set_foldl3_maybe_stop_at_error_mi(!.Info ^ mki_keep_going,
        union_deps(find_module_foreign_imports(set.list_to_set(Languages))),
        Globals, insert(IntermodModules, ModuleIndex),
        ForeignSucceeded, init, Modules, !Info, !IO),
    Succeeded = IntermodSucceeded `and` ForeignSucceeded.

:- pred find_module_foreign_imports(set(foreign_language)::in,
    globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_module_foreign_imports(Languages, Globals, ModuleIndex, Succeeded,
        ForeignModules, !Info, !IO) :-
    % Languages should be constant for the duration of the process,
    % so is unnecessary to include in the cache key.
    CachedForeignImports0 = !.Info ^ mki_cached_transitive_foreign_imports,
    ( if map.search(CachedForeignImports0, ModuleIndex, CachedResult) then
        CachedResult = deps_result(Succeeded, ForeignModules)
    else
        find_transitive_implementation_imports(Globals, ModuleIndex,
            Succeeded0, ImportedModules, !Info, !IO),
        (
            Succeeded0 = succeeded,
            deps_set_foldl3_maybe_stop_at_error_mi(!.Info ^ mki_keep_going,
                union_deps(find_module_foreign_imports_uncached(Languages)),
                Globals, insert(ImportedModules, ModuleIndex),
                Succeeded, init, ForeignModules, !Info, !IO),
            Result = deps_result(Succeeded, ForeignModules),
            CachedForeignImports1 =
                !.Info ^ mki_cached_transitive_foreign_imports,
            map.det_insert(ModuleIndex, Result,
                CachedForeignImports1, CachedForeignImports),
            !Info ^ mki_cached_transitive_foreign_imports :=
                CachedForeignImports
        ;
            Succeeded0 = did_not_succeed,
            Succeeded = did_not_succeed,
            ForeignModules = init
        )
    ).

:- pred find_module_foreign_imports_uncached(set(foreign_language)::in,
    globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_module_foreign_imports_uncached(Languages, Globals, ModuleIndex,
        Succeeded, ForeignModules, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_fims(ModuleDepInfo, FIMSpecs),
        ForLangsPred =
            ( pred(fim_spec(Lang, Module)::in, Module::out) is semidet :-
                set.contains(Languages, Lang)
            ),
        set.filter_map(ForLangsPred, FIMSpecs, ForeignModuleNameSet),
        module_names_to_index_set(set.to_sorted_list(ForeignModuleNameSet),
            ForeignModules, !Info),
        Succeeded = succeeded
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        ForeignModules = init,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred fact_table_files(globals::in, module_index::in,
    maybe_succeeded::out, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

fact_table_files(Globals, ModuleIndex, Succeeded, Files, !Info, !IO) :-
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        Succeeded = succeeded,
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFiles),
        Files = set.map((func(File) = dep_file(File)), FactTableFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        Files = init
    ).

%---------------------------------------------------------------------------%

:- pred foreign_include_files(globals::in, module_index::in,
    maybe_succeeded::out, set(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

foreign_include_files(Globals, ModuleIndex, Succeeded, Files, !Info, !IO) :-
    globals.get_backend_foreign_languages(Globals, Languages),
    module_index_to_name(!.Info, ModuleIndex, ModuleName),
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        Succeeded = succeeded,
        module_dep_info_get_source_file_name(ModuleDepInfo, SourceFileName),
        module_dep_info_get_foreign_include_files(ModuleDepInfo,
            ForeignIncludeFiles),
        FilesList = get_foreign_include_files(set.list_to_set(Languages),
            SourceFileName, set.to_sorted_list(ForeignIncludeFiles)),
        Files = set.list_to_set(FilesList)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed,
        Files = set.init
    ).

:- func get_foreign_include_files(set(foreign_language), file_name,
    list(foreign_include_file_info)) = list(dependency_file).

get_foreign_include_files(Languages, SourceFileName, ForeignIncludes)
        = Files :-
    list.filter_map(get_foreign_include_files_2(Languages, SourceFileName),
        ForeignIncludes, Files).

:- pred get_foreign_include_files_2(set(foreign_language)::in, file_name::in,
    foreign_include_file_info::in, dependency_file::out) is semidet.

get_foreign_include_files_2(Languages, SourceFileName, ForeignInclude, File) :-
    ForeignInclude = foreign_include_file_info(Language, IncludeFileName),
    set.member(Language, Languages),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
    File = dep_file(IncludePath).

%---------------------------------------------------------------------------%

:- type deps_result(T)
    --->    deps_result(
                dr_success  :: maybe_succeeded,
                dr_set      :: deps_set(T)
            ).

:- type module_deps_result == deps_result(module_index).

union_deps(FindDeps, Globals, ModuleIndex, Succeeded, Deps0, Deps,
        !Info, !IO) :-
    FindDeps(Globals, ModuleIndex, Succeeded, Deps1, !Info, !IO),
    Deps = union(Deps0, Deps1).

union_deps_plain_set(FindDeps, Globals, ModuleName, Succeeded, Deps0, Deps,
        !Info, !IO) :-
    FindDeps(Globals, ModuleName, Succeeded, Deps1, !Info, !IO),
    Deps = set.union(Deps0, Deps1).

    % Note that we go to some effort in this module to stop dependency
    % calculation as soon as possible if there are errors.
    % This is important, because the calls to get_module_dependencies from
    % the dependency calculation predicates can result in every module in
    % the program being read.
    %
:- func combine_deps(
    find_module_deps(T)::in(find_module_deps),
    find_module_deps(T)::in(find_module_deps)) =
    (find_module_deps(T)::out(find_module_deps)) is det.

combine_deps(FindDeps1, FindDeps2) =
    combine_deps_2(FindDeps1, FindDeps2).

:- pred combine_deps_2(
    find_module_deps(T)::in(find_module_deps),
    find_module_deps(T)::in(find_module_deps),
    globals::in, module_index::in, maybe_succeeded::out, deps_set(T)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

combine_deps_2(FindDeps1, FindDeps2, Globals, ModuleIndex, Succeeded, Deps,
        !Info, !IO) :-
    FindDeps1(Globals, ModuleIndex, Succeeded1, Deps1, !Info, !IO),
    ( if
        Succeeded1 = did_not_succeed,
        !.Info ^ mki_keep_going = do_not_keep_going
    then
        Succeeded = did_not_succeed,
        Deps = Deps1
    else
        FindDeps2(Globals, ModuleIndex, Succeeded2, Deps2, !Info, !IO),
        Succeeded = Succeeded1 `and` Succeeded2,
        Deps = union(Deps1, Deps2)
    ).

:- func combine_deps_list(list(
    find_module_deps(T))::in(list_skel(find_module_deps))) =
    (find_module_deps(T)::out(find_module_deps)) is det.

combine_deps_list([]) = no_deps.
combine_deps_list([FindDeps]) = FindDeps.
combine_deps_list([FindDeps1, FindDeps2 | FindDepsTail]) =
    combine_deps(FindDeps1, combine_deps_list([FindDeps2 | FindDepsTail])).

deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing, P, Globals, Ts,
        Succeeded, !Acc, !Info, !IO) :-
    foldl3_maybe_stop_at_error_mi(KeepGoing, P, Globals, to_sorted_list(Ts),
        Succeeded, !Acc, !Info, !IO).

deps_set_foldl3_maybe_stop_at_error_fi(KeepGoing, P, Globals, Ts,
        Succeeded, !Acc, !Info, !IO) :-
    foldl3_maybe_stop_at_error_fi(KeepGoing, P, Globals, to_sorted_list(Ts),
        Succeeded, !Acc, !Info, !IO).

%---------------------------------------------------------------------------%

find_reachable_local_modules(Globals, ModuleName, Succeeded, Modules,
        !Info, !IO) :-
    module_name_to_index(ModuleName, ModuleIndex, !Info),
    find_transitive_module_dependencies(Globals, all_dependencies,
        local_module, ModuleIndex, Succeeded, Modules0, !Info, !IO),
    module_index_set_to_plain_set(!.Info, Modules0, Modules).

:- pred find_transitive_implementation_imports(globals::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_transitive_implementation_imports(Globals, ModuleIndex,
        Succeeded, Modules, !Info, !IO) :-
    find_transitive_module_dependencies(Globals, all_imports, any_module,
        ModuleIndex, Succeeded, Modules0, !Info, !IO),
    Modules = insert(Modules0, ModuleIndex).

:- pred find_transitive_module_dependencies(globals::in,
    transitive_dependencies_type::in, module_locn::in, module_index::in,
    maybe_succeeded::out, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_transitive_module_dependencies(Globals, DependenciesType, ModuleLocn,
        ModuleIndex, Succeeded, Modules, !Info, !IO) :-
    DepsRoot = transitive_dependencies_root(ModuleIndex, DependenciesType,
        ModuleLocn),
    CachedTransDeps0 = !.Info ^ mki_cached_transitive_dependencies,
    ( if map.search(CachedTransDeps0, DepsRoot, Result0) then
        Result0 = deps_result(Succeeded, Modules)
    else
        KeepGoing = !.Info ^ mki_keep_going,
        find_transitive_module_dependencies_2(KeepGoing, DependenciesType,
            ModuleLocn, Globals, ModuleIndex, Succeeded, init, Modules,
            !Info, !IO),
        Result = deps_result(Succeeded, Modules),
        CachedTransDeps1 = !.Info ^ mki_cached_transitive_dependencies,
        map.det_insert(DepsRoot, Result, CachedTransDeps1, CachedTransDeps),
        !Info ^ mki_cached_transitive_dependencies := CachedTransDeps
    ).

:- pred find_transitive_module_dependencies_2(maybe_keep_going::in,
    transitive_dependencies_type::in, module_locn::in, globals::in,
    module_index::in, maybe_succeeded::out,
    deps_set(module_index)::in, deps_set(module_index)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_transitive_module_dependencies_2(KeepGoing, DependenciesType, ModuleLocn,
        Globals, ModuleIndex, Succeeded, Modules0, Modules, !Info, !IO) :-
    ( if
        member(ModuleIndex, Modules0)
    then
        Succeeded = succeeded,
        Modules = Modules0
    else if
        DepsRoot = transitive_dependencies_root(ModuleIndex,
            DependenciesType, ModuleLocn),
        map.search(!.Info ^ mki_cached_transitive_dependencies, DepsRoot,
            Result0)
    then
        Result0 = deps_result(Succeeded, Modules1),
        Modules = union(Modules0, Modules1)
    else
        module_index_to_name(!.Info, ModuleIndex, ModuleName),
        get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
            !Info, !IO),
        (
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
            ( if
                (
                    ModuleLocn = any_module
                ;
                    ModuleLocn = local_module,
                    ModuleDir = dir.this_directory
                )
            then
                module_dep_info_get_fims(ModuleDepInfo, FIMSpecs),
                module_dep_info_get_module_name(ModuleDepInfo, MDI_ModuleName),
                expect(unify(ModuleName, MDI_ModuleName), $pred,
                    "ModuleName != MDI_ModuleName"),
                Ancestors = get_ancestors_set(ModuleName),
                module_dep_info_get_children(ModuleDepInfo, Children),
                module_dep_info_get_int_deps(ModuleDepInfo, IntDeps),
                module_dep_info_get_imp_deps(ModuleDepInfo, ImpDeps),
                (
                    % Ancestors don't need to be considered here.
                    % Anywhere the interface of the child module is needed,
                    % the ancestors must also have been imported.
                    DependenciesType = interface_imports,
                    ImportsToCheck = IntDeps,
                    IncludesToCheck = set.init
                ;
                    DependenciesType = all_dependencies,
                    set.map((pred(fim_spec(_, Mod)::in, Mod::out) is det),
                        FIMSpecs, ForeignDeps),
                    ImportsToCheck = set.union_list([
                        Ancestors, IntDeps, ImpDeps, ForeignDeps
                    ]),
                    IncludesToCheck = Children
                ;
                    DependenciesType = all_imports,
                    set.map((pred(fim_spec(_, Mod)::in, Mod::out) is det),
                        FIMSpecs, ForeignDeps),
                    ImportsToCheck = set.union_list([
                        Ancestors, IntDeps, ImpDeps, ForeignDeps
                    ]),
                    IncludesToCheck = set.init
                ),
                module_names_to_index_set(set.to_sorted_list(ImportsToCheck),
                    ImportsToCheckSet, !Info),
                module_names_to_index_set(set.to_sorted_list(IncludesToCheck),
                    IncludesToCheckSet, !Info),
                Modules1 = insert(Modules0, ModuleIndex),
                % XXX The pattern of use of the mki_importing_module field
                % here suggest that it should not be a field of make_info
                % at all, but rather a separate parameter of this predicate.
                OldImportingModule = !.Info ^ mki_importing_module,
                !Info ^ mki_importing_module := yes(ioi_import(ModuleName)),
                deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
                    find_transitive_module_dependencies_2(KeepGoing,
                        DependenciesType, ModuleLocn),
                    Globals, ImportsToCheckSet, SucceededA, Modules1, Modules2,
                    !Info, !IO),
                !Info ^ mki_importing_module := yes(ioi_include(ModuleName)),
                deps_set_foldl3_maybe_stop_at_error_mi(KeepGoing,
                    find_transitive_module_dependencies_2(KeepGoing,
                        DependenciesType, ModuleLocn),
                    Globals, IncludesToCheckSet, SucceededB, Modules2, Modules,
                    !Info, !IO),
                !Info ^ mki_importing_module := OldImportingModule,
                Succeeded = SucceededA `and` SucceededB
            else
                Succeeded = succeeded,
                Modules = Modules0
            )
        ;
            MaybeModuleDepInfo = no_module_dep_info,
            Succeeded = did_not_succeed,
            Modules = Modules0
        )
    ).

%---------------------------------------------------------------------------%

remove_nested_modules(Globals, Modules0, Modules, !Info, !IO) :-
    list.foldl3(collect_nested_modules(Globals), Modules0,
        set.init, NestedModules, !Info, !IO),
    list.negated_filter(set.contains(NestedModules), Modules0, Modules).

:- pred collect_nested_modules(globals::in, module_name::in,
    set(module_name)::in, set(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

collect_nested_modules(Globals, ModuleName, !NestedModules, !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        NestedSubModules = get_nested_children_of_top_module(MaybeTopModule),
        set.union(NestedSubModules, !NestedModules)
    ;
        MaybeModuleDepInfo = no_module_dep_info
    ).

%---------------------------------------------------------------------------%

make_local_module_id_options(Globals, ModuleName, Succeeded, Options,
        !Info, !IO) :-
    find_reachable_local_modules(Globals, ModuleName, Succeeded, LocalModules,
        !Info, !IO),
    set.fold(make_local_module_id_option, LocalModules, [], Options).

:- pred make_local_module_id_option(module_name::in, list(string)::in,
    list(string)::out) is det.

make_local_module_id_option(ModuleName, Opts0, Opts) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    Opts = ["--local-module-id", ModuleNameStr | Opts0].

:- pred make_write_target_dependency_status(globals::in,
    pair(dependency_file, dependency_status)::in, io::di, io::uo) is det.

make_write_target_dependency_status(Globals, DepTarget - DepStatus, !IO) :-
    (
        DepStatus = deps_status_not_considered,
        DepStatusStr = "deps_status_not_considered"
    ;
        DepStatus = deps_status_being_built,
        DepStatusStr = "deps_status_being_built"
    ;
        DepStatus = deps_status_up_to_date,
        DepStatusStr = "deps_status_up_to_date"
    ;
        DepStatus = deps_status_error,
        DepStatusStr = "deps_status_error"
    ),
    make_write_dependency_file(Globals, DepTarget, !IO),
    io.write_string(" - ", !IO),
    io.write_string(DepStatusStr, !IO).

%---------------------------------------------------------------------------%

dependency_status(Globals, Dep, Status, !Info, !IO) :-
    (
        Dep = dep_file(_FileName),
        DepStatusMap0 = !.Info ^ mki_dependency_status,
        ( if version_hash_table.search(DepStatusMap0, Dep, StatusPrime) then
            Status = StatusPrime
        else
            get_dependency_timestamp(Globals, Dep, MaybeTimestamp, !Info, !IO),
            (
                MaybeTimestamp = ok(_),
                Status = deps_status_up_to_date
            ;
                MaybeTimestamp = error(Error),
                Status = deps_status_error,
                io.format("** Error: %s\n", [s(Error)], !IO)
            ),
            version_hash_table.det_insert(Dep, Status,
                DepStatusMap0, DepStatusMap),
            !Info ^ mki_dependency_status := DepStatusMap
        )
    ;
        Dep = dep_target(Target),
        Target = target_file(ModuleName, FileType),
        ( if
            ( FileType = module_target_source
            ; FileType = module_target_track_flags
            )
        then
            % Source files are always up-to-date.
            % .track_flags should already have been made, if required,
            % so are also up-to-date.
            ModuleTarget = module_target(module_target_source),
            maybe_warn_up_to_date_target(Globals,
                top_target_file(ModuleName, ModuleTarget), !Info, !IO),
            Status = deps_status_up_to_date
        else if
            DepStatusMap0 = !.Info ^ mki_dependency_status,
            version_hash_table.search(DepStatusMap0, Dep, StatusPrime)
        then
            Status = StatusPrime
        else
            get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
                !Info, !IO),
            (
                MaybeModuleDepInfo = no_module_dep_info,
                Status = deps_status_error
            ;
                MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
                module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
                ( if ModuleDir = dir.this_directory then
                    Status = deps_status_not_considered
                else
                    % Targets from libraries are always considered to be
                    % up-to-date if they exist.

                    get_target_timestamp(Globals, do_search, Target,
                        MaybeTimestamp, !Info, !IO),
                    (
                        MaybeTimestamp = ok(_),
                        Status = deps_status_up_to_date
                    ;
                        MaybeTimestamp = error(Error),
                        Status = deps_status_error,
                        get_make_target_file_name(Globals, Target,
                            TargetFileName, !IO),
                        string.format("** Error: file `%s' not found: %s\n",
                            [s(TargetFileName), s(Error)], ErrorMsg),
                        % Try to write this with one call to avoid
                        % interleaved output when doing parallel builds.
                        io.write_string(ErrorMsg, !IO)
                    )
                )
            ),
            DepStatusMap1 = !.Info ^ mki_dependency_status,
            version_hash_table.det_insert(Dep, Status,
                DepStatusMap1, DepStatusMap),
            !Info ^ mki_dependency_status := DepStatusMap
        )
    ).

check_dependencies(Globals, TargetFileName, MaybeTimestamp, BuildDepsSucceeded,
        DepFiles, DepsResult, !Info, !IO) :-
    list.map_foldl2(dependency_status(Globals), DepFiles, DepStatusList,
        !Info, !IO),
    assoc_list.from_corresponding_lists(DepFiles, DepStatusList, DepStatusAL),
    list.filter(
        ( pred((_ - DepStatus)::in) is semidet :-
            DepStatus \= deps_status_up_to_date
        ), DepStatusAL, UnbuiltDependencies),
    (
        UnbuiltDependencies = [_ | _],
        debug_make_msg(Globals,
            check_dependencies_debug_unbuilt(Globals, TargetFileName,
                UnbuiltDependencies),
            !IO),
        DepsResult = deps_error
    ;
        UnbuiltDependencies = [],
        debug_make_msg(Globals,
            io.write_string(TargetFileName ++ ": finished dependencies\n"),
            !IO),
        list.map_foldl2(get_dependency_timestamp(Globals), DepFiles,
            DepTimestamps, !Info, !IO),

        check_dependency_timestamps(Globals, TargetFileName, MaybeTimestamp,
            BuildDepsSucceeded, DepFiles, make_write_dependency_file(Globals),
            DepTimestamps, DepsResult, !IO)
    ).

:- pred check_dependencies_debug_unbuilt(globals::in, file_name::in,
    assoc_list(dependency_file, dependency_status)::in,
    io::di, io::uo) is det.

check_dependencies_debug_unbuilt(Globals, TargetFileName, UnbuiltDependencies,
        !IO) :-
    io.write_string(TargetFileName, !IO),
    io.write_string(": dependencies could not be built.\n\t", !IO),
    io.write_list(UnbuiltDependencies, ",\n\t",
        make_write_target_dependency_status(Globals), !IO),
    io.nl(!IO).

:- pred check_dependencies_timestamps_write_missing_deps(file_name::in,
    maybe_succeeded::in,
    list(File)::in, pred(File, io, io)::(pred(in, di, uo) is det),
    list(maybe_error(timestamp))::in, io::di, io::uo) is det.

check_dependencies_timestamps_write_missing_deps(TargetFileName,
        BuildDepsSucceeded, DepFiles, WriteDepFile, DepTimestamps, !IO) :-
    assoc_list.from_corresponding_lists(DepFiles, DepTimestamps,
        DepTimestampAL),
    list.filter_map(
        ( pred(Pair::in, DepFile::out) is semidet :-
            Pair = DepFile - error(_)
        ), DepTimestampAL, ErrorDeps0),
    list.sort(ErrorDeps0, ErrorDeps),
    io.write_string("** dependencies for `", !IO),
    io.write_string(TargetFileName, !IO),
    io.write_string("' do not exist: ", !IO),
    io.write_list(ErrorDeps, ", ", WriteDepFile, !IO),
    io.nl(!IO),
    (
        BuildDepsSucceeded = succeeded,
        io.write_string("** This indicates a bug in `mmc --make'.\n", !IO)
    ;
        BuildDepsSucceeded = did_not_succeed
    ).

check_dependency_timestamps(Globals, TargetFileName, MaybeTimestamp,
        BuildDepsSucceeded, DepFiles, WriteDepFile, DepTimestamps,
        DepsResult, !IO) :-
    (
        MaybeTimestamp = error(_),
        DepsResult = deps_out_of_date,
        debug_make_msg(Globals,
            io.write_string(TargetFileName ++ " does not exist.\n"), !IO)
    ;
        MaybeTimestamp = ok(Timestamp),
        ( if error_in_timestamps(DepTimestamps) then
            DepsResult = deps_error,
            WriteMissingDeps =
                check_dependencies_timestamps_write_missing_deps(
                    TargetFileName, BuildDepsSucceeded, DepFiles,
                    WriteDepFile, DepTimestamps),
            (
                BuildDepsSucceeded = succeeded,

                % Something has gone wrong -- building the target has
                % succeeded, but there are some files missing.
                % Report an error.

                WriteMissingDeps(!IO)
            ;
                BuildDepsSucceeded = did_not_succeed,
                debug_make_msg(Globals, WriteMissingDeps, !IO)
            )
        else
            globals.lookup_bool_option(Globals, rebuild, Rebuild),
            (
                Rebuild = yes,
                % With `--rebuild', we always consider the target to be
                % out-of-date, regardless of the timestamps of its
                % dependencies.
                DepsResult = deps_out_of_date
            ;
                Rebuild = no,
                ( if newer_timestamp(DepTimestamps, Timestamp) then
                    debug_newer_dependencies(Globals, TargetFileName,
                        MaybeTimestamp, DepFiles, DepTimestamps, !IO),
                    DepsResult = deps_out_of_date
                else
                    DepsResult = deps_up_to_date
                )
            )
        )
    ).

:- pred error_in_timestamps(list(maybe_error(timestamp))::in) is semidet.

error_in_timestamps([H | T]) :-
    ( H = error(_)
    ; error_in_timestamps(T)
    ).

:- pred newer_timestamp(list(maybe_error(timestamp))::in, timestamp::in)
    is semidet.

newer_timestamp([H | T], Timestamp) :-
    (
        H = ok(DepTimestamp),
        compare((>), DepTimestamp, Timestamp)
    ;
        newer_timestamp(T, Timestamp)
    ).

:- pred debug_newer_dependencies(globals::in, string::in,
    maybe_error(timestamp)::in, list(T)::in, list(maybe_error(timestamp))::in,
    io::di, io::uo) is det.

debug_newer_dependencies(Globals, TargetFileName, MaybeTimestamp,
        DepFiles, DepTimestamps, !IO) :-
    debug_make_msg(Globals,
        debug_newer_dependencies_2(TargetFileName, MaybeTimestamp,
            DepFiles, DepTimestamps),
        !IO).

:- pred debug_newer_dependencies_2(string::in, maybe_error(timestamp)::in,
    list(T)::in, list(maybe_error(timestamp))::in, io::di, io::uo) is det.

debug_newer_dependencies_2(TargetFileName, MaybeTimestamp,
        DepFiles, DepTimestamps, !IO) :-
    io.write_string(TargetFileName, !IO),
    io.write_string(" [", !IO),
    io.write(MaybeTimestamp, !IO),
    io.write_string("]: newer dependencies:\n", !IO),
    assoc_list.from_corresponding_lists(DepFiles, DepTimestamps,
        DepTimestampAL),
    list.filter(
        ( pred((_DepFile - MaybeDepTimestamp)::in) is semidet :-
            (
                MaybeDepTimestamp = error(_)
            ;
                MaybeDepTimestamp = ok(DepTimestamp),
                MaybeTimestamp = ok(Timestamp),
                compare((>), DepTimestamp, Timestamp)
            )
        ), DepTimestampAL, NewerDepsAL0),
    list.sort(NewerDepsAL0, NewerDepsAL),
    make_write_dependency_file_and_timestamp_list(NewerDepsAL, !IO).

:- pred make_write_dependency_file_and_timestamp_list(
    assoc_list(T, maybe_error(timestamp))::in, io::di, io::uo) is det.

make_write_dependency_file_and_timestamp_list([], !IO).
make_write_dependency_file_and_timestamp_list([Head | Tail], !IO) :-
    Head = DepFile - MaybeTimestamp,
    io.write_char('\t', !IO),
    io.write(DepFile, !IO),
    io.write_char(' ', !IO),
    io.write(MaybeTimestamp, !IO),
    io.nl(!IO),
    make_write_dependency_file_and_timestamp_list(Tail, !IO).

%---------------------------------------------------------------------------%

:- type cached_direct_imports == map(module_index, module_deps_result).

init_cached_direct_imports = map.init.

:- type cached_indirect_imports == map(module_index, module_deps_result).

init_cached_indirect_imports = map.init.

:- type cached_transitive_foreign_imports
    == map(module_index, module_deps_result).

init_cached_transitive_foreign_imports = map.init.

:- type transitive_dependencies_root
    --->    transitive_dependencies_root(
                module_index,
                transitive_dependencies_type,
                module_locn
            ).

:- type transitive_dependencies_type
    --->    interface_imports
    ;       all_imports             % every import_module and use_module
    ;       all_dependencies.       % all_imports plus every include_module

:- type module_locn
    --->    local_module
            % The source file for the module is in the current directory.
    ;       any_module.

:- type cached_transitive_dependencies ==
    map(transitive_dependencies_root, deps_result(module_index)).

init_cached_transitive_dependencies = map.init.

%---------------------------------------------------------------------------%
:- end_module make.dependencies.
%---------------------------------------------------------------------------%
