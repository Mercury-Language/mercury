%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: module_deps_graph.m.
%
% XXX document me.
% The contents of this module used to be in the old giant version of modules.m.
% If you want to understand it and clean it up, you will also want to look at
% deps_map.m, whose functionality seems to be quite related.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.module_deps_graph.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_imports.

:- import_module digraph.

%-----------------------------------------------------------------------------%

    % (Module1 -> Module2) means Module1 is imported by Module2.
:- type deps_graph == digraph(module_name).

:- type lookup_module_dep_info == (func(module_name) = module_dep_info).

:- pred add_module_dep_info_to_deps_graph(module_dep_info::in,
    lookup_module_dep_info::in,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set.

:- type deps_graph_key == digraph_key(module_name).

%-----------------------------------------------------------------------------%

add_module_dep_info_to_deps_graph(ModuleDepInfo, LookupModuleDepInfo,
        !IntDepsGraph, !ImpDepsGraph) :-
    % Add interface dependencies to the interface deps graph.
    %
    % Note that we need to do this both for the interface imports of this
    % module and for the *implementation* imports of its ancestors.
    % This is because if this module is defined in the implementation section
    % of its parent, then the interface of this module may depend on things
    % imported only by its parent's implementation.
    %
    % If this module was actually defined in the interface section of one
    % of its ancestors, then it should only depend on the interface imports
    % of that ancestor, so the dependencies added here are in fact more
    % conservative than they need to be in that case. However, that should
    % not be a major problem.
    % XXX Actually, I (zs) think it is, because I suspect that after some
    % source code changes, it can lead to the unnecessary recompilation
    % of not just a few, but many modules.

    module_dep_info_get_module_name(ModuleDepInfo, ModuleName),
    Ancestors = get_ancestors_set(ModuleName),
    digraph.add_vertex(ModuleName, IntModuleKey, !IntDepsGraph),
    add_int_deps(IntModuleKey, ModuleDepInfo, !IntDepsGraph),
    add_parent_imp_deps_set(LookupModuleDepInfo, IntModuleKey, Ancestors,
        !IntDepsGraph),

    % Add implementation dependencies to the implementation deps graph.
    % (The implementation dependencies are a superset of the interface
    % dependencies.)
    %
    % Note that we need to do this both for the imports of this module
    % and for the imports of its parents, because this module may depend on
    % things imported only by its parents.

    digraph.add_vertex(ModuleName, ImpModuleKey, !ImpDepsGraph),
    add_imp_deps(ImpModuleKey, ModuleDepInfo, !ImpDepsGraph),
    add_parent_imp_deps_set(LookupModuleDepInfo, ImpModuleKey, Ancestors,
        !ImpDepsGraph).

    % Add interface dependencies to the interface deps graph.
    %
:- pred add_int_deps(deps_graph_key::in, module_dep_info::in,
    deps_graph::in, deps_graph::out) is det.

add_int_deps(ModuleKey, ModuleDepInfo, !DepsGraph) :-
    AddDep = add_dep(ModuleKey),
    module_dep_info_get_module_name(ModuleDepInfo, ModuleName),
    Ancestors = get_ancestors_set(ModuleName),
    module_dep_info_get_int_deps(ModuleDepInfo, IntDeps),
    set.fold(AddDep, Ancestors, !DepsGraph),
    set.fold(AddDep, IntDeps, !DepsGraph).

    % Add direct implementation dependencies for a module to the
    % implementation deps graph.
    %
:- pred add_imp_deps(deps_graph_key::in, module_dep_info::in,
    deps_graph::in, deps_graph::out) is det.

add_imp_deps(ModuleKey, ModuleDepInfo, !DepsGraph) :-
    % The implementation dependencies are a superset of the
    % interface dependencies, so first we add the interface deps, ...
    add_int_deps(ModuleKey, ModuleDepInfo, !DepsGraph),
    % ... and then we add the implementation deps.
    module_dep_info_get_imp_deps(ModuleDepInfo, ImpDeps),
    set.foldl(add_dep(ModuleKey), ImpDeps, !DepsGraph).

    % Add parent implementation dependencies for the given Parent module
    % to the implementation deps graph values for the given ModuleKey.
    %
:- pred add_parent_imp_deps(lookup_module_dep_info::in,
    deps_graph_key::in, module_name::in, deps_graph::in, deps_graph::out)
    is det.

add_parent_imp_deps(LookupModuleDepInfo, ModuleKey, Parent, !DepsGraph) :-
    ParentModuleDepInfo = LookupModuleDepInfo(Parent),
    add_imp_deps(ModuleKey, ParentModuleDepInfo, !DepsGraph).

:- pred add_parent_imp_deps_set(lookup_module_dep_info::in,
    deps_graph_key::in, set(module_name)::in, deps_graph::in, deps_graph::out)
    is det.

add_parent_imp_deps_set(LookupModuleDepInfo, ModuleKey, Parents,
        !DepsGraph) :-
    set.fold(add_parent_imp_deps(LookupModuleDepInfo, ModuleKey), Parents,
        !DepsGraph).

    % Add a single dependency to a graph.
    %
:- pred add_dep(digraph_key(T)::in, T::in, digraph(T)::in, digraph(T)::out)
    is det.

add_dep(ModuleKey, Dep, !DepsGraph) :-
    digraph.add_vertex(Dep, DepKey, !DepsGraph),
    digraph.add_edge(ModuleKey, DepKey, !DepsGraph).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.module_deps_graph.
%-----------------------------------------------------------------------------%
