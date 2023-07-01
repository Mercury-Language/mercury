%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: module_dep_info.m.
% Author: zs, based on ancient code by fjh.
%
% This module contains the main data structure used by mmc --make
% for representing dependencies between modules.
%
%---------------------------------------------------------------------------%

:- module parse_tree.module_dep_info.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module set.

%---------------------------------------------------------------------------%

    % The source of mmc --make's information about what other modules
    % a given module depends on may be either
    %
    % - the contents of that module' source file (this is represented
    %   by module_dep_info_imports), or
    %
    % - the contents of the module's .module_dep file (this is represented
    %   by module_dep_info_summary).
    %
:- type module_dep_info
    --->    module_dep_info_full(burdened_module)
    ;       module_dep_info_summary(module_dep_summary).

:- type module_dep_summary
    --->    module_dep_summary(
                mds_source_file_name        :: string,
                mds_source_file_dir         :: string,
                mds_source_file_module_name :: module_name,
                mds_module_name             :: module_name,
                mds_children                :: set(module_name),
                mds_maybe_top_module        :: maybe_top_module,
                mds_int_deps                :: set(module_name),
                mds_imp_deps                :: set(module_name),
                mds_fact_table_file_names   :: set(string),
                mds_fims                    :: set(fim_spec),
                mds_foreign_include_files   :: set(foreign_include_file_info),
                mds_contains_foreign_code   :: contains_foreign_code,
                mds_contains_foreign_export :: contains_foreign_export
            ).

:- pred module_dep_info_get_source_file_name(module_dep_info::in,
    string::out) is det.
:- pred module_dep_info_get_source_file_dir(module_dep_info::in,
    string::out) is det.
:- pred module_dep_info_get_source_file_module_name(module_dep_info::in,
    module_name::out) is det.
:- pred module_dep_info_get_module_name(module_dep_info::in,
    module_name::out) is det.
:- pred module_dep_info_get_children(module_dep_info::in,
    set(module_name)::out) is det.
:- pred module_dep_info_get_maybe_top_module(module_dep_info::in,
    maybe_top_module::out) is det.
:- pred module_dep_info_get_int_deps(module_dep_info::in,
    set(module_name)::out) is det.
:- pred module_dep_info_get_imp_deps(module_dep_info::in,
    set(module_name)::out) is det.
:- pred module_dep_info_get_fact_tables(module_dep_info::in,
    set(string)::out) is det.
:- pred module_dep_info_get_fims(module_dep_info::in,
    set(fim_spec)::out) is det.
:- pred module_dep_info_get_foreign_include_files(module_dep_info::in,
    set(foreign_include_file_info)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.prog_data.

:- import_module map.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

module_dep_info_get_source_file_name(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        Baggage = BurdenedModule ^ bm_baggage,
        X = Baggage ^ mb_source_file_name
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_source_file_name
    ).

module_dep_info_get_source_file_dir(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        Baggage = BurdenedModule ^ bm_baggage,
        X = Baggage ^ mb_source_file_dir
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_source_file_dir
    ).

module_dep_info_get_source_file_module_name(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        Baggage = BurdenedModule ^ bm_baggage,
        X = Baggage ^ mb_source_file_module_name
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_source_file_module_name
    ).

module_dep_info_get_module_name(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        X = ParseTreeModuleSrc ^ ptms_module_name
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_module_name
    ).

module_dep_info_get_children(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
        X = map.keys_as_set(IncludeMap)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_children
    ).

module_dep_info_get_maybe_top_module(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        Baggage = BurdenedModule ^ bm_baggage,
        X = Baggage ^ mb_maybe_top_module
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_maybe_top_module
    ).

module_dep_info_get_int_deps(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        parse_tree_module_src_get_int_imp_deps(ParseTreeModuleSrc, X, _)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_int_deps
    ).

module_dep_info_get_imp_deps(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        parse_tree_module_src_get_int_imp_deps(ParseTreeModuleSrc, _, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_imp_deps
    ).

module_dep_info_get_fact_tables(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        get_fact_tables(ParseTreeModuleSrc, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_fact_table_file_names
    ).

module_dep_info_get_fims(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        get_fim_specs(ParseTreeModuleSrc, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_fims
    ).

module_dep_info_get_foreign_include_files(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        get_foreign_include_file_infos(ParseTreeModuleSrc, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_foreign_include_files
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_dep_info.
%---------------------------------------------------------------------------%
