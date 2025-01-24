%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: item_util.m.
%
% This module contains utility predicates for dealing with items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.convert_include.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.

:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

    % classify_include_modules(IntIncludes, ImpIncludes, InclMap, !Specs):
    %
    % Record the inclusion of each submodule in the section and at the context
    % where it happens in InclMap.
    %
    % If a submodule is included more than once within the same section,
    % keep only the first inclusion. If a submodule is included in both
    % the interface and the implementation section, keep only the one in the
    % interface section. In both cases, generate an error message for all
    % the other inclusions.
    %
:- pred classify_include_modules(
    list(item_include)::in, list(item_include)::in, include_module_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred include_map_to_int_imp_modules(include_module_map::in,
    set(module_name)::out, set(module_name)::out) is det.

:- pred add_only_int_include(module_name::in, include_module_info::in,
    int_include_module_map::in, int_include_module_map::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_data.

:- import_module term_context.

%---------------------------------------------------------------------------%

classify_include_modules(IntIncludes, ImpIncludes,
        !:InclMap, !Specs) :-
    map.init(!:InclMap),
    list.foldl2(classify_include_module(ms_interface), IntIncludes,
        !InclMap, !Specs),
    list.foldl2(classify_include_module(ms_implementation), ImpIncludes,
        !InclMap, !Specs).

:- pred classify_include_module(module_section::in, item_include::in,
    include_module_map::in, include_module_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_include_module(Section, ItemInclude, !InclMap, !Specs) :-
    ItemInclude = item_include(ModuleName, Context, _SeqNum),
    ( if map.search(!.InclMap, ModuleName, PrevEntry) then
        PrevEntry = include_module_info(_PrevSection, PrevContext),
        report_duplicate_include(ModuleName, PrevContext, Context, !Specs)
    else
        Entry = include_module_info(Section, Context),
        map.det_insert(ModuleName, Entry, !InclMap)
    ).

:- pred report_duplicate_include(module_name::in,
    prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_include(ModuleName, PrevContext, Context, !Specs) :-
    MainPieces = [words("Error:")] ++
        color_as_incorrect([words("duplicate"),
            decl("include"), words("declaration")]) ++
        [words("for submodule")] ++
        color_as_subject([qual_sym_name(ModuleName), suffix(".")]) ++
        [nl],
    MainMsg = msg(Context, MainPieces),
    PrevPieces = [words("The previous"),
        decl("include"), words("declaration was here."), nl],
    PrevMsg = msg(PrevContext, PrevPieces),
    Spec = error_spec($pred, severity_error, phase_pt2h, [MainMsg, PrevMsg]),
    !:Specs = [Spec | !.Specs].

%---------------------%

include_map_to_int_imp_modules(IncludeMap, IntModules, ImpModules) :-
    map.foldl2(include_map_to_int_imp_modules_acc, IncludeMap,
        set.init, IntModules, set.init, ImpModules).

:- pred include_map_to_int_imp_modules_acc(
    module_name::in, include_module_info::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

include_map_to_int_imp_modules_acc(ModuleName, InclInfo,
        !IntModules, !ImpModules) :-
    InclInfo = include_module_info(Section, _Context),
    (
        Section = ms_interface,
        set.insert(ModuleName, !IntModules)
    ;
        Section = ms_implementation,
        set.insert(ModuleName, !ImpModules)
    ).

%---------------------%

add_only_int_include(ModuleName, InclInfo, !IntInclMap) :-
    InclInfo = include_module_info(Section, Context),
    (
        Section = ms_interface,
        IntInclInfo = include_module_info(ms_interface, Context),
        map.det_insert(ModuleName, IntInclInfo, !IntInclMap)
    ;
        Section = ms_implementation
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.convert_include.
%---------------------------------------------------------------------------%
