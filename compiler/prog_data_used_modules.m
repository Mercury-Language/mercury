%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the used_modules type and its operations.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_data_used_modules.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module set.

%---------------------------------------------------------------------------%

:- type used_modules
    --->    used_modules(
                % The modules used in the interface and implementation.
                int_used_modules    :: set(module_name),
                imp_used_modules    :: set(module_name)
            ).

:- type item_visibility
    --->    visibility_public
    ;       visibility_private.

    % Initialize the used_modules structure.
    %
:- func used_modules_init = used_modules.

    % Given a sym_name, call record_module_and_ancestors_as_used on the module
    % part of the name.
    %
:- pred record_sym_name_module_as_used(item_visibility::in, sym_name::in,
    used_modules::in, used_modules::out) is det.

    % Given a module name, add the module and all of its parent modules
    % to the used_modules.
    %
:- pred record_module_and_ancestors_as_used(item_visibility::in, sym_name::in,
    used_modules::in, used_modules::out) is det.

:- pred record_format_modules_as_used(used_modules::in, used_modules::out)
    is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.

:- import_module list.

used_modules_init = used_modules(set.init, set.init).

record_sym_name_module_as_used(Visibility, SymName, !UsedModules) :-
    (
        SymName = unqualified(_)
    ;
        SymName = qualified(ModuleName, _),
        record_module_and_ancestors_as_used(Visibility, ModuleName,
            !UsedModules)
    ).

record_module_and_ancestors_as_used(Visibility, ModuleName, !UsedModules) :-
    (
        Visibility = visibility_public,
        IntUsedModules0 = !.UsedModules ^ int_used_modules,
        add_module_and_ancestors(ModuleName, IntUsedModules0, IntUsedModules),
        !UsedModules ^ int_used_modules := IntUsedModules
    ;
        Visibility = visibility_private,
        ImpUsedModules0 = !.UsedModules ^ imp_used_modules,
        add_module_and_ancestors(ModuleName, ImpUsedModules0, ImpUsedModules),
        !UsedModules ^ imp_used_modules := ImpUsedModules
    ).

:- pred add_module_and_ancestors(sym_name::in,
    set(module_name)::in, set(module_name)::out) is det.

add_module_and_ancestors(ModuleName, !UsedModuleNames) :-
    set.insert(ModuleName, !UsedModuleNames),
    (
        ModuleName = unqualified(_)
    ;
        ModuleName = qualified(ParentModuleName, _),
        add_module_and_ancestors(ParentModuleName, !UsedModuleNames)
    ).

record_format_modules_as_used(!UsedModules) :-
    ImpUsedModules0 = !.UsedModules ^ imp_used_modules,
    FormatModules = [mercury_string_format_module,
        mercury_string_parse_util_module, mercury_stream_module],
    set.insert_list(FormatModules, ImpUsedModules0, ImpUsedModules),
    !UsedModules ^ imp_used_modules := ImpUsedModules.

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_used_modules.
%---------------------------------------------------------------------------%
