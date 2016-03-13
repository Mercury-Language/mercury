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
        ModuleName = unqualified(_),
        record_module_as_used(Visibility, ModuleName, !UsedModules)
    ;
        ModuleName = qualified(ParentModuleName, _),
        record_module_as_used(Visibility, ModuleName, !UsedModules),
        record_module_and_ancestors_as_used(Visibility, ParentModuleName,
            !UsedModules)
    ).

:- pred record_module_as_used(item_visibility::in, module_name::in,
    used_modules::in, used_modules::out) is det.

record_module_as_used(Visibility, ModuleName, !UsedModules) :-
    (
        Visibility = visibility_public,
        IntUsedModules0 = !.UsedModules ^ int_used_modules,
        set.insert(ModuleName, IntUsedModules0, IntUsedModules),
        !UsedModules ^ int_used_modules := IntUsedModules
    ;
        Visibility = visibility_private,
        ImplUsedModules0 = !.UsedModules ^ imp_used_modules,
        set.insert(ModuleName, ImplUsedModules0, ImplUsedModules),
        !UsedModules ^ imp_used_modules := ImplUsedModules
    ).

record_format_modules_as_used(!UsedModules) :-
    ImplUsedModules0 = !.UsedModules ^ imp_used_modules,
    FormatModules = [mercury_string_format_module,
        mercury_string_parse_util_module, mercury_stream_module],
    set.insert_list(FormatModules, ImplUsedModules0, ImplUsedModules),
    !UsedModules ^ imp_used_modules := ImplUsedModules.

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_used_modules.
%---------------------------------------------------------------------------%
