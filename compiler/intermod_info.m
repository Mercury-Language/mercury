%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod_info.m.
% Main author: stayl (the original intermod.m).
%
% This module defines the intermod_info type, which we use to record
% the set of entities we want to put into .opt files, and the basic
% operations on it.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type maybe_need_foreign_import_modules
    --->    do_not_need_foreign_import_modules
    ;       do_need_foreign_import_modules.

    % A value of this type specifies the set of entities we opt-export
    % from a module.
    %
:- type intermod_info.

    % Initialize an intermod_info structure.
    %
:- pred init_intermod_info(module_info::in, intermod_info::out) is det.

    % Return the results recorded in a filled-in intermod_info structure.
    %
:- pred deconstruct_intermod_info(intermod_info::in, module_info::out,
    set(module_name)::out, set(pred_id)::out, set(pred_id)::out,
    assoc_list(class_id, hlds_instance_defn)::out,
    assoc_list(type_ctor, hlds_type_defn)::out,
    maybe_need_foreign_import_modules::out) is det.

    % Getters for the intermod_info structure.
    %
:- pred intermod_info_get_module_info(intermod_info::in, module_info::out)
    is det.
:- pred intermod_info_get_use_modules(intermod_info::in, set(module_name)::out)
    is det.
:- pred intermod_info_get_pred_decls(intermod_info::in, set(pred_id)::out)
    is det.
:- pred intermod_info_get_pred_defns(intermod_info::in, set(pred_id)::out)
    is det.
:- pred intermod_info_get_instances(intermod_info::in,
    assoc_list(class_id, hlds_instance_defn)::out) is det.
:- pred intermod_info_get_types(intermod_info::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

    % Setters for the intermod_info structure.
    %
:- pred intermod_info_set_use_modules(set(module_name)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_pred_decls(set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_pred_defns(set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_instances(
    assoc_list(class_id, hlds_instance_defn)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_types(assoc_list(type_ctor, hlds_type_defn)::in,
    intermod_info::in, intermod_info::out) is det.
%:- pred intermod_info_set_insts(set(inst_ctor)::in,
%   intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_need_foreign_import_modules(intermod_info::in,
    intermod_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

    % A collection of stuff to go in the .opt file.
    %
:- type intermod_info
    --->    intermod_info(
                % The initial ModuleInfo. Readonly.
                im_module_info          :: module_info,

                % The modules that the .opt file will need to use.
                im_use_modules          :: set(module_name),

                % The ids of the predicates (and functions) whose type and mode
                % declarations we want to put into the .opt file.
                im_pred_decls           :: set(pred_id),

                % The ids of the predicates (and functions) whose definitions
                % (i.e. clauses, foreign_procs or promises) we want to put
                % into the .opt file.
                im_pred_defns           :: set(pred_id),

                % The instance definitions we want to put into the .opt file.
                im_instance_defns       :: assoc_list(class_id,
                                            hlds_instance_defn),

                % The type definitions we want to put into the .opt file.
                im_type_defns           :: assoc_list(type_ctor,
                                            hlds_type_defn),

                % Is there anything we want to put into the .opt file
                % that may refer to foreign language entities that may need
                % access to foreign_import_modules to resolve?
                %
                % If no, we don't need to include any of the
                % foreign_import_modules declarations in the module
                % in the .opt file.
                %
                % If yes, we need to include all of them in the .opt file,
                % since we have no info about which fim defines what.
                im_need_foreign_imports :: maybe_need_foreign_import_modules
            ).

init_intermod_info(ModuleInfo, IntermodInfo) :-
    set.init(Modules),
    set.init(PredDecls),
    set.init(PredDefns),
    InstanceDefns = [],
    TypeDefns = [],
    IntermodInfo = intermod_info(ModuleInfo, Modules, PredDecls, PredDefns,
        InstanceDefns, TypeDefns, do_not_need_foreign_import_modules).

deconstruct_intermod_info(IntermodInfo, ModuleInfo, Modules,
        PredDecls, PredDefns, InstanceDefns, TypeDefns, NeedFIMs) :-
    IntermodInfo = intermod_info(ModuleInfo, Modules, PredDecls, PredDefns,
        InstanceDefns, TypeDefns, NeedFIMs).

intermod_info_get_module_info(IntermodInfo, X) :-
    X = IntermodInfo ^ im_module_info.
intermod_info_get_use_modules(IntermodInfo, X) :-
    X = IntermodInfo ^ im_use_modules.
intermod_info_get_pred_decls(IntermodInfo, X) :-
    X = IntermodInfo ^ im_pred_decls.
intermod_info_get_pred_defns(IntermodInfo, X) :-
    X = IntermodInfo ^ im_pred_defns.
intermod_info_get_instances(IntermodInfo, X) :-
    X = IntermodInfo ^ im_instance_defns.
intermod_info_get_types(IntermodInfo, X) :-
    X = IntermodInfo ^ im_type_defns.

intermod_info_set_use_modules(X, !IntermodInfo) :-
    !IntermodInfo ^ im_use_modules := X.
intermod_info_set_pred_decls(X, !IntermodInfo) :-
    !IntermodInfo ^ im_pred_decls := X.
intermod_info_set_pred_defns(X, !IntermodInfo) :-
    !IntermodInfo ^ im_pred_defns := X.
intermod_info_set_instances(X, !IntermodInfo) :-
    !IntermodInfo ^ im_instance_defns := X.
intermod_info_set_types(X, !IntermodInfo) :-
    !IntermodInfo ^ im_type_defns := X.
intermod_info_set_need_foreign_import_modules(!IntermodInfo) :-
    !IntermodInfo ^ im_need_foreign_imports := do_need_foreign_import_modules.

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod_info.
%---------------------------------------------------------------------------%
