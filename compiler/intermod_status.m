
%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023, 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module contains code that makes decisions about whether an entity's
% status allows it to be written out to .opt files.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod_status.
:- interface.

:- import_module hlds.
:- import_module hlds.status.

:- import_module bool.
:- import_module maybe.

    % Should a declaration with the given status be written to the `.opt' file?
    %
:- func type_status_to_write(type_status) = bool.
:- func inst_status_to_write(inst_status) = bool.
:- func mode_status_to_write(mode_status) = bool.
:- func typeclass_status_to_write(typeclass_status) = maybe(typeclass_export).
:- func instance_status_to_write(instance_status) = maybe(instance_export).
:- func pred_status_to_write(pred_status) = bool.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

type_status_to_write(type_status(OldStatus)) = ToWrite :-
    ToWrite = old_status_to_write(OldStatus).

inst_status_to_write(inst_status(InstModeStatus)) = ToWrite :-
    ToWrite = instmode_status_to_write(InstModeStatus).

mode_status_to_write(mode_status(InstModeStatus)) = ToWrite :-
    ToWrite = instmode_status_to_write(InstModeStatus).

typeclass_status_to_write(TypeClassStatus) = ToWrite :-
    ToWrite = new_typeclass_status_to_write(TypeClassStatus).

instance_status_to_write(InstanceStatus) = ToWrite :-
    ToWrite = new_instance_status_to_write(InstanceStatus).

pred_status_to_write(pred_status(OldStatus)) = ToWrite :-
    ToWrite = old_status_to_write(OldStatus).

%---------------------------------------------------------------------------%

:- func instmode_status_to_write(instmode_status) = bool.

instmode_status_to_write(InstModeStatus) = ToWrite :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_anywhere,
            ToWrite = no
        ;
            ( InstModeExport = instmode_export_only_submodules
            ; InstModeExport = instmode_export_nowhere
            ),
            ToWrite = yes
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(_),
        ToWrite = no
    ).

:- func new_typeclass_status_to_write(typeclass_status)
    = maybe(typeclass_export).

new_typeclass_status_to_write(Status) = ToWrite :-
    (
        Status = typeclass_defined_in_this_module(Export),
        (
            ( Export = typeclass_export_gen_none_sub_none
            ; Export = typeclass_export_gen_none_sub_full
            ; Export = typeclass_export_gen_abs_sub_full
            ),
            ToWrite = yes(typeclass_export_gen_full_sub_full)
        ;
            Export = typeclass_export_gen_full_sub_full,
            ToWrite = no
        )
    ;
        Status = typeclass_defined_in_other_module(_),
        ToWrite = no
    ).

:- func new_instance_status_to_write(instance_status) = maybe(instance_export).

new_instance_status_to_write(Status) = ToWrite :-
    (
        Status = instance_defined_in_this_module(Export),
        (
            ( Export = instance_export_gen_none_sub_none
            ; Export = instance_export_gen_none_sub_abs
            ; Export = instance_export_gen_abs_sub_abs
            ),
            ToWrite = yes(instance_export_full_opt)
        ;
            Export = instance_export_full_opt,
            % XXX INSTANCE_STATUS This seems strange, but
            % it preserves old behavior.
            ToWrite = no
        )
    ;
        Status = instance_defined_in_other_module(_),
        ToWrite = no
    ).

:- func old_status_to_write(old_import_status) = bool.

old_status_to_write(status_imported(_)) = no.
old_status_to_write(status_abstract_imported) = no.
old_status_to_write(status_pseudo_imported) = no.
old_status_to_write(status_opt_imported) = no.
old_status_to_write(status_exported) = no.
old_status_to_write(status_opt_exported) = yes.
old_status_to_write(status_abstract_exported) = yes.
old_status_to_write(status_pseudo_exported) = no.
old_status_to_write(status_exported_to_submodules) = yes.
old_status_to_write(status_local) = yes.
old_status_to_write(status_external(Status)) =
    bool.not(old_status_is_exported(Status)).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod_status.
%---------------------------------------------------------------------------%
