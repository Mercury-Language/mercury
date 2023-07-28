
%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
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

    % Should a declaration with the given status be written to the `.opt' file?
    %
:- func type_status_to_write(type_status) = bool.
:- func inst_status_to_write(inst_status) = bool.
:- func mode_status_to_write(mode_status) = bool.
:- func typeclass_status_to_write(typeclass_status) = bool.
:- func instance_status_to_write(instance_status) = bool.
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

typeclass_status_to_write(typeclass_status(OldStatus)) = ToWrite :-
    ToWrite = old_status_to_write(OldStatus).

instance_status_to_write(instance_status(OldStatus)) = ToWrite :-
    ToWrite = old_status_to_write(OldStatus).

pred_status_to_write(pred_status(OldStatus)) = ToWrite :-
    ToWrite = old_status_to_write(OldStatus).

%---------------------------------------------------------------------------%

:- func instmode_status_to_write(new_instmode_status) = bool.

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
