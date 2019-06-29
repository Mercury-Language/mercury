%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module defines the type that holds the status of six kinds of
% HLDS entities; types, insts, modes, typeclasses, instances and predicates.
%
% XXX The old import_status type applied to every one of these entity kinds,
% even though these have different visibility rules. (For example, types can be
% abstract-exported, but predicates cannot.)
%
% There is an accepted design for replacing this single status type with
% with a set of entity-kind-specific types, which avoids this confusion.
% This design is also more structured, in that it has separate fields recording
% the answers to separate questions, instead of flattening out all possible
% combinations of answers into an enum. For the details, see status_proposal
% in compiler/notes.
%
% We have made the first step towards implementing this proposal. Each kind
% of entity now has its own status type, but they are (for now) only wrappers
% around the old_import_status type. Later, they will be specialized to their
% unique needs.
%
%-----------------------------------------------------------------------------%

:- module hlds.status.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_item.

:- import_module bool.

:- type maybe_opt_imported
    --->    is_not_opt_imported
    ;       is_opt_imported.

:- type type_status
    --->    type_status(old_import_status).

:- type inst_status
    --->    inst_status(new_instmode_status).

:- type mode_status
    --->    mode_status(new_instmode_status).

:- type pred_status
    --->    pred_status(old_import_status).

:- type typeclass_status
    --->    typeclass_status(old_import_status).

:- type instance_status
    --->    instance_status(old_import_status).

%-----------------------------------------------------------------------------%

    % The type that should represent the import/export status of both
    % insts and modes, once we transition away from using old_import_status.
:- type new_instmode_status
    --->    instmode_defined_in_this_module(
                instmode_export
            )
    ;       instmode_defined_in_other_module(
                instmode_import
            ).

:- type instmode_export
    --->    instmode_export_nowhere
    ;       instmode_export_only_submodules
    ;       instmode_export_anywhere.

:- type instmode_import
    --->    instmode_import_plain(instmode_import_plain_locn)
            % This inst or mode is defined in a module that was imported
            % by either the current module, or one of its ancestors.
            % The argument gives the location of the import.
    ;       instmode_import_abstract
            % This inst or mode is defined in a module (say module C)
            % that was imported in the implementation section of another
            % module (say module B) that was imported by this module (say
            % module A).
            % XXX STATUS These should never be needed, but there is a test
            % case (just one, valid_seq/tc_map_lookup), that does create
            % insts with such a status, though it does not seem to use them.
            % NOTE We never store insts or modes in abstract form:
            % we either store the full definition, or nothing.
    ;       instmode_import_opt.
            % This inst or mode was read in from either
            % (a) the .opt or .trans_opt file of another module, or
            % (b) an interface file that was read to make sense
            % of a .opt or .trans_opt file.

:- type instmode_import_plain_locn
    --->    instmode_import_plain_imp
    ;       instmode_import_plain_int
    ;       instmode_import_plain_ancestors_priv_int_file.

%-----------------------------------------------------------------------------%

    % The type `old_import_status' describes whether an entity (a predicate,
    % type, inst, or mode) is local to the current module, exported from
    % the current module, or imported from some other module.
    % Only predicates can have status pseudo_exported or pseudo_imported.
    % Only types can have status abstract_exported or abstract_imported.
    %

:- type old_import_status
    --->    status_external(old_import_status)
            % Declared `:- external'. This means that the implementation
            % for this procedure will be provided by some external source,
            % rather than via Mercury clauses (including `pragma
            % foreign_code' clauses). It can be through the use of another
            % language, or it could be through some other method we haven't
            % thought of yet.
            %
            % From the point of view of code generation, an external
            % procedure usually acts like an imported procedure, as its
            % definition is not visible. But in some cases, e.g. writing
            % out declarations for procedures defined in a module, it may
            % need to be treated like an exported procedure (depending on
            % its inner old_import_status).

    ;       status_imported(import_locn)
            % Defined in the interface of some other module.

    ;       status_opt_imported
            % Defined in the optimization interface of another module.

    ;       status_abstract_imported
            % Describes a type with only an abstract declaration imported,
            % maybe with the body of the type imported from a .opt file.

    ;       status_pseudo_imported
            % This is used for entities that are defined in the interface
            % of some other module but for which we may generate some code
            % in this module - in particular, this is used for unification
            % predicates (see comments in unify_proc.m).

    ;       status_exported
            % Defined in the interface of this module.

    ;       status_opt_exported
            % A local item for which the import-status has been changed
            % due to its presence in the .opt files
            % (intermod.adjust_pred_import_status).

    ;       status_abstract_exported
            % Describes a type with only an abstract declaration exported
            % to non-sub-modules. The definition of the type is exported to
            % sub-modules.

    ;       status_pseudo_exported
            % The converse of pseudo_imported; this means that only the
            % (in, in) mode of a unification is exported.

    ;       status_exported_to_submodules
            % Defined in the implementation of this module, and thus in
            % a sense local, but the module contains sub-modules, so the
            % entity needs to be exported to those sub-modules.

    ;       status_local.
            % Defined in the implementation of this module, and the module
            % does not contain any sub-modules.

    % Returns yes if the status indicates that the item was in any way exported
    % -- that is, if it could be used by any other module, or by sub-modules
    % of this module.
    %
    % NOTE: this returns `no' for :- external procedures.
    %
    % See also `procedure_is_exported'.
    %
:- func type_status_is_exported(type_status) = bool.
:- func inst_status_is_exported(inst_status) = bool.
:- func mode_status_is_exported(mode_status) = bool.
:- func pred_status_is_exported(pred_status) = bool.
:- func typeclass_status_is_exported(typeclass_status) = bool.
:- func instance_status_is_exported(instance_status) = bool.

:- func old_status_is_exported(old_import_status) = bool.

    % Returns yes if the status indicates that the item was exported
    % to importing modules (not just to sub-modules).
    %
    % NOTE: this returns `no' for :- external procedures.
    %
:- func type_status_is_exported_to_non_submodules(type_status) = bool.
:- func inst_status_is_exported_to_non_submodules(inst_status) = bool.
:- func mode_status_is_exported_to_non_submodules(mode_status) = bool.
:- func pred_status_is_exported_to_non_submodules(pred_status) = bool.
:- func typeclass_status_is_exported_to_non_submodules(typeclass_status)
    = bool.
:- func instance_status_is_exported_to_non_submodules(instance_status) = bool.

    % Returns yes if the status indicates that the item was in any way imported
    % -- that is, if it was defined in some other module, or in a sub-module
    % of this module. This is the opposite of status_defined_in_this_module.
    %
    % NOTE: this returns `yes' for :- external procedures.
    %
:- func type_status_is_imported(type_status) = bool.
:- func inst_status_is_imported(inst_status) = bool.
:- func mode_status_is_imported(mode_status) = bool.
:- func pred_status_is_imported(pred_status) = bool.
:- func typeclass_status_is_imported(typeclass_status) = bool.
:- func instance_status_is_imported(instance_status) = bool.

    % Returns yes if the status indicates that the item was defined in this
    % module. This is the opposite of status_is_imported.
    %
    % NOTE: this returns `no' for :- external procedures.
    %
:- func type_status_defined_in_this_module(type_status) = bool.
:- func inst_status_defined_in_this_module(inst_status) = bool.
:- func mode_status_defined_in_this_module(mode_status) = bool.
:- func pred_status_defined_in_this_module(pred_status) = bool.
:- func typeclass_status_defined_in_this_module(typeclass_status) = bool.
:- func instance_status_defined_in_this_module(instance_status) = bool.

    % Returns yes if the status indicates the item came from
    % the implementation section.
    %
:- func type_status_defined_in_impl_section(type_status) = bool.
:- func inst_status_defined_in_impl_section(inst_status) = bool.
:- func mode_status_defined_in_impl_section(mode_status) = bool.
:- func pred_status_defined_in_impl_section(pred_status) = bool.
:- func typeclass_status_defined_in_impl_section(typeclass_status) = bool.
:- func instance_status_defined_in_impl_section(instance_status) = bool.

%-----------------------------------------------------------------------------%

:- pred type_make_status_abstract(type_status::in, type_status::out) is det.
:- pred pred_make_status_abstract(pred_status::in, pred_status::out) is det.
:- pred typeclass_make_status_abstract(typeclass_status::in,
    typeclass_status::out) is det.
:- pred instance_make_status_abstract(instance_status::in,
    instance_status::out) is det.

    % XXX Document me.
    %
:- pred type_combine_status(type_status::in, type_status::in,
    type_status::out) is det.
:- pred pred_combine_status(pred_status::in, pred_status::in,
    pred_status::out) is det.
:- pred typeclass_combine_status(typeclass_status::in, typeclass_status::in,
    typeclass_status::out) is det.
:- pred instance_combine_status(instance_status::in, instance_status::in,
    instance_status::out) is det.

%-----------------------------------------------------------------------------%

:- type item_mercury_status
    --->    item_defined_in_this_module(
                item_export
            )
    ;       item_defined_in_other_module(
                item_import
            ).

:- type item_export
    --->    item_export_nowhere
    ;       item_export_only_submodules
    ;       item_export_anywhere.

:- type item_import
    --->    item_import_int_concrete(import_locn)
    ;       item_import_int_abstract
    ;       item_import_opt_int.

:- pred item_mercury_status_to_type_status(item_mercury_status::in,
    type_status::out) is det.
:- pred item_mercury_status_to_inst_status(item_mercury_status::in,
    inst_status::out) is det.
:- pred item_mercury_status_to_mode_status(item_mercury_status::in,
    mode_status::out) is det.
:- pred item_mercury_status_to_typeclass_status(item_mercury_status::in,
    typeclass_status::out) is det.
:- pred item_mercury_status_to_instance_status(item_mercury_status::in,
    instance_status::out) is det.
:- pred item_mercury_status_to_pred_status(item_mercury_status::in,
    pred_status::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

type_status_is_exported(type_status(OldStatus)) =
    old_status_is_exported(OldStatus).

inst_status_is_exported(InstStatus) = IsExported :-
    InstStatus = inst_status(InstModeStatus),
    IsExported = instmode_status_is_exported(InstModeStatus).

mode_status_is_exported(ModeStatus) = IsExported :-
    ModeStatus = mode_status(InstModeStatus),
    IsExported = instmode_status_is_exported(InstModeStatus).

pred_status_is_exported(pred_status(OldStatus)) =
    old_status_is_exported(OldStatus).
typeclass_status_is_exported(typeclass_status(OldStatus)) =
    old_status_is_exported(OldStatus).
instance_status_is_exported(instance_status(OldStatus)) =
    old_status_is_exported(OldStatus).

:- func instmode_status_is_exported(new_instmode_status) = bool.

instmode_status_is_exported(NewInstModeStatus) = NewIsExported :-
    (
        NewInstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            ( InstModeExport = instmode_export_anywhere
            ; InstModeExport = instmode_export_only_submodules
            ),
            NewIsExported = yes
        ;
            InstModeExport = instmode_export_nowhere,
            NewIsExported = no
        )
    ;
        NewInstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        NewIsExported = no
    ).

%-----------------------------------------------------------------------------%

old_status_is_exported(status_imported(_)) =             no.
old_status_is_exported(status_external(_)) =             no.
old_status_is_exported(status_abstract_imported) =       no.
old_status_is_exported(status_pseudo_imported) =         no.
old_status_is_exported(status_opt_imported) =            no.
old_status_is_exported(status_exported) =                yes.
old_status_is_exported(status_opt_exported) =            yes.
old_status_is_exported(status_abstract_exported) =       yes.
old_status_is_exported(status_pseudo_exported) =         yes.
old_status_is_exported(status_exported_to_submodules) =  yes.
old_status_is_exported(status_local) =                   no.

%-----------------------------------------------------------------------------%

type_status_is_exported_to_non_submodules(type_status(Status)) =
    old_status_is_exported_to_non_submodules(Status).

inst_status_is_exported_to_non_submodules(InstStatus) = IsExported :-
    InstStatus = inst_status(InstModeStatus),
    IsExported = instmode_status_is_exported_to_non_submodules(InstModeStatus).

mode_status_is_exported_to_non_submodules(ModeStatus) = IsExported :-
    ModeStatus = mode_status(InstModeStatus),
    IsExported = instmode_status_is_exported_to_non_submodules(InstModeStatus).

pred_status_is_exported_to_non_submodules(pred_status(Status)) =
    old_status_is_exported_to_non_submodules(Status).
typeclass_status_is_exported_to_non_submodules(typeclass_status(Status)) =
    old_status_is_exported_to_non_submodules(Status).
instance_status_is_exported_to_non_submodules(instance_status(Status)) =
    old_status_is_exported_to_non_submodules(Status).

:- func old_status_is_exported_to_non_submodules(old_import_status) = bool.

old_status_is_exported_to_non_submodules(Status) =
    ( if
        old_status_is_exported(Status) = yes,
        Status \= status_exported_to_submodules
    then
        yes
    else
        no
    ).

:- func instmode_status_is_exported_to_non_submodules(new_instmode_status)
    = bool.

instmode_status_is_exported_to_non_submodules(NewInstModeStatus)
        = NewIsExported :-
    (
        NewInstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_anywhere,
            NewIsExported = yes
        ;
            ( InstModeExport = instmode_export_nowhere
            ; InstModeExport = instmode_export_only_submodules
            ),
            NewIsExported = no
        )
    ;
        NewInstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        NewIsExported = no
    ).

%-----------------------------------------------------------------------------%

type_status_is_imported(type_status(OldStatus)) =
    old_status_is_imported(OldStatus).

inst_status_is_imported(InstStatus) = IsImported :-
    InstStatus = inst_status(InstModeStatus),
    (
        InstModeStatus = instmode_defined_in_this_module(_InstModeExport),
        IsImported = no
    ;
        InstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        IsImported = yes
    ).

mode_status_is_imported(ModeStatus) = IsImported :-
    ModeStatus = mode_status(InstModeStatus),
    (
        InstModeStatus = instmode_defined_in_this_module(_InstModeExport),
        IsImported = no
    ;
        InstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        IsImported = yes
    ).

pred_status_is_imported(pred_status(OldStatus)) =
    old_status_is_imported(OldStatus).
typeclass_status_is_imported(typeclass_status(OldStatus)) =
    old_status_is_imported(OldStatus).
instance_status_is_imported(instance_status(OldStatus)) =
    old_status_is_imported(OldStatus).

:- func old_status_is_imported(old_import_status) = bool.

old_status_is_imported(Status) =
    bool.not(old_status_defined_in_this_module(Status)).

%-----------------------------------------------------------------------------%

type_status_defined_in_this_module(type_status(OldStatus)) =
    old_status_defined_in_this_module(OldStatus).

inst_status_defined_in_this_module(InstStatus) = IsDefnThisModule :-
    InstStatus = inst_status(InstModeStatus),
    (
        InstModeStatus = instmode_defined_in_this_module(_InstExport),
        IsDefnThisModule = yes
    ;
        InstModeStatus = instmode_defined_in_other_module(_InstImport),
        IsDefnThisModule = no
    ).

mode_status_defined_in_this_module(ModeStatus) = IsDefnThisModule :-
    ModeStatus = mode_status(InstModeStatus),
    (
        InstModeStatus = instmode_defined_in_this_module(_InstExport),
        IsDefnThisModule = yes
    ;
        InstModeStatus = instmode_defined_in_other_module(_InstImport),
        IsDefnThisModule = no
    ).

pred_status_defined_in_this_module(pred_status(OldStatus)) =
    old_status_defined_in_this_module(OldStatus).
typeclass_status_defined_in_this_module(typeclass_status(OldStatus)) =
    old_status_defined_in_this_module(OldStatus).
instance_status_defined_in_this_module(instance_status(OldStatus)) =
    old_status_defined_in_this_module(OldStatus).

:- func old_status_defined_in_this_module(old_import_status) = bool.

old_status_defined_in_this_module(status_imported(_)) =             no.
old_status_defined_in_this_module(status_external(_)) =             no.
old_status_defined_in_this_module(status_abstract_imported) =       no.
old_status_defined_in_this_module(status_pseudo_imported) =         no.
old_status_defined_in_this_module(status_opt_imported) =            no.
old_status_defined_in_this_module(status_exported) =                yes.
old_status_defined_in_this_module(status_opt_exported) =            yes.
old_status_defined_in_this_module(status_abstract_exported) =       yes.
old_status_defined_in_this_module(status_pseudo_exported) =         yes.
old_status_defined_in_this_module(status_exported_to_submodules) =  yes.
old_status_defined_in_this_module(status_local) =                   yes.

%-----------------------------------------------------------------------------%

type_status_defined_in_impl_section(type_status(OldStatus)) =
    old_status_defined_in_impl_section(OldStatus).

inst_status_defined_in_impl_section(InstStatus) = IsDefnImplSection :-
    InstStatus = inst_status(InstModeStatus),
    IsDefnImplSection =
        instmode_status_defined_in_impl_section(InstModeStatus).

mode_status_defined_in_impl_section(ModeStatus) = IsDefnImplSection :-
    ModeStatus = mode_status(InstModeStatus),
    IsDefnImplSection =
        instmode_status_defined_in_impl_section(InstModeStatus).

pred_status_defined_in_impl_section(pred_status(OldStatus)) =
    old_status_defined_in_impl_section(OldStatus).
typeclass_status_defined_in_impl_section(typeclass_status(OldStatus)) =
    old_status_defined_in_impl_section(OldStatus).
instance_status_defined_in_impl_section(instance_status(OldStatus)) =
    old_status_defined_in_impl_section(OldStatus).

:- func old_status_defined_in_impl_section(old_import_status) = bool.

old_status_defined_in_impl_section(status_abstract_exported) =      yes.
old_status_defined_in_impl_section(status_exported_to_submodules) = yes.
old_status_defined_in_impl_section(status_local) =                  yes.
old_status_defined_in_impl_section(status_opt_imported) =           no.
old_status_defined_in_impl_section(status_abstract_imported) =      no.
old_status_defined_in_impl_section(status_pseudo_imported) =        no.
old_status_defined_in_impl_section(status_exported) =               no.
old_status_defined_in_impl_section(status_opt_exported) =           yes.
old_status_defined_in_impl_section(status_pseudo_exported) =        no.
old_status_defined_in_impl_section(status_external(Status)) =
    old_status_defined_in_impl_section(Status).
old_status_defined_in_impl_section(status_imported(ImportLocn)) =
    import_locn_defined_in_impl_section(ImportLocn).

:- func import_locn_defined_in_impl_section(import_locn) = bool.

% XXX Returning "yes" for import_locn_interface seems wrong.
% XXX Returning "yes" for everything seems wrong.
import_locn_defined_in_impl_section(import_locn_implementation) = yes.
import_locn_defined_in_impl_section(import_locn_interface) = yes.
import_locn_defined_in_impl_section(import_locn_import_by_ancestor) = yes.
import_locn_defined_in_impl_section(import_locn_ancestor_int0_interface) = yes.
import_locn_defined_in_impl_section(import_locn_ancestor_int0_implementation)
    = yes.

:- func instmode_status_defined_in_impl_section(new_instmode_status) = bool.

instmode_status_defined_in_impl_section(NewInstModeStatus)
        = NewIsDefnImplSection :-
    (
        NewInstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            ( InstModeExport = instmode_export_nowhere
            ; InstModeExport = instmode_export_only_submodules
            ),
            NewIsDefnImplSection = yes
        ;
            InstModeExport = instmode_export_anywhere,
            NewIsDefnImplSection = no
        )
    ;
        NewInstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        NewIsDefnImplSection = no
    ).

%-----------------------------------------------------------------------------%

type_make_status_abstract(type_status(Status), type_status(AbstractStatus)) :-
    old_make_status_abstract(Status, AbstractStatus).
pred_make_status_abstract(pred_status(Status), pred_status(AbstractStatus)) :-
    old_make_status_abstract(Status, AbstractStatus).
typeclass_make_status_abstract(typeclass_status(Status),
        typeclass_status(AbstractStatus)) :-
    old_make_status_abstract(Status, AbstractStatus).
instance_make_status_abstract(instance_status(Status),
        instance_status(AbstractStatus)) :-
    old_make_status_abstract(Status, AbstractStatus).

:- pred old_make_status_abstract(old_import_status::in, old_import_status::out)
    is det.

old_make_status_abstract(Status, AbstractStatus) :-
    ( if Status = status_exported then
        AbstractStatus = status_abstract_exported
    else if Status = status_imported(_) then
        AbstractStatus = status_abstract_imported
    else
        AbstractStatus = Status
    ).

%-----------------------------------------------------------------------------%

type_combine_status(type_status(StatusA), type_status(StatusB),
        type_status(Status)) :-
    old_combine_status(StatusA, StatusB, Status).
pred_combine_status(pred_status(StatusA), pred_status(StatusB),
        pred_status(Status)) :-
    old_combine_status(StatusA, StatusB, Status).
typeclass_combine_status(typeclass_status(StatusA), typeclass_status(StatusB),
        typeclass_status(Status)) :-
    old_combine_status(StatusA, StatusB, Status).
instance_combine_status(instance_status(StatusA), instance_status(StatusB),
        instance_status(Status)) :-
    old_combine_status(StatusA, StatusB, Status).

:- pred old_combine_status(old_import_status::in, old_import_status::in,
    old_import_status::out) is det.

old_combine_status(StatusA, StatusB, Status) :-
    ( if combine_status_2(StatusA, StatusB, CombinedStatus) then
        Status = CombinedStatus
    else
        unexpected($pred, "unexpected status for type definition")
    ).

:- pred combine_status_2(old_import_status::in, old_import_status::in,
    old_import_status::out) is semidet.

combine_status_2(status_imported(ImportLocn), Status2, Status) :-
    require_complete_switch [ImportLocn]
    (
        ( ImportLocn = import_locn_implementation
        ; ImportLocn = import_locn_interface
        ; ImportLocn = import_locn_import_by_ancestor
        ),
        combine_status_imported_non_private(Status2, Status)
    ;
        ImportLocn = import_locn_ancestor_int0_interface,
        Status = status_imported(import_locn_ancestor_int0_interface)
    ;
        ImportLocn = import_locn_ancestor_int0_implementation,
        Status = status_imported(import_locn_ancestor_int0_implementation)
    ).
combine_status_2(status_local, Status2, Status) :-
    combine_status_local(Status2, Status).
combine_status_2(status_exported, _Status2, status_exported).
combine_status_2(status_exported_to_submodules, Status2, Status) :-
    combine_status_local(Status2, Status3),
    ( if Status3 = status_local then
        Status = status_exported_to_submodules
    else
        Status = Status3
    ).
combine_status_2(status_opt_imported, _Status2, status_opt_imported).
combine_status_2(status_abstract_imported, Status2, Status) :-
    combine_status_abstract_imported(Status2, Status).
combine_status_2(status_abstract_exported, Status2, Status) :-
    combine_status_abstract_exported(Status2, Status).

:- pred combine_status_imported_non_private(old_import_status::in,
    old_import_status::out) is semidet.

combine_status_imported_non_private(Status2, Status) :-
    (
        Status2 = status_imported(Section),
        Status = status_imported(Section)
    ;
        Status2 = status_local,
        Status = status_imported(import_locn_implementation)
    ;
        Status2 = status_exported,
        Status = status_exported
    ;
        Status2 = status_opt_imported,
        Status = status_opt_imported
    ;
        Status2 = status_abstract_imported,
        Status = status_imported(import_locn_interface)
    ;
        Status2 = status_abstract_exported,
        Status = status_abstract_exported
    ).

:- pred combine_status_local(old_import_status::in, old_import_status::out)
    is semidet.

combine_status_local(status_exported_to_submodules,
    status_exported_to_submodules).
combine_status_local(status_imported(_),            status_local).
combine_status_local(status_local,                  status_local).
combine_status_local(status_exported,               status_exported).
combine_status_local(status_opt_imported,           status_local).
combine_status_local(status_abstract_imported,      status_local).
combine_status_local(status_abstract_exported,      status_abstract_exported).

:- pred combine_status_abstract_exported(old_import_status::in,
    old_import_status::out) is det.

combine_status_abstract_exported(Status2, Status) :-
    ( if Status2 = status_exported then
        Status = status_exported
    else
        Status = status_abstract_exported
    ).

:- pred combine_status_abstract_imported(old_import_status::in,
    old_import_status::out) is det.

combine_status_abstract_imported(Status2, Status) :-
    ( if Status2 = status_imported(Section) then
        Status = status_imported(Section)
    else
        Status = status_abstract_imported
    ).

%-----------------------------------------------------------------------------%

item_mercury_status_to_type_status(ItemMercuryStatus, TypeStatus) :-
    item_mercury_status_to_old_import_status(ItemMercuryStatus,
        OldImportStatus),
    TypeStatus = type_status(OldImportStatus).

item_mercury_status_to_inst_status(ItemMercuryStatus, InstStatus) :-
    item_mercury_status_to_instmode_status(ItemMercuryStatus, InstModeStatus),
    InstStatus = inst_status(InstModeStatus).

item_mercury_status_to_mode_status(ItemMercuryStatus, ModeStatus) :-
    item_mercury_status_to_instmode_status(ItemMercuryStatus, InstModeStatus),
    ModeStatus = mode_status(InstModeStatus).

item_mercury_status_to_typeclass_status(ItemMercuryStatus, TypeClassStatus) :-
    item_mercury_status_to_old_import_status(ItemMercuryStatus,
        OldImportStatus),
    TypeClassStatus = typeclass_status(OldImportStatus).

item_mercury_status_to_instance_status(ItemMercuryStatus, InstanceStatus) :-
    item_mercury_status_to_old_import_status(ItemMercuryStatus,
        OldImportStatus),
    InstanceStatus = instance_status(OldImportStatus).

item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus) :-
    item_mercury_status_to_old_import_status(ItemMercuryStatus,
        OldImportStatus),
    PredStatus = pred_status(OldImportStatus).

:- pred item_mercury_status_to_instmode_status(item_mercury_status::in,
    new_instmode_status::out) is det.

item_mercury_status_to_instmode_status(ItemMercuryStatus, InstModeStatus) :-
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_nowhere,
            InstExport = instmode_export_nowhere
        ;
            ItemExport = item_export_only_submodules,
            InstExport = instmode_export_only_submodules
        ;
            ItemExport = item_export_anywhere,
            InstExport = instmode_export_anywhere
        ),
        InstModeStatus = instmode_defined_in_this_module(InstExport)
    ;
        ItemMercuryStatus = item_defined_in_other_module(ItemImport),
        (
            ItemImport = item_import_int_concrete(ImportLocn),
            (
                ImportLocn = import_locn_implementation,
                InstImportLocn = instmode_import_plain_imp
            ;
                ImportLocn = import_locn_interface,
                InstImportLocn = instmode_import_plain_int
            ;
                ImportLocn = import_locn_import_by_ancestor,
                unexpected($pred, "import_locn_import_by_ancestor")
            ;
                ImportLocn = import_locn_ancestor_int0_interface,
                % XXX Maybe we should have an equivalent to
                % this value of ImportLocn in the type of InstImportLocn
                % as well.
                InstImportLocn = instmode_import_plain_int
            ;
                ImportLocn = import_locn_ancestor_int0_implementation,
                InstImportLocn = instmode_import_plain_ancestors_priv_int_file
            ),
            InstImport = instmode_import_plain(InstImportLocn)
        ;
            ItemImport = item_import_int_abstract,
            InstImport = instmode_import_abstract
        ;
            ItemImport = item_import_opt_int,
            InstImport = instmode_import_opt
        ),
        InstModeStatus = instmode_defined_in_other_module(InstImport)
    ).

:- pred item_mercury_status_to_old_import_status(item_mercury_status::in,
    old_import_status::out) is det.

item_mercury_status_to_old_import_status(ItemMercuryStatus, OldImportStatus) :-
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_nowhere,
            OldImportStatus = status_local
        ;
            ItemExport = item_export_only_submodules,
            OldImportStatus = status_exported_to_submodules
        ;
            ItemExport = item_export_anywhere,
            OldImportStatus = status_exported
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(ItemImport),
        (
            ItemImport = item_import_int_concrete(ImportLocn),
            OldImportStatus = status_imported(ImportLocn)
        ;
            ItemImport = item_import_int_abstract,
            OldImportStatus = status_abstract_imported
        ;
            ItemImport = item_import_opt_int,
            OldImportStatus = status_opt_imported
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.status.
%-----------------------------------------------------------------------------%
