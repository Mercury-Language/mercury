%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2015, 2024-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%

:- module hlds.status.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_parse_tree.

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

    % NOTE Any replacement for this type should take into account
    % the possibility that a predicate may be declared to be external
    % for one backend, while being actually defined, by either clauses
    % or foreign_procs, for another backend.
:- type pred_status
    --->    pred_status(old_import_status).

% The new_{typeclass,instance}_status types and these equivalences
% should go away after a transitional period.

:- type typeclass_status == new_typeclass_status.

:- type instance_status == new_instance_status.

%---------------------------------------------------------------------------%

    % The type that should represent the import/export status of both
    % insts and modes, once we transition away from using old_import_status.
:- type new_instmode_status
    --->    instmode_defined_in_this_module(instmode_export)
    ;       instmode_defined_in_other_module(instmode_import).

:- type instmode_export
    --->    instmode_export_nowhere
    ;       instmode_export_only_submodules
    ;       instmode_export_anywhere.

:- type instmode_import
    --->    instmode_import_plain
            % This inst or mode is defined in a module that was imported
            % by either the current module, or one of its ancestors.
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

%---------------------------------------------------------------------------%

:- type new_typeclass_status
    --->    typeclass_defined_in_this_module(typeclass_export)
    ;       typeclass_defined_in_other_module(typeclass_import).

:- type typeclass_export
    --->    typeclass_export_gen_none_sub_none
            % Both the interface and the concrete definition of the typeclass
            % are visible only in the current module. They would also be
            % visible to its submodules, but there aren't any.
    ;       typeclass_export_gen_none_sub_full
            % Both the interface and the concrete definition of the typeclass
            % are visible only in the current module and its submodules.
    ;       typeclass_export_gen_abs_sub_full
            % Both the interface and the concrete definition of the typeclass
            % are visible in the current module and its submodules, and
            % its interface is also visible to any other module that
            % imports the current module.
    ;       typeclass_export_gen_full_sub_full.
            % Both the interface and the concrete definition of the typeclass
            % are visible in the current module, its submodules, and in
            % any other module that imports the current module.
            %
            % A typeclass can have this typeclass_export value *either*
            % because of the occurrence of the concrete typeclass definition
            % in the interface section, *or* because intermod_status.m
            % decides to opt-export the typeclass. This conflation
            % is why there is no export mirror of typeclass_import_full_opt.

    % XXX TYPECLASS_STATUS Are any of these "full"s lies?
    % XXX TYPECLASS_STATUS We need *some* of the distinctions between
    % import locations, but do we need them *all*?
    % No, we do not. We could merge these three into one function symbol:
    %   typeclass_import_full_own_int
    %   typeclass_import_full_own_imp
    %   typeclass_import_full_by_ancestor
    % and these two into one function symbol:
    %   typeclass_import_full_int0_int
    %   typeclass_import_full_int0_imp
:- type typeclass_import
    --->    typeclass_import_full_own_int
    ;       typeclass_import_full_own_imp
    ;       typeclass_import_full_int0_int
    ;       typeclass_import_full_int0_imp
    ;       typeclass_import_full_by_ancestor
    ;       typeclass_import_full_opt
    ;       typeclass_import_abstract.

%---------------------------------------------------------------------------%

:- type new_instance_status
    --->    instance_defined_in_this_module(instance_export)
    ;       instance_defined_in_other_module(instance_import).

:- type instance_export
    --->    instance_export_gen_none_sub_none
            % Both the abstract and concrete versions of the instance
            % are visible only in the current module. The abstract version
            % would also be visible in its submodules, but there aren't any.
    ;       instance_export_gen_none_sub_abs
            % Both the abstract and concrete versions of the instance
            % are visible in the current module. The abstract version
            % is also visible in its submodules.
    ;       instance_export_gen_abs_sub_abs
            % Both the abstract and concrete versions of the instance
            % are visible in the current module. The abstract version
            % is also visible in its submodules and in any other module
            % that imports the current module.
    ;       instance_export_full_opt.
            % Both the abstract and concrete versions of the instance
            % are visible in the current module, in its submodules,
            % and in any other module that imports the current module.
            %
            % A instance can have this instance_export value *only*
            % because intermod_status.m decides to opt-export the instance.
            % This conflation is why there is no export mirror of
            % instance_import_full_opt.

:- type instance_import
    --->    instance_import_full_opt
    ;       instance_import_abstract.

%---------------------------------------------------------------------------%

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
            % to non-submodules. The definition of the type is exported to
            % submodules.

    ;       status_pseudo_exported
            % The converse of pseudo_imported; this means that only the
            % (in, in) mode of a unification is exported.

    ;       status_exported_to_submodules
            % Defined in the implementation of this module, and thus in
            % a sense local, but the module contains submodules, so the
            % entity needs to be exported to those submodules.

    ;       status_local.
            % Defined in the implementation of this module, and the module
            % does not contain any submodules.

    % Returns yes if the status indicates that the item was in any way exported
    % -- that is, if it could be used by any other module, or by submodules
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
    % to importing modules (not just to submodules).
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
    % -- that is, if it was defined in some other module, or in a submodule
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

%---------------------------------------------------------------------------%

:- pred type_make_status_abstract(type_status::in, type_status::out) is det.
:- pred pred_make_status_abstract(pred_status::in, pred_status::out) is det.
:- pred typeclass_make_status_abstract(typeclass_status::in,
    typeclass_status::out) is det.

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % Exported to add_class.m.
    %
:- func new_typeclass_status_to_old(new_typeclass_status) = old_import_status.

    % Exported to check_typeclass.m.
    %
:- func new_instance_status_to_old(new_instance_status) = old_import_status.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

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

typeclass_status_is_exported(TypeClassStatus) = IsExported :-
    OldStatus = new_typeclass_status_to_old(TypeClassStatus),
    OldIsExported = old_status_is_exported(OldStatus),
    NewIsExported = new_typeclass_status_is_exported(TypeClassStatus),
    IsExported = return_if_agreed(OldIsExported, NewIsExported).

instance_status_is_exported(InstanceStatus) = IsExported :-
    OldStatus = new_instance_status_to_old(InstanceStatus),
    OldIsExported = old_status_is_exported(OldStatus),
    NewIsExported = new_instance_status_is_exported(InstanceStatus),
    IsExported = return_if_agreed(OldIsExported, NewIsExported).

%---------------------%

:- func instmode_status_is_exported(new_instmode_status) = bool.

instmode_status_is_exported(InstModeStatus) = IsExported :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            ( InstModeExport = instmode_export_anywhere
            ; InstModeExport = instmode_export_only_submodules
            ),
            IsExported = yes
        ;
            InstModeExport = instmode_export_nowhere,
            IsExported = no
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        IsExported = no
    ).

%---------------------%

:- func new_typeclass_status_is_exported(new_typeclass_status) = bool.

new_typeclass_status_is_exported(TypeClassStatus) = IsExported :-
    (
        TypeClassStatus = typeclass_defined_in_this_module(TypeClassExport),
        (
            TypeClassExport = typeclass_export_gen_none_sub_none,
            IsExported = no
        ;
            ( TypeClassExport = typeclass_export_gen_none_sub_full
            ; TypeClassExport = typeclass_export_gen_abs_sub_full
            ; TypeClassExport = typeclass_export_gen_full_sub_full
            ),
            IsExported = yes
        )
    ;
        TypeClassStatus = typeclass_defined_in_other_module(_TypeClassImport),
        IsExported = no
    ).

%---------------------%

:- func new_instance_status_is_exported(new_instance_status) = bool.

new_instance_status_is_exported(InstanceStatus) = IsExported :-
    (
        InstanceStatus = instance_defined_in_this_module(InstanceExport),
        (
            InstanceExport = instance_export_gen_none_sub_none,
            IsExported = no
        ;
            ( InstanceExport = instance_export_gen_none_sub_abs
            ; InstanceExport = instance_export_gen_abs_sub_abs
            ; InstanceExport = instance_export_full_opt
            ),
            IsExported = yes
        )
    ;
        InstanceStatus = instance_defined_in_other_module(_InstanceImport),
        IsExported = no
    ).

%---------------------%

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

%---------------------------------------------------------------------------%

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

typeclass_status_is_exported_to_non_submodules(TypeClassStatus) = IsExported :-
    OldStatus = new_typeclass_status_to_old(TypeClassStatus),
    OldIsExported = old_status_is_exported_to_non_submodules(OldStatus),
    NewIsExported =
        new_typeclass_status_is_exported_to_non_submodules(TypeClassStatus),
    IsExported = return_if_agreed(OldIsExported, NewIsExported).

instance_status_is_exported_to_non_submodules(InstanceStatus) = IsExported :-
    OldStatus = new_instance_status_to_old(InstanceStatus),
    OldIsExported = old_status_is_exported_to_non_submodules(OldStatus),
    NewIsExported =
        new_instance_status_is_exported_to_non_submodules(InstanceStatus),
    IsExported = return_if_agreed(OldIsExported, NewIsExported).

%---------------------%

:- func instmode_status_is_exported_to_non_submodules(new_instmode_status)
    = bool.

instmode_status_is_exported_to_non_submodules(InstModeStatus) = IsExported :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_anywhere,
            IsExported = yes
        ;
            ( InstModeExport = instmode_export_nowhere
            ; InstModeExport = instmode_export_only_submodules
            ),
            IsExported = no
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(_InstModeImport),
        IsExported = no
    ).

%---------------------%

:- func new_typeclass_status_is_exported_to_non_submodules(
    new_typeclass_status) = bool.

new_typeclass_status_is_exported_to_non_submodules(Status) = IsExported :-
    (
        Status = typeclass_defined_in_this_module(Export),
        (
            ( Export = typeclass_export_gen_none_sub_none
            ; Export = typeclass_export_gen_none_sub_full
            ),
            IsExported = no
        ;
            ( Export = typeclass_export_gen_abs_sub_full
            ; Export = typeclass_export_gen_full_sub_full
            ),
            IsExported = yes
        )
    ;
        Status = typeclass_defined_in_other_module(_Import),
        IsExported = no
    ).

%---------------------%

:- func new_instance_status_is_exported_to_non_submodules(
    new_instance_status) = bool.

new_instance_status_is_exported_to_non_submodules(Status) = IsExported :-
    (
        Status = instance_defined_in_this_module(Export),
        (
            ( Export = instance_export_gen_none_sub_none
            ; Export = instance_export_gen_none_sub_abs
            ),
            IsExported = no
        ;
            ( Export = instance_export_gen_abs_sub_abs
            ; Export = instance_export_full_opt
            ),
            IsExported = yes
        )
    ;
        Status = instance_defined_in_other_module(_Import),
        IsExported = no
    ).

%---------------------%

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

%---------------------------------------------------------------------------%

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

typeclass_status_is_imported(Status) = IsImported :-
    (
        Status = typeclass_defined_in_this_module(_Export),
        IsImported = no
    ;
        Status = typeclass_defined_in_other_module(_Import),
        IsImported = yes
    ).

instance_status_is_imported(Status) = IsImported :-
    (
        Status = instance_defined_in_this_module(_Export),
        IsImported = no
    ;
        Status = instance_defined_in_other_module(_Import),
        IsImported = yes
    ).

%---------------------%

:- func old_status_is_imported(old_import_status) = bool.

old_status_is_imported(Status) =
    bool.not(old_status_defined_in_this_module(Status)).

%---------------------------------------------------------------------------%

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

typeclass_status_defined_in_this_module(Status) = IsDefnThisModule :-
    (
        Status = typeclass_defined_in_this_module(_Export),
        IsDefnThisModule = yes
    ;
        Status = typeclass_defined_in_other_module(_Import),
        IsDefnThisModule = no
    ).

instance_status_defined_in_this_module(Status) = IsDefnThisModule :-
    (
        Status = instance_defined_in_this_module(_Export),
        IsDefnThisModule = yes
    ;
        Status = instance_defined_in_other_module(_Import),
        IsDefnThisModule = no
    ).

%---------------------%

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

%---------------------------------------------------------------------------%

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

typeclass_status_defined_in_impl_section(TypeClassStatus) = InImplSection :-
    OldStatus = new_typeclass_status_to_old(TypeClassStatus),
    OldInImplSection = old_status_defined_in_impl_section(OldStatus),
    NewInImplSection =
        new_typeclass_status_defined_in_impl_section(TypeClassStatus),
    InImplSection = return_if_agreed(OldInImplSection, NewInImplSection).

instance_status_defined_in_impl_section(InstanceStatus) = InImplSection :-
    OldStatus = new_instance_status_to_old(InstanceStatus),
    OldInImplSection = old_status_defined_in_impl_section(OldStatus),
    NewInImplSection =
        new_instance_status_defined_in_impl_section(InstanceStatus),
    InImplSection = return_if_agreed(OldInImplSection, NewInImplSection).

%---------------------%

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

%---------------------%

:- func new_typeclass_status_defined_in_impl_section(new_typeclass_status)
    = bool.

new_typeclass_status_defined_in_impl_section(Status) = IsDefnImplSection :-
    (
        Status = typeclass_defined_in_this_module(Export),
        (
            ( Export = typeclass_export_gen_none_sub_none
            ; Export = typeclass_export_gen_none_sub_full
            ; Export = typeclass_export_gen_abs_sub_full
            ),
            IsDefnImplSection = yes
        ;
            Export = typeclass_export_gen_full_sub_full,
            IsDefnImplSection = no
        )
    ;
        Status = typeclass_defined_in_other_module(_Import),
        IsDefnImplSection = no
    ).

%---------------------%

:- func new_instance_status_defined_in_impl_section(new_instance_status)
    = bool.

new_instance_status_defined_in_impl_section(Status) = IsDefnImplSection :-
    (
        Status = instance_defined_in_this_module(Export),
        (
            ( Export = instance_export_gen_none_sub_none
            ; Export = instance_export_gen_none_sub_abs
            ; Export = instance_export_gen_abs_sub_abs
            ),
            IsDefnImplSection = yes
        ;
            Export = instance_export_full_opt,
            IsDefnImplSection = no
        )
    ;
        Status = instance_defined_in_other_module(_Import),
        IsDefnImplSection = no
    ).

%---------------------%

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
old_status_defined_in_impl_section(status_imported(_ImportLocn)) =   no.

%---------------------------------------------------------------------------%

type_make_status_abstract(type_status(Status), type_status(AbstractStatus)) :-
    old_make_status_abstract(Status, AbstractStatus).

pred_make_status_abstract(pred_status(Status), pred_status(AbstractStatus)) :-
    old_make_status_abstract(Status, AbstractStatus).

typeclass_make_status_abstract(Status, AbstractStatus) :-
    OldStatus = new_typeclass_status_to_old(Status),
    old_make_status_abstract(OldStatus, OldAbstractStatus),
    new_typeclass_status_make_status_abstract(Status, NewAbstractStatus),
    OldNewAbstractStatus = new_typeclass_status_to_old(NewAbstractStatus),
    ( if OldAbstractStatus = OldNewAbstractStatus then
        AbstractStatus = NewAbstractStatus
    else
        unexpected($pred, "disagreement")
    ).

%---------------------%

:- pred new_typeclass_status_make_status_abstract(new_typeclass_status::in,
    new_typeclass_status::out) is det.

new_typeclass_status_make_status_abstract(Status, AbstractStatus) :-
    (
        Status = typeclass_defined_in_this_module(Export),
        (
            ( Export = typeclass_export_gen_none_sub_none
            ; Export = typeclass_export_gen_none_sub_full
            ; Export = typeclass_export_gen_abs_sub_full
            ),
            AbstractStatus = Status
        ;
            Export = typeclass_export_gen_full_sub_full,
            AbstractExport = typeclass_export_gen_abs_sub_full,
            AbstractStatus = typeclass_defined_in_this_module(AbstractExport)
        )
    ;
        Status = typeclass_defined_in_other_module(Import),
        (
            % XXX TYPECLASS It does not make sense to try to make
            % typeclass_import_full_opt abstract. We could try throwing
            % an exception whith typeclass_import_full_opt.
            ( Import = typeclass_import_full_opt
            ; Import = typeclass_import_abstract
            ),
            AbstractStatus = Status
        ;
            ( Import = typeclass_import_full_own_int
            ; Import = typeclass_import_full_own_imp
            ; Import = typeclass_import_full_int0_int
            ; Import = typeclass_import_full_int0_imp
            ; Import = typeclass_import_full_by_ancestor
            ),
            AbstractImport = typeclass_import_abstract,
            AbstractStatus = typeclass_defined_in_other_module(AbstractImport)
        )
    ).

%---------------------%

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

%---------------------------------------------------------------------------%

type_combine_status(type_status(StatusA), type_status(StatusB),
        type_status(Status)) :-
    ( if old_combine_status(StatusA, StatusB, CombinedStatus) then
        Status = CombinedStatus
    else
        unexpected($pred, "unexpected status for type definition")
    ).

pred_combine_status(pred_status(StatusA), pred_status(StatusB),
        pred_status(Status)) :-
    ( if old_combine_status(StatusA, StatusB, CombinedStatus) then
        Status = CombinedStatus
    else
        unexpected($pred, "unexpected status for pred definition")
    ).

typeclass_combine_status(StatusA, StatusB, Status) :-
    ( if new_typeclass_combine_status(StatusA, StatusB, CombinedStatus) then
        NewStatus = CombinedStatus
    else
        unexpected($pred, "unexpected status for new typeclass definition")
    ),
    OldStatusA = new_typeclass_status_to_old(StatusA),
    OldStatusB = new_typeclass_status_to_old(StatusB),
    OldNewStatus = new_typeclass_status_to_old(NewStatus),
    ( if old_combine_status(OldStatusA, OldStatusB, OldCombinedStatus) then
        OldStatus = OldCombinedStatus
    else
        unexpected($pred, "unexpected status for old typeclass definition")
    ),
    ( if OldStatus = OldNewStatus then
        Status = NewStatus
    else
        unexpected($pred, "disagreement")
    ).

instance_combine_status(StatusA, StatusB, Status) :-
    ( if new_instance_combine_status(StatusA, StatusB, CombinedStatus) then
        NewStatus = CombinedStatus
    else
        unexpected($pred, "unexpected status for new instance definition")
    ),
    OldStatusA = new_instance_status_to_old(StatusA),
    OldStatusB = new_instance_status_to_old(StatusB),
    OldNewStatus = new_instance_status_to_old(NewStatus),
    ( if old_combine_status(OldStatusA, OldStatusB, OldCombinedStatus) then
        OldStatus = OldCombinedStatus
    else
        unexpected($pred, "unexpected status for old instance definition")
    ),
    ( if OldStatus = OldNewStatus then
        Status = NewStatus
    else
        unexpected($pred, "disagreement")
    ).

%---------------------%

:- pred new_typeclass_combine_status(new_typeclass_status::in,
    new_typeclass_status::in, new_typeclass_status::out) is semidet.

new_typeclass_combine_status(StatusA, StatusB, Status) :-
    require_complete_switch [StatusA]
    (
        StatusA = typeclass_defined_in_other_module(ImportA),
        StatusB = typeclass_defined_in_other_module(ImportB),
        require_complete_switch [ImportA]
        (
            ( ImportA = typeclass_import_full_own_int
            ; ImportA = typeclass_import_full_own_imp
            ; ImportA = typeclass_import_full_by_ancestor
            ),
            (
                ( ImportB = typeclass_import_full_own_int
                ; ImportB = typeclass_import_full_own_imp
                ; ImportB = typeclass_import_full_int0_int
                ; ImportB = typeclass_import_full_int0_imp
                ; ImportB = typeclass_import_full_by_ancestor
                ),
                Import = ImportB
            ;
                ImportB = typeclass_import_abstract,
                % XXX TYPECLASS_STATUS I (zs) think this should be ImportA,
                % but it does not seem to matter.
                Import = typeclass_import_full_own_imp
            )
        ;
            ( ImportA = typeclass_import_full_int0_int
            ; ImportA = typeclass_import_full_int0_imp
            ; ImportA = typeclass_import_full_opt
            ),
            Import = ImportA
        ;
            ImportA = typeclass_import_abstract,
            require_complete_switch [ImportB]
            (
                ( ImportB = typeclass_import_full_own_int
                ; ImportB = typeclass_import_full_own_imp
                ; ImportB = typeclass_import_full_int0_int
                ; ImportB = typeclass_import_full_int0_imp
                ; ImportB = typeclass_import_full_by_ancestor
                ),
                Import = ImportB
            ;
                ( ImportB = typeclass_import_full_opt
                ; ImportB = typeclass_import_abstract
                ),
                Import = ImportA
            )
        ),
        Status = typeclass_defined_in_other_module(Import)
    ;
        StatusA = typeclass_defined_in_this_module(ExportA),
        StatusB = typeclass_defined_in_this_module(ExportB),
        require_complete_switch [ExportA]
        (
            ExportA = typeclass_export_gen_none_sub_none,
            Export = ExportB
        ;
            ExportA = typeclass_export_gen_none_sub_full,
            (
                ExportB = typeclass_export_gen_none_sub_none,
                % XXX TYPECLASS_STATUS This ExportA/ExportB combination
                % should never occur.
                Export = typeclass_export_gen_none_sub_full
            ;
                ( ExportB = typeclass_export_gen_none_sub_full
                ; ExportB = typeclass_export_gen_abs_sub_full
                ; ExportB = typeclass_export_gen_full_sub_full
                ),
                Export = ExportB
            )
        ;
            ExportA = typeclass_export_gen_full_sub_full,
            Export = typeclass_export_gen_full_sub_full
        ;
            ExportA = typeclass_export_gen_abs_sub_full,
            ( if ExportB = typeclass_export_gen_full_sub_full then
                Export = typeclass_export_gen_full_sub_full
            else
                Export = typeclass_export_gen_abs_sub_full
            )
        ),
        Status = typeclass_defined_in_this_module(Export)
    ).

%---------------------%

:- pred new_instance_combine_status(new_instance_status::in,
    new_instance_status::in, new_instance_status::out) is semidet.

new_instance_combine_status(StatusA, StatusB, Status) :-
    require_complete_switch [StatusA]
    (
        StatusA = instance_defined_in_other_module(ImportA),
        StatusB = instance_defined_in_other_module(_ImportB),
        Status = instance_defined_in_other_module(ImportA)
    ;
        StatusA = instance_defined_in_this_module(ExportA),
        StatusB = instance_defined_in_this_module(ExportB),
        require_complete_switch [ExportA]
        (
            ExportA = instance_export_gen_none_sub_none,
            Export = ExportB
        ;
            ExportA = instance_export_gen_none_sub_abs,
            (
                ExportB = instance_export_gen_none_sub_none,
                % XXX INSTANCE_STATUS This ExportA/ExportB combination
                % should never occur.
                Export = instance_export_gen_none_sub_abs
            ;
                ( ExportB = instance_export_gen_none_sub_abs
                ; ExportB = instance_export_gen_abs_sub_abs
                ; ExportB = instance_export_full_opt
                ),
                Export = ExportB
            )
        ;
            ExportA = instance_export_gen_abs_sub_abs,
            ( if ExportB = instance_export_full_opt then
                Export = instance_export_full_opt
            else
                Export = instance_export_gen_abs_sub_abs
            )
        ;
            ExportA = instance_export_full_opt,
            Export = instance_export_full_opt
        ),
        Status = instance_defined_in_this_module(Export)
    ;
        StatusA = instance_defined_in_this_module(ExportA),
        StatusB = instance_defined_in_other_module(_ImportB),
        % XXX INSTANCE_STATUS This combination should not be allowed.
        Status = instance_defined_in_this_module(ExportA)
    ).

%---------------------%

:- pred old_combine_status(old_import_status::in, old_import_status::in,
    old_import_status::out) is semidet.

old_combine_status(StatusA, StatusB, Status) :-
    require_complete_switch [StatusA]
    (
        StatusA = status_imported(ImportLocnA),
        require_complete_switch [ImportLocnA]
        (
            ( ImportLocnA = import_locn_implementation
            ; ImportLocnA = import_locn_interface
            ; ImportLocnA = import_locn_import_by_ancestor
            ),
            (
                StatusB = status_imported(SectionB),
                Status = status_imported(SectionB)
            ;
                StatusB = status_local,
                Status = status_imported(import_locn_implementation)
            ;
                StatusB = status_exported,
                Status = status_exported
            ;
                StatusB = status_opt_imported,
                Status = status_opt_imported
            ;
                StatusB = status_abstract_imported,
                Status = status_imported(import_locn_interface)
            ;
                StatusB = status_abstract_exported,
                Status = status_abstract_exported
            )
        ;
            ImportLocnA = import_locn_ancestor_int0_interface,
            Status = status_imported(import_locn_ancestor_int0_interface)
        ;
            ImportLocnA = import_locn_ancestor_int0_implementation,
            Status = status_imported(import_locn_ancestor_int0_implementation)
        )
    ;
        StatusA = status_local,
        old_combine_status_local(StatusB, Status)
    ;
        StatusA = status_exported,
        Status = status_exported
    ;
        StatusA = status_exported_to_submodules,
        old_combine_status_local(StatusB, Status3),
        ( if Status3 = status_local then
            Status = status_exported_to_submodules
        else
            Status = Status3
        )
    ;
        StatusA = status_opt_imported,
        Status = status_opt_imported
    ;
        StatusA = status_abstract_imported,
        ( if StatusB = status_imported(Section) then
            Status = status_imported(Section)
        else
            Status = status_abstract_imported
        )
    ;
        StatusA = status_abstract_exported,
        ( if StatusB = status_exported then
            Status = status_exported
        else
            Status = status_abstract_exported
        )
    ;
        ( StatusA = status_external(_)
        ; StatusA = status_opt_exported
        ; StatusA = status_pseudo_exported
        ; StatusA = status_pseudo_imported
        ),
        fail
    ).

:- pred old_combine_status_local(old_import_status::in, old_import_status::out)
    is semidet.

old_combine_status_local(status_exported_to_submodules,
    status_exported_to_submodules).
old_combine_status_local(status_imported(_),        status_local).
old_combine_status_local(status_local,              status_local).
old_combine_status_local(status_exported,           status_exported).
old_combine_status_local(status_opt_imported,       status_local).
old_combine_status_local(status_abstract_imported,  status_local).
old_combine_status_local(status_abstract_exported,  status_abstract_exported).

%---------------------------------------------------------------------------%

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
    item_mercury_status_to_new_typeclass_status(ItemMercuryStatus,
        NewTypeClassStatus),
    item_mercury_status_to_old_import_status(ItemMercuryStatus,
        OldImportStatus),
    OldNewTypeClassStatus = new_typeclass_status_to_old(NewTypeClassStatus),
    ( if OldNewTypeClassStatus = OldImportStatus then
        TypeClassStatus = NewTypeClassStatus
    else
        unexpected($pred, "disagreement")
    ).

item_mercury_status_to_instance_status(ItemMercuryStatus, InstanceStatus) :-
    % We cannot do the same sanity check for instances as for typeclasses,
    % because the relationship between the values of the instance_import type
    % and of the item_import type is one-to-many, not one-to-one.
    item_mercury_status_to_new_instance_status(ItemMercuryStatus,
        InstanceStatus).

item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus) :-
    item_mercury_status_to_old_import_status(ItemMercuryStatus,
        OldImportStatus),
    PredStatus = pred_status(OldImportStatus).

%---------------------%

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
            ItemImport = item_import_int_concrete(_ImportLocn),
            InstImport = instmode_import_plain
        ;
            ItemImport = item_import_int_abstract,
            InstImport = instmode_import_abstract
        ;
            ItemImport = item_import_opt_int,
            InstImport = instmode_import_opt
        ),
        InstModeStatus = instmode_defined_in_other_module(InstImport)
    ).

%---------------------%

:- pred item_mercury_status_to_new_typeclass_status(item_mercury_status::in,
    new_typeclass_status::out) is det.

item_mercury_status_to_new_typeclass_status(ItemMercuryStatus,
        TypeClassStatus) :-
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_nowhere,
            TypeClassExport = typeclass_export_gen_none_sub_none
        ;
            ItemExport = item_export_only_submodules,
            TypeClassExport = typeclass_export_gen_none_sub_full
        ;
            ItemExport = item_export_anywhere,
            TypeClassExport = typeclass_export_gen_full_sub_full
        ),
        TypeClassStatus = typeclass_defined_in_this_module(TypeClassExport)
    ;
        ItemMercuryStatus = item_defined_in_other_module(ItemImport),
        (
            ItemImport = item_import_int_concrete(ImportLocn),
            require_complete_switch [ImportLocn]
            (
                ImportLocn = import_locn_interface,
                TypeClassImport = typeclass_import_full_own_int
            ;
                ImportLocn = import_locn_implementation,
                TypeClassImport = typeclass_import_full_own_imp
            ;
                ImportLocn = import_locn_ancestor_int0_interface,
                TypeClassImport = typeclass_import_full_int0_int
            ;
                ImportLocn = import_locn_ancestor_int0_implementation,
                TypeClassImport = typeclass_import_full_int0_imp
            ;
                ImportLocn = import_locn_import_by_ancestor,
                TypeClassImport = typeclass_import_full_by_ancestor
            )
        ;
            ItemImport = item_import_int_abstract,
            TypeClassImport = typeclass_import_abstract
        ;
            ItemImport = item_import_opt_int,
            TypeClassImport = typeclass_import_full_opt
        ),
        TypeClassStatus = typeclass_defined_in_other_module(TypeClassImport)
    ).

%---------------------%

:- pred item_mercury_status_to_new_instance_status(item_mercury_status::in,
    new_instance_status::out) is det.

item_mercury_status_to_new_instance_status(ItemMercuryStatus,
        InstanceStatus) :-
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_nowhere,
            InstanceExport = instance_export_gen_none_sub_none
        ;
            ItemExport = item_export_only_submodules,
            InstanceExport = instance_export_gen_none_sub_abs
        ;
            ItemExport = item_export_anywhere,
            InstanceExport = instance_export_gen_abs_sub_abs
        ),
        InstanceStatus = instance_defined_in_this_module(InstanceExport)
    ;
        ItemMercuryStatus = item_defined_in_other_module(ItemImport),
        (
            ( ItemImport = item_import_int_concrete(_ImportLocn)
            ; ItemImport = item_import_int_abstract
            ),
            % *All* instances we get from interface files are imported
            % abstract, even the ones we get from implementation sections.
            % The only instances we get in concrete form are ...
            InstanceImport = instance_import_abstract
        ;
            ItemImport = item_import_opt_int,
            % ... from .opt files.
            InstanceImport = instance_import_full_opt
        ),
        InstanceStatus = instance_defined_in_other_module(InstanceImport)
    ).

%---------------------%

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

%---------------------------------------------------------------------------%

new_typeclass_status_to_old(New) = Old :-
    (
        New = typeclass_defined_in_this_module(Export),
        (
            Export = typeclass_export_gen_none_sub_none,
            Old = status_local
        ;
            Export = typeclass_export_gen_none_sub_full,
            Old = status_exported_to_submodules
        ;
            Export = typeclass_export_gen_full_sub_full,
            Old = status_exported
        ;
            Export = typeclass_export_gen_abs_sub_full,
            Old = status_abstract_exported
        )
    ;
        New = typeclass_defined_in_other_module(Import),
        (
            Import = typeclass_import_full_own_int,
            Old = status_imported(import_locn_interface)
        ;
            Import = typeclass_import_full_own_imp,
            Old = status_imported(import_locn_implementation)
        ;
            Import = typeclass_import_full_int0_int,
            Old = status_imported(import_locn_ancestor_int0_interface)
        ;
            Import = typeclass_import_full_int0_imp,
            Old = status_imported(import_locn_ancestor_int0_implementation)
        ;
            Import = typeclass_import_full_by_ancestor,
            Old = status_imported(import_locn_import_by_ancestor)
        ;
            Import = typeclass_import_full_opt,
            Old = status_opt_imported
        ;
            Import = typeclass_import_abstract,
            Old = status_abstract_imported
        )
    ).

new_instance_status_to_old(New) = Old :-
    (
        New = instance_defined_in_this_module(Export),
        (
            Export = instance_export_gen_none_sub_none,
            Old = status_local
        ;
            Export = instance_export_gen_none_sub_abs,
            Old = status_exported_to_submodules
        ;
            Export = instance_export_gen_abs_sub_abs,
            Old = status_exported
        ;
            Export = instance_export_full_opt,
            Old = status_opt_exported
        )
    ;
        New = instance_defined_in_other_module(Import),
        (
            Import = instance_import_full_opt,
            Old = status_opt_imported
        ;
            Import = instance_import_abstract,
            Old = status_abstract_imported
        )
    ).

:- type instance_import
    --->    instance_import_full_opt
    ;       instance_import_abstract.

:- func return_if_agreed(T, T) = T.

return_if_agreed(Old, New) = Agreed :-
    ( if Old = New then
        Agreed = Old
    else
        unexpected($pred, "disagreement")
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.status.
%---------------------------------------------------------------------------%
