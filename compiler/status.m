%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module defines the type that holds the status of items and
% of HLDS entities, such as types and predicates.
%
% XXX We probably should have different types for the status
% in the parse tree and the HLDS.
%
% XXX The current import_status type applies to every kind of entity in the
% HLDS, including types, insts, modes, user-defined functions and predicates,
% and compiler-generated unify, index and compare predicates, even though
% these have different visibility rules. (For example, types can be
% abstract-exported, but predicates cannot.)
%
% There is an accepted design for replacing this single status type with
% with a set of entity-kind-specific types, which avoids this confusion.
% This design is also more structured, in that it has separate fields recording
% the answers to separate questions, instead of flattening out all possible
% combinations of answers into an enum. For the details, see status_proposal
% in compiler/notes.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.status.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_kind.

:- import_module bool.

    % The type `import_status' describes whether an entity (a predicate,
    % type, inst, or mode) is local to the current module, exported from
    % the current module, or imported from some other module.
    % Only predicates can have status pseudo_exported or pseudo_imported.
    % Only types can have status abstract_exported or abstract_imported.
    %
:- type import_status
    --->    status_external(import_status)
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
            % its inner import_status).

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

    % An import_locn is used to describe the place where an item was
    % imported from.
:- type import_locn
    --->    import_locn_implementation
            % The item is from a module imported in the implementation.

    ;       import_locn_interface
            % The item is from a module imported in the interface.

    ;       import_locn_ancestor
            % The item is from a module imported by an ancestor.

    ;       import_locn_ancestor_private_interface_proper.
            % The item is from the _actual_ private interface of an ancestor
            % module, i.e. the implementation section of a `.int0' file.

    % Returns yes if the status indicates that the item was in any way exported
    % -- that is, if it could be used by any other module, or by sub-modules
    % of this module.
    %
    % NOTE: this returns `no' for :- external procedures.
    %
    % See also `procedure_is_exported'.
    %
:- func status_is_exported(import_status) = bool.

    % Returns yes if the status indicates that the item was exported
    % to importing modules (not just to sub-modules).
    %
    % NOTE: this returns `no' for :- external procedures.
    %
:- func status_is_exported_to_non_submodules(import_status) = bool.

    % Returns yes if the status indicates that the item was in any way imported
    % -- that is, if it was defined in some other module, or in a sub-module
    % of this module. This is the opposite of status_defined_in_this_module.
    %
    % NOTE: this returns `yes' for :- external procedures.
    %
:- func status_is_imported(import_status) = bool.

    % Returns yes if the status indicates that the item was defined in this
    % module. This is the opposite of status_is_imported.
    %
    % NOTE: this returns `no' for :- external procedures.
    %
:- func status_defined_in_this_module(import_status) = bool.

    % Returns yes if the import_status indicates the item came from
    % the implementation section.
    %
:- func status_defined_in_impl_section(import_status) = bool.

%-----------------------------------------------------------------------------%

    % Describes whether an item can be used without an explicit module
    % qualifier.
    %
:- type need_qualifier
    --->    must_be_qualified
    ;       may_be_unqualified.

    % When adding an item to the HLDS, we need to know both its import_status
    % and whether uses of it must be module qualified.
    %
:- type item_status
    --->    item_status(import_status, need_qualifier).

%-----------------------------------------------------------------------------%

:- type module_section
    --->    ms_interface
    ;       ms_implementation.

:- type src_module_section
    --->    sms_interface
    ;       sms_implementation
    ;       sms_impl_but_exported_to_submodules.
            % This is used internally by the compiler, to identify items
            % which originally came from an implementation section of a module
            % that contains submodules; such items need to be exported
            % to the submodules.

:- type int_module_section
    --->    ims_imported(module_name, int_file_kind, import_locn)
    ;       ims_used(module_name, int_file_kind, import_locn)
            % These are used internally by the compiler, to identify
            % declarations which originally came from some other module
            % imported with a `:- import_module' or `:- use_module'
            % declaration. They record the name of the imported module,
            % and in which section the module was imported or used.

    ;       ims_abstract_imported(module_name, int_file_kind).
            % This is used internally by the compiler, to identify items which
            % originally came from the implementation section of an interface
            % file; usually type declarations (especially equivalence types)
            % which should be used in code generation but not in type checking.

:- type opt_module_section
    --->    oms_opt_imported(module_name, opt_file_kind).
            % This is used internally by the compiler, to identify items which
            % originally came from an optimization file.

:- type int_for_opt_module_section
    --->    ioms_opt_imported(module_name, int_file_kind).
            % This is used internally by the compiler, to identify items which
            % originally came from an interface file needed by an
            % optimization file.

%-----------------------------------------------------------------------------%

:- func make_ims_imported(import_locn, module_name, int_file_kind) =
    int_module_section.
:- func make_ims_used(import_locn, module_name, int_file_kind) =
    int_module_section.
:- func make_ims_abstract_imported(module_name, int_file_kind) =
    int_module_section.

:- func make_oms_opt_imported(module_name, opt_file_kind) =
    opt_module_section.
:- func make_ioms_opt_imported(module_name, int_file_kind) =
    int_for_opt_module_section.

    % Return the item_status appropriate for items in the given kind of block.
    %
:- pred src_module_section_status(src_module_section::in,
    item_status::out) is det.
:- pred int_module_section_status(int_module_section::in,
    item_status::out) is det.
:- pred opt_module_section_status(opt_module_section::in,
    item_status::out) is det.
:- pred int_for_opt_module_section_status(int_for_opt_module_section::in,
    item_status::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

status_is_exported(status_imported(_)) =             no.
status_is_exported(status_external(_)) =             no.
status_is_exported(status_abstract_imported) =       no.
status_is_exported(status_pseudo_imported) =         no.
status_is_exported(status_opt_imported) =            no.
status_is_exported(status_exported) =                yes.
status_is_exported(status_opt_exported) =            yes.
status_is_exported(status_abstract_exported) =       yes.
status_is_exported(status_pseudo_exported) =         yes.
status_is_exported(status_exported_to_submodules) =  yes.
status_is_exported(status_local) =                   no.

status_is_exported_to_non_submodules(Status) =
    ( if
        status_is_exported(Status) = yes,
        Status \= status_exported_to_submodules
    then
        yes
    else
        no
    ).

status_is_imported(Status) = bool.not(status_defined_in_this_module(Status)).

status_defined_in_this_module(status_imported(_)) =             no.
status_defined_in_this_module(status_external(_)) =             no.
status_defined_in_this_module(status_abstract_imported) =       no.
status_defined_in_this_module(status_pseudo_imported) =         no.
status_defined_in_this_module(status_opt_imported) =            no.
status_defined_in_this_module(status_exported) =                yes.
status_defined_in_this_module(status_opt_exported) =            yes.
status_defined_in_this_module(status_abstract_exported) =       yes.
status_defined_in_this_module(status_pseudo_exported) =         yes.
status_defined_in_this_module(status_exported_to_submodules) =  yes.
status_defined_in_this_module(status_local) =                   yes.

status_defined_in_impl_section(status_abstract_exported) =      yes.
status_defined_in_impl_section(status_exported_to_submodules) = yes.
status_defined_in_impl_section(status_local) =                  yes.
status_defined_in_impl_section(status_opt_imported) =           no.
status_defined_in_impl_section(status_abstract_imported) =      no.
status_defined_in_impl_section(status_pseudo_imported) =        no.
status_defined_in_impl_section(status_exported) =               no.
status_defined_in_impl_section(status_opt_exported) =           yes.
status_defined_in_impl_section(status_pseudo_exported) =        no.
status_defined_in_impl_section(status_external(Status)) =
    status_defined_in_impl_section(Status).
status_defined_in_impl_section(status_imported(ImportLocn)) =
    import_locn_defined_in_impl_section(ImportLocn).

:- func import_locn_defined_in_impl_section(import_locn) = bool.

% XXX Returning "yes" for import_locn_interface seems wrong.
import_locn_defined_in_impl_section(import_locn_implementation) = yes.
import_locn_defined_in_impl_section(import_locn_interface) = yes.
import_locn_defined_in_impl_section(import_locn_ancestor) = yes.
import_locn_defined_in_impl_section(
    import_locn_ancestor_private_interface_proper) = yes.

%-----------------------------------------------------------------------------%

make_ims_imported(ImportLocn, ModuleName, IntFileKind) =
    ims_imported(ModuleName, IntFileKind, ImportLocn).
make_ims_used(ImportLocn, ModuleName, IntFileKind) =
    ims_used(ModuleName, IntFileKind, ImportLocn).
make_ims_abstract_imported(ModuleName, IntFileKind) =
    ims_abstract_imported(ModuleName, IntFileKind).

make_oms_opt_imported(ModuleName, OptFileKind) =
    oms_opt_imported(ModuleName, OptFileKind).
make_ioms_opt_imported(ModuleName, OptFileKind) =
    ioms_opt_imported(ModuleName, OptFileKind).

%-----------------------------------------------------------------------------%

src_module_section_status(SrcSection, Status) :-
    (
        SrcSection = sms_interface,
        Status = item_status(status_exported, may_be_unqualified)
    ;
        SrcSection = sms_implementation,
        Status = item_status(status_local, may_be_unqualified)
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        Status = item_status(status_exported_to_submodules,
            may_be_unqualified)
    ).

int_module_section_status(IntSection, Status) :-
    (
        IntSection = ims_imported(_ModuleName, _IntFileKind, Section),
        Status = item_status(status_imported(Section), may_be_unqualified)
    ;
        IntSection = ims_used(_ModuleName, _IntFileKind, Section),
        Status = item_status(status_imported(Section), must_be_qualified)
    ;
        IntSection = ims_abstract_imported(_ModuleName, _IntFileKind),
        Status = item_status(status_abstract_imported, must_be_qualified)
    ).

opt_module_section_status(OptSection, Status) :-
    (
        OptSection = oms_opt_imported(_ModuleName, _OptFileKind),
        Status = item_status(status_opt_imported, must_be_qualified)
    ).

int_for_opt_module_section_status(IntForOptSection, Status) :-
    (
        IntForOptSection = ioms_opt_imported(_ModuleName, _OptFileKind),
        Status = item_status(status_opt_imported, must_be_qualified)
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.status.
%-----------------------------------------------------------------------------%
