%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mq_info.m.
%
% This module defines the main data structure used by the module_qual package.
%

:- module parse_tree.module_qual.mq_info.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_parse_tree.
:- import_module recompilation.
:- import_module recompilation.record_uses.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set_tree234.

%---------------------------------------------------------------------------%

:- type maybe_should_report_errors
    --->    should_not_report_errors
    ;       should_report_errors.

:- type maybe_suppress_found_undef
    --->    do_not_suppress_found_undef
    ;       suppress_found_undef.

%---------------------------------------------------------------------------%

:- type mq_info.

:- pred init_mq_info(globals::in, module_name::in,
    maybe_should_report_errors::in, mq_info::out) is det.

%---------------------------------------------------------------------------%

:- pred mq_info_get_modules(mq_info::in, module_id_set::out) is det.
:- pred mq_info_get_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_insts(mq_info::in, inst_id_set::out) is det.
:- pred mq_info_get_modes(mq_info::in, mode_id_set::out) is det.
:- pred mq_info_get_classes(mq_info::in, class_id_set::out) is det.
:- pred mq_info_get_as_yet_unused_interface_modules(mq_info::in,
    module_names_contexts::out) is det.
:- pred mq_info_get_recompilation_info(mq_info::in,
    maybe(recompilation_info)::out) is det.

:- pred mq_info_get_this_module(mq_info::in, module_name::out) is det.
:- pred mq_info_get_imported_modules(mq_info::in,
    set_tree234(module_name)::out) is det.
:- pred mq_info_get_imported_instance_modules(mq_info::in,
    set_tree234(module_name)::out) is det.
:- pred mq_info_get_exported_instances_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_undef_types(mq_info::in,
    set_tree234(type_ctor)::out) is det.
:- pred mq_info_get_undef_insts(mq_info::in,
    set_tree234(inst_ctor)::out) is det.
:- pred mq_info_get_undef_modes(mq_info::in,
    set_tree234(mode_ctor)::out) is det.
:- pred mq_info_get_undef_typeclasses(mq_info::in,
    set_tree234(sym_name_arity)::out) is det.
:- pred mq_info_get_suppress_found_undef(mq_info::in,
    maybe_suppress_found_undef::out) is det.
:- pred mq_info_get_should_report_errors(mq_info::in,
    maybe_should_report_errors::out) is det.

:- pred mq_info_set_modules(module_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_types(type_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_insts(inst_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_modes(mode_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_classes(class_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_as_yet_unused_interface_modules(module_names_contexts::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_recompilation_info(maybe(recompilation_info)::in,
    mq_info::in, mq_info::out) is det.

:- pred mq_info_set_imported_modules(set_tree234(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_imported_instance_modules(set_tree234(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_exported_instances_flag(bool::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_undef_types(set_tree234(type_ctor)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_undef_insts(set_tree234(inst_ctor)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_undef_modes(set_tree234(mode_ctor)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_undef_typeclasses(set_tree234(sym_name_arity)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_suppress_found_undef(maybe_suppress_found_undef::in,
    mq_info::in, mq_info::out) is det.

%---------------------------------------------------------------------------%

    % The type partial_qualifier_info holds info need for computing which
    % partial quantifiers are visible -- see get_partial_qualifiers/3.
    %
:- type partial_qualifier_info.

:- pred mq_info_get_partial_qualifier_info(mq_info::in,
    partial_qualifier_info::out) is det.

    % Suppose we are processing a definition which defines the symbol
    % foo.bar.baz.quux/1. Then we insert the following symbols
    % into the symbol table:
    %   - if the current value of the NeedQual flag at this point
    %       is `may_be_unqualified',
    %       i.e. module `foo.bar.baz' was imported,
    %       then we insert the fully unqualified symbol quux/1;
    %   - if module `foo.bar.baz' occurs in the "imported" section,
    %       i.e. if module `foo.bar' was imported,
    %       then we insert the partially qualified symbol baz.quux/1;
    %   - if module `foo.bar' occurs in the "imported" section,
    %       i.e. if module `foo' was imported,
    %       then we insert the partially qualified symbol bar.baz.quux/1;
    %   - we always insert the fully qualified symbol foo.bar.baz.quux/1.
    %
    % The predicate `get_partial_qualifiers' returns all of the
    % partial qualifiers for which we need to insert definitions,
    % i.e. all the ones which are visible. For example,
    % given as input `foo.bar.baz', it returns a list containing
    %   (1) `baz', iff `foo.bar' is imported, and
    %   (2) `bar.baz', iff `foo' is imported.
    % Note that the caller will still need to handle the fully-qualified
    % and fully-unqualified versions separately.
    %
:- pred get_partial_qualifiers(mq_in_interface::in, module_name::in,
    partial_qualifier_info::in, list(module_name)::out) is det.

%---------------------------------------------------------------------------%

:- pred update_recompilation_info(
    pred(recompilation_info, recompilation_info)::in(pred(in, out) is det),
    mq_info::in, mq_info::out) is det.

    % If the current item is in the interface, remove its module name from
    % the list of modules not used in the interface, and if the module name
    % is itself module-qualified, mark its ancestor modules as used as well.
    %
:- pred mq_info_set_module_used(mq_in_interface::in, module_name::in,
    mq_info::in, mq_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.

:- import_module map.

%---------------------------------------------------------------------------%

    % The main data structure used by the code that does module qualification,
    % and its initialisation and access predicates.
    %
:- type mq_info
    --->    mq_info(
                % Keep the size of the main mq_info structure at eight fields,
                % as this allows Boehm gc to allocate memory blocks that don't
                % have wasted unused space.
                mqi_sub_info                    :: mq_sub_info,

                % Sets of all modules, types, insts, modes, and typeclasses
                % visible in this module.
                mqi_modules                     :: module_id_set,
                mqi_types                       :: type_id_set,
                mqi_insts                       :: inst_id_set,
                mqi_modes                       :: mode_id_set,
                mqi_classes                     :: class_id_set,

                % Map each modules known to be imported in the interface
                % that is not yet known to be needed in the interface
                % to the location (or sometimes, locations) of the import.
                mqi_as_yet_unused_interface_modules :: module_names_contexts,

                mqi_maybe_recompilation_info    :: maybe(recompilation_info)
            ).

:- type mq_sub_info
    --->    mq_sub_info(
                % The name of the current module.
                mqsi_this_module                :: module_name,

                % Modules which have been imported or used, i.e. the ones
                % for which there was a `:- import_module' or `:- use_module'
                % declaration in this module, plus the implicitly imported
                % module builtin.m. We use the contents of this field
                % to decide whether to add "No module named xyz has been
                % imported" to error messages when we find a reference
                % to an undefined type, inst, mode, pred etc.
                mqsi_imported_modules           :: set_tree234(module_name),

                % Modules from which `:- instance' declarations have
                % been imported.
                mqsi_imported_instance_modules  :: set_tree234(module_name),

                % Does this module export any type class instances?
                mqsi_exported_instances_flag    :: bool,

                % What types, insts, modes or typeclasses are undefined?
                mqsi_undef_types                :: set_tree234(type_ctor),
                mqsi_undef_insts                :: set_tree234(inst_ctor),
                mqsi_undef_modes                :: set_tree234(mode_ctor),
                mqsi_undef_typeclasses          :: set_tree234(sym_name_arity),

                % Do we want to suppress the recording of an undef type, inst,
                % mode or typeclass, as such? We sometimes do, when the error
                % won't prevent us from continuing on to later compiler passes.
                % (We still generate error messages in such instances, of
                % course.)
                mqsi_suppress_found_undef       :: maybe_suppress_found_undef,

                % Do we want to report errors.
                mqsi_should_report_errors       :: maybe_should_report_errors,

                % The number of errors found.
                mqsi_num_errors                 :: int
            ).

%---------------------------------------------------------------------------%

init_mq_info(Globals, ModuleName, ReportErrors, Info) :-
    InstanceModules = set_tree234.init,
    ExportedInstancesFlag = no,
    % mercury_public_builtin_module is always implicitly imported into
    % every module, and users may use the entities it defines without
    % qualification. Users shouldn't write code that refers to entities
    % defined in other implicitly imported modules without explicitly
    % importing them, since the implicit import mechanism is to give
    % *compiler writers* access to those modules, not the users directly.
    % Therefore I (zs) think that if users write code that refers to entities
    % defined in other implicitly imported modules, including private_builtin,
    % but get the names of those entities wrong, it is ok to tell them that the
    % affected module hasn't been (explicitly) imported.
    ImportedOrUsedModules =
        set_tree234.make_singleton_set(mercury_public_builtin_module),
    SubInfo = mq_sub_info(ModuleName, ImportedOrUsedModules,
        InstanceModules, ExportedInstancesFlag,
        set_tree234.init, set_tree234.init, set_tree234.init, set_tree234.init,
        do_not_suppress_found_undef, ReportErrors, 0),

    id_set_init(ModuleIdSet),
    id_set_init(TypeIdSet),
    id_set_init(InstIdSet),
    id_set_init(ModeIdSet),
    id_set_init(ClassIdSet),
    map.init(AsYetUnusedInterfaceModules),
    globals.lookup_bool_option(Globals, smart_recompilation,
        SmartRecompilation),
    (
        SmartRecompilation = no,
        MaybeRecompInfo = no
    ;
        SmartRecompilation = yes,
        MaybeRecompInfo = yes(init_recompilation_info(ModuleName))
    ),
    Info = mq_info(SubInfo, ModuleIdSet,
        TypeIdSet, InstIdSet, ModeIdSet, ClassIdSet,
        AsYetUnusedInterfaceModules, MaybeRecompInfo).

%---------------------%

mq_info_get_modules(Info, X) :-
    X = Info ^ mqi_modules.
mq_info_get_types(Info, X) :-
    X = Info ^ mqi_types.
mq_info_get_insts(Info, X) :-
    X = Info ^ mqi_insts.
mq_info_get_modes(Info, X) :-
    X = Info ^ mqi_modes.
mq_info_get_classes(Info, X) :-
    X = Info ^ mqi_classes.
mq_info_get_as_yet_unused_interface_modules(Info, X) :-
    X = Info ^ mqi_as_yet_unused_interface_modules.
mq_info_get_recompilation_info(Info, X) :-
    X = Info ^ mqi_maybe_recompilation_info.

mq_info_get_this_module(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_this_module.
mq_info_get_imported_modules(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_imported_modules.
mq_info_get_imported_instance_modules(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_imported_instance_modules.
mq_info_get_exported_instances_flag(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_exported_instances_flag.
mq_info_get_undef_types(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_undef_types.
mq_info_get_undef_insts(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_undef_insts.
mq_info_get_undef_modes(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_undef_modes.
mq_info_get_undef_typeclasses(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_undef_typeclasses.
mq_info_get_suppress_found_undef(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_suppress_found_undef.
mq_info_get_should_report_errors(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_should_report_errors.

mq_info_set_modules(X, !Info) :-
    !Info ^ mqi_modules := X.
mq_info_set_types(X, !Info) :-
    !Info ^ mqi_types := X.
mq_info_set_insts(X, !Info) :-
    !Info ^ mqi_insts := X.
mq_info_set_modes(X, !Info) :-
    !Info ^ mqi_modes := X.
mq_info_set_classes(X, !Info) :-
    !Info ^ mqi_classes := X.
mq_info_set_as_yet_unused_interface_modules(X, !Info) :-
    !Info ^ mqi_as_yet_unused_interface_modules := X.
mq_info_set_recompilation_info(X, !Info) :-
    !Info ^ mqi_maybe_recompilation_info := X.

mq_info_set_imported_modules(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_imported_modules := X.
mq_info_set_imported_instance_modules(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_imported_instance_modules := X.
mq_info_set_exported_instances_flag(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_exported_instances_flag := X.
mq_info_set_undef_types(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_undef_types := X.
mq_info_set_undef_insts(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_undef_insts := X.
mq_info_set_undef_modes(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_undef_modes := X.
mq_info_set_undef_typeclasses(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_undef_typeclasses := X.
mq_info_set_suppress_found_undef(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_suppress_found_undef := X.

%---------------------------------------------------------------------------%

:- type partial_qualifier_info
    --->    partial_qualifier_info(module_id_set).

mq_info_get_partial_qualifier_info(MQInfo, QualifierInfo) :-
    mq_info_get_modules(MQInfo, ModuleIdSet),
    QualifierInfo = partial_qualifier_info(ModuleIdSet).

get_partial_qualifiers(InInt, ModuleName, PartialQualInfo,
        PartialQualifiers) :-
    PartialQualInfo = partial_qualifier_info(ModuleIdSet),
    (
        ModuleName = unqualified(_),
        PartialQualifiers = []
    ;
        ModuleName = qualified(Parent, Child),
        get_partial_qualifiers_acc(InInt, ModuleIdSet,
            Parent, unqualified(Child), [], PartialQualifiers)
    ).

:- pred get_partial_qualifiers_acc(mq_in_interface::in, module_id_set::in,
    module_name::in, module_name::in,
    list(module_name)::in, list(module_name)::out) is det.

get_partial_qualifiers_acc(InInt, ModuleIdSet, ImplicitPart, ExplicitPart,
        !Qualifiers) :-
    % If the ImplicitPart module was imported, rather than just being used,
    % then insert the ExplicitPart module into the list of valid partial
    % qualifiers.
    ( if
        parent_module_is_imported(InInt, ModuleIdSet,
            ImplicitPart, ExplicitPart)
    then
        !:Qualifiers = [ExplicitPart | !.Qualifiers]
    else
        true
    ),
    % Recursively try to add the other possible partial qualifiers.
    (
        ImplicitPart = qualified(Parent, Child),
        NextImplicitPart = Parent,
        NextExplicitPart = add_outermost_qualifier(Child, ExplicitPart),
        get_partial_qualifiers_acc(InInt, ModuleIdSet,
            NextImplicitPart, NextExplicitPart, !Qualifiers)
    ;
        ImplicitPart = unqualified(_)
    ).

%---------------------------------------------------------------------------%

update_recompilation_info(Pred, !Info) :-
    mq_info_get_recompilation_info(!.Info, MaybeRecompInfo0),
    (
        MaybeRecompInfo0 = yes(RecompInfo0),
        Pred(RecompInfo0, RecompInfo),
        mq_info_set_recompilation_info(yes(RecompInfo), !Info)
    ;
        MaybeRecompInfo0 = no
    ).

%---------------------%

mq_info_set_module_used(InInt, ModuleName, !Info) :-
    (
        InInt = mq_used_in_interface,
        mq_info_get_as_yet_unused_interface_modules(!.Info, AsYetUnused0),
        ( if map.remove(ModuleName, _, AsYetUnused0, AsYetUnused) then
            mq_info_set_as_yet_unused_interface_modules(AsYetUnused, !Info)
        else
            % ModuleName was not in AsYetUnused0.
            true
        ),
        (
            ModuleName = qualified(ParentModule, _),
            mq_info_set_module_used(InInt, ParentModule, !Info)
        ;
            ModuleName = unqualified(_)
        )
    ;
        InInt = mq_not_used_in_interface
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.mq_info.
%---------------------------------------------------------------------------%
