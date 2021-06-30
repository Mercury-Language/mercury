%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: module_qual.m.
% Main authors: stayl, fjh.
%
% The code in this module performs two tasks.
%
% - It checks for undefined types, typeclasses, insts and modes.
%
% - It module qualifies types, typeclasses, insts and modes within declaration
%   items in the source code of the compilation unit. The heads of all
%   declarations should be module qualified as they are read in by the parser;
%   this module qualifies the bodies of those declarations.
%
%   Note that we don't qualify the parts of the augmented compilation unit
%   that derive from other modules' interface or optimization files, since
%   those parts should be read in fully module qualified already.
%
% The algorithm we use does two passes over all the items in the compilation
% unit. The first pass records the set of modules, types, typeclasses, insts
% and modes that are visible in the compilation unit. The second uses this
% information to actually do this module's job.
%
% If any type, typeclass, inst or mode used in the module is not uniquely
% module qualifiable, i.e. if we either find zero matches for it, or we find
% two or more matches for it, we can generate an error message for it.
% We do so when we are module qualifying a compilation unit; we don't when
% we are qualifying the contents of an interface file.
%
% If the --warn-interface-imports option is set, we generate warnings about
% modules imported in the interface that are not used in the interface.
%
% Note that this module is NOT the only place in the compiler that does module
% qualification. The modes of lambda expressions are qualified in modes.m,
% and predicate and function names are qualified during typecheck, with
% the results recorded during the post_typecheck phase of the purity pass.
% This is because figuring out whether e.g. a call to predicate `p' calls
% module m1's predicate p or module m2's predicate p may require knowing
% the types of the arguments in the call.
%
% Since this module does not and cannot know about the module qualification
% of predicate names, function names and function symbols, it cannot figure out
% which modules are referred to in goals. The only goals that may appear
% in the interface section of a module are in promise declarations.
% If a promise goal contains any unqualified symbols, the second pass
% leaves the symbol unchanged, but since the eventual actual qualification
% of the symbol could refer to any of the modules imported in the interface,
% we consider them *all* of them to be "used".
%
% For the same reason (we don't know what modules predicate names,
% function names and function symbols in goals may refer to), this module
% cannot implement any equivalent of --warn-interface-imports that would
% report unnecessary imports in the *implementation* section of a module.
%
% If the --warn-unused-imports option is set, then unused_imports.m
% can generate all the warnings we would, but it can generate *better*
% messages, since unlike the code here, it can report that an imported module
% is unused *anywhere* in the module. However, even if --warn-unused-imports
% *is* set, the code in unused_imports.m won't be invoked if we stop
% compilation before its normal invocation time, due to e.g. type or more
% errors. What we should do is generate warnings here; print them if we
% stop before the unused_imports pass; throw them away if we *do* get to
% that pass. We don't (yet) do this.
%
%---------------------------------------------------------------------------%

:- module parse_tree.module_qual.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type maybe_found_undef_type
    --->    did_not_find_undef_type
    ;       found_undef_type.

:- type maybe_found_undef_inst
    --->    did_not_find_undef_inst
    ;       found_undef_inst.

:- type maybe_found_undef_mode
    --->    did_not_find_undef_mode
    ;       found_undef_mode.

:- type maybe_found_undef_typeclass
    --->    did_not_find_undef_typeclass
    ;       found_undef_typeclass.

    % module_qualify_aug_comp_unit(Globals, AugCompUnit0, AugCompUnit,
    %   EventSpecMap0, EventSpecMap, MaybeContext, EventSpecFileName, MQ_Info,
    %   UndefTypes, UndefInsts, UndefModes, UndefTypeClasses, !Specs):
    %
    % AugCompUnit is AugCompUnit0 with all items module qualified
    % as much as possible; likewise for EventSpecMap0 and EventSpecMap.
    %
    % Errors in EventSpecMap0 will be reported as being for EventSpecFileName.
    %
:- pred module_qualify_aug_comp_unit(globals::in,
    aug_compilation_unit::in, aug_compilation_unit::out,
    event_spec_map::in, event_spec_map::out, string::in, mq_info::out,
    maybe_found_undef_type::out, maybe_found_undef_inst::out,
    maybe_found_undef_mode::out, maybe_found_undef_typeclass::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % module_qualify_parse_tree_int3(Globals, ParseTreeInt0, ParseTreeInt,
    %   !Specs):
    %
    % ParseTreeInt is ParseTreeInt0 with all items in the .int3 file
    % module qualified as much as possible.
    %
:- pred module_qualify_parse_tree_int3(globals::in,
    parse_tree_int3::in, parse_tree_int3::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % When we process types, typeclasses, insts or modes, we need to know
    % whether they occur in the interface of the current module. This is
    % so that if we see e.g. m1.t1 in the interface, we can mark module m1
    % as being used in the interface, so we can avoid generating a warning
    % about m1 being unused in the interface.
    %
:- type mq_in_interface
    --->    mq_not_used_in_interface
    ;       mq_used_in_interface.

    % This is called from make_hlds to qualify the mode of an argument
    % of a lambda expression.
    %
:- pred qualify_lambda_mode(mq_in_interface::in, prog_context::in,
    mer_mode::in, mer_mode::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds.m to qualify the modes in a
    % clause mode annotation.
    %
:- pred qualify_clause_mode_list(mq_in_interface::in, prog_context::in,
    list(mer_mode)::in, list(mer_mode)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds to qualify an explicit type qualification.
    %
:- pred qualify_type_qualification(mq_in_interface::in, prog_context::in,
    mer_type::in, mer_type::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

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

    % The type mq_info holds information needed for doing module qualification.
    %
:- type mq_info.

:- type maybe_suppress_found_undef
    --->    do_not_suppress_found_undef
    ;       suppress_found_undef.

:- pred mq_info_get_recompilation_info(mq_info::in,
    maybe(recompilation_info)::out) is det.
:- pred mq_info_get_found_undef_type(mq_info::in,
    maybe_found_undef_type::out) is det.
:- pred mq_info_get_found_undef_inst(mq_info::in,
    maybe_found_undef_inst::out) is det.
:- pred mq_info_get_found_undef_mode(mq_info::in,
    maybe_found_undef_mode::out) is det.
:- pred mq_info_get_found_undef_typeclass(mq_info::in,
    maybe_found_undef_typeclass::out) is det.
:- pred mq_info_get_suppress_found_undef(mq_info::in,
    maybe_suppress_found_undef::out) is det.

:- pred mq_info_set_recompilation_info(maybe(recompilation_info)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_suppress_found_undef(maybe_suppress_found_undef::in,
    mq_info::in, mq_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module parse_tree.module_qual.collect_mq_info.
:- include_module parse_tree.module_qual.id_set.
:- include_module parse_tree.module_qual.qual_errors.
:- include_module parse_tree.module_qual.qualify_items.

:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.module_qual.collect_mq_info.
:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.module_qual.qual_errors.
:- import_module parse_tree.module_qual.qualify_items.

:- import_module bool.
:- import_module io.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

module_qualify_aug_comp_unit(Globals, AugCompUnit0, AugCompUnit,
        EventSpecMap0, EventSpecMap, EventSpecFileName, !:Info,
        UndefTypes, UndefInsts, UndefModes, UndefTypeClasses, !Specs) :-
    AugCompUnit0 = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, ParseTreeModuleSrc0, AncestorIntSpecs,
        DirectIntSpecs, IndirectIntSpecs,
        PlainOptSpecs, TransOptSpecs, IntForOptSpecs, TypeRepnSpecs),

    init_mq_info(Globals, ModuleName, should_report_errors, !:Info),
    collect_mq_info_in_parse_tree_module_src(ParseTreeModuleSrc0, !Info),
    list.foldl(collect_mq_info_in_ancestor_int_spec,
        map.values(AncestorIntSpecs), !Info),
    list.foldl(collect_mq_info_in_direct_int_spec,
        map.values(DirectIntSpecs), !Info),
    module_qualify_parse_tree_module_src(
        ParseTreeModuleSrc0, ParseTreeModuleSrc, !Info, !Specs),
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, ParseTreeModuleSrc, AncestorIntSpecs,
        DirectIntSpecs, IndirectIntSpecs,
        PlainOptSpecs, TransOptSpecs, IntForOptSpecs, TypeRepnSpecs),

    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    qualify_event_specs(mq_not_used_in_interface, EventSpecFileName,
        EventSpecList0, EventSpecList, !Info, !Specs),
    map.from_assoc_list(EventSpecList, EventSpecMap),
    mq_info_get_found_undef_type(!.Info, UndefTypes),
    mq_info_get_found_undef_inst(!.Info, UndefInsts),
    mq_info_get_found_undef_mode(!.Info, UndefModes),
    mq_info_get_found_undef_typeclass(!.Info, UndefTypeClasses),

    % Warn about any unused module imports in the interface.
    % There is a special case involving type class instances that
    % we need to handle here. Consider:
    %
    %   :- module foo.
    %   :- interface.
    %
    %   :- import_module bar.
    %   :- typeclass tc1(T) <= tc2(T).
    %   :- instance tc1(unit).
    %
    % where module bar exports the instance tc2(unit). We must import
    % the module bar in the interface of the module foo in order for
    % the superclass constraint on the instance tc1(unit) to be satisfied.
    % However, at this stage of compilation we do not know that the
    % instance tc2(unit) needs to be visible. (Knowing this would require
    % a more extensive analysis of type classes and instances to be done
    % in this module.)
    %
    % In order to prevent the import of the module bar being erroneously
    % reported as unused, we make the conservative assumption that any
    % imported module that exports a type class instance is used in
    % the interface of the importing module, except if the importing
    % module itself exports _no_ type class instances.
    mq_info_get_as_yet_unused_interface_modules(!.Info, UnusedImportsMap0),
    mq_info_get_exported_instances_flag(!.Info, ModuleExportsInstances),
    (
        ModuleExportsInstances = yes,
        mq_info_get_imported_instance_modules(!.Info, InstanceImports),
        map.delete_list(set.to_sorted_list(InstanceImports),
            UnusedImportsMap0, UnusedImportsMap)
    ;
        ModuleExportsInstances = no,
        UnusedImportsMap = UnusedImportsMap0
    ),
    globals.lookup_bool_option(Globals, warn_interface_imports,
        WarnInterfaceImports),
    (
        WarnInterfaceImports = no
    ;
        WarnInterfaceImports = yes,
        map.to_assoc_list(UnusedImportsMap, UnusedImports),
        list.foldl(warn_unused_interface_import(ModuleName), UnusedImports,
            !Specs)
    ).

module_qualify_parse_tree_int3(Globals, OrigParseTreeInt3, ParseTreeInt3,
        !Specs) :-
    ModuleName = OrigParseTreeInt3 ^ pti3_module_name,
    init_mq_info(Globals, ModuleName, should_not_report_errors, Info0),
    collect_mq_info_in_parse_tree_int3(int3_as_src, OrigParseTreeInt3,
        Info0, Info1),
    module_qualify_parse_tree_int3(OrigParseTreeInt3, ParseTreeInt3,
        Info1, _Info, !Specs).

%---------------------------------------------------------------------------%

qualify_lambda_mode(InInt, Context, Mode0, Mode, !Info, !Specs) :-
    ErrorContext = mqec_lambda_expr(Context),
    qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs).

qualify_clause_mode_list(InInt, Context, Modes0, Modes, !Info, !Specs) :-
    ErrorContext = mqec_clause_mode_annotation(Context),
    qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs).

qualify_type_qualification(InInt, Context, Type0, Type, !Info, !Specs) :-
    ErrorContext = mqec_type_qual(Context),
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs).

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
        get_partial_qualifiers_acc(InInt, Parent, unqualified(Child),
            ModuleIdSet, [], PartialQualifiers)
    ).

:- pred get_partial_qualifiers_acc(mq_in_interface::in,
    module_name::in, module_name::in, module_id_set::in,
    list(module_name)::in, list(module_name)::out) is det.

get_partial_qualifiers_acc(InInt, ImplicitPart, ExplicitPart, ModuleIdSet,
        !Qualifiers) :-
    % If the ImplicitPart module was imported, rather than just being used,
    % then insert the ExplicitPart module into the list of valid partial
    % qualifiers.
    ( if
        parent_module_is_imported(InInt, ImplicitPart,
            ExplicitPart, ModuleIdSet)
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
        get_partial_qualifiers_acc(InInt, NextImplicitPart, NextExplicitPart,
            ModuleIdSet, !Qualifiers)
    ;
        ImplicitPart = unqualified(_)
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred update_recompilation_info(
    pred(recompilation_info, recompilation_info)::in(pred(in, out) is det),
    mq_info::in, mq_info::out) is det.

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

    % If the current item is in the interface, remove its module name from
    % the list of modules not used in the interface, and if the module name
    % is itself module-qualified, mark its ancestor modules as used as well.
    %
:- pred mq_info_set_module_used(mq_in_interface::in, module_name::in,
    mq_info::in, mq_info::out) is det.

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
%
% Access and initialisation predicates.
%

:- type maybe_should_report_errors
    --->    should_not_report_errors
    ;       should_report_errors.

    % We process only the interface, so we will warn only about unused
    % imports in parent's INTERFACE sections.
:- type maybe_warn_unused_imports_in_parents
    --->    should_not_warn_unused_imports_in_parents
    ;       should_warn_unused_imports_in_parents.

:- type mq_sub_info
    --->    mq_sub_info(
                % The name of the current module.
                mqsi_this_module                :: module_name,
                mqsi_globals                    :: globals,

                % Modules which have been imported or used, i.e. the ones
                % for which there was a `:- import_module' or `:- use_module'
                % declaration in this module, plus the implicitly imported
                % module builtin.m. We use the contents of this field
                % to decide whether to add "No module named xyz has been
                % imported" to error messages when we find a reference
                % to an undefined type, inst, mode, pred etc.
                mqsi_imported_modules           :: set(module_name),

                % Modules from which `:- instance' declarations have
                % been imported.
                mqsi_imported_instance_modules  :: set(module_name),

                % Does this module export any type class instances?
                mqsi_exported_instances_flag    :: bool,

                % Are there any undefined types, insts, modes or typeclasses?
                mqsi_found_undef_type           :: maybe_found_undef_type,
                mqsi_found_undef_inst           :: maybe_found_undef_inst,
                mqsi_found_undef_mode           :: maybe_found_undef_mode,
                mqsi_found_undef_typeclass      :: maybe_found_undef_typeclass,

                % Do we want to suppress the recording of an undef type, inst,
                % mode or typeclass, as such? We sometimes do, when the error
                % won't prevent us from continuing on to later compiler passes.
                % (We still generate error messages in such instances, of
                % course.)
                mqsi_suppress_found_undef       :: maybe_suppress_found_undef,

                % Do we want to report errors.
                mqsi_should_report_errors       :: maybe_should_report_errors,
                mqsi_warn_parents_imports       ::
                                        maybe_warn_unused_imports_in_parents,

                % The number of errors found.
                mqsi_num_errors                 :: int
            ).

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

:- pred init_mq_info(globals::in, module_name::in,
    maybe_should_report_errors::in, mq_info::out) is det.

init_mq_info(Globals, ModuleName, ReportErrors, Info) :-
    set.init(InstanceModules),
    ExportedInstancesFlag = no,
    globals.lookup_bool_option(Globals, warn_interface_imports_in_parents,
        WarnInterfaceImportsInParents),
    % All the warnings generated by module_qual.*m for unused modules
    % are for modules imported in the interface; see the comment at the
    % top of this module.
    (
        WarnInterfaceImportsInParents = no,
        WarnUnusedImportsInParents = should_not_warn_unused_imports_in_parents
    ;
        WarnInterfaceImportsInParents = yes,
        WarnUnusedImportsInParents = should_warn_unused_imports_in_parents
    ),
    % This module is always implicitly imported into every module,
    % and users may use the entities it defines without qualification.
    % Users shouldn't write code that refers to entities defined in
    % other implicitly imported modules without explicitly importing them,
    % since the implicit import mechanism is to give *compiler writers*
    % access to those modules, not the users directly. Therefore I (zs) think
    % that if users write code that refers to entities defined in other
    % implicitly imported modules, including private_builtin, but get
    % the names of those entities wrong, it is ok to tell them that the
    % affected module hasn't been (explicitly) imported.
    set.list_to_set([mercury_public_builtin_module], ImportedOrUsedModules),
    SubInfo = mq_sub_info(ModuleName, Globals, ImportedOrUsedModules,
        InstanceModules, ExportedInstancesFlag,
        did_not_find_undef_type, did_not_find_undef_inst,
        did_not_find_undef_mode, did_not_find_undef_typeclass,
        do_not_suppress_found_undef,
        ReportErrors, WarnUnusedImportsInParents, 0),

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

%---------------------------------------------------------------------------%

:- pred mq_info_get_modules(mq_info::in, module_id_set::out) is det.
:- pred mq_info_get_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_insts(mq_info::in, inst_id_set::out) is det.
:- pred mq_info_get_modes(mq_info::in, mode_id_set::out) is det.
:- pred mq_info_get_classes(mq_info::in, class_id_set::out) is det.
:- pred mq_info_get_as_yet_unused_interface_modules(mq_info::in,
    module_names_contexts::out) is det.
% mq_info_get_recompilation_info is exported

:- pred mq_info_get_this_module(mq_info::in, module_name::out) is det.
:- pred mq_info_get_globals(mq_info::in, globals::out) is det.
:- pred mq_info_get_imported_modules(mq_info::in, set(module_name)::out)
    is det.
:- pred mq_info_get_imported_instance_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_exported_instances_flag(mq_info::in, bool::out) is det.
% mq_info_get_type_error_flag is exported
% mq_info_get_mode_error_flag is exported
:- pred mq_info_get_should_report_errors(mq_info::in,
    maybe_should_report_errors::out) is det.
:- pred mq_info_get_should_warn_unused_imports_in_parents(mq_info::in,
    maybe_warn_unused_imports_in_parents::out) is det.

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
mq_info_get_globals(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_globals.
mq_info_get_imported_modules(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_imported_modules.
mq_info_get_imported_instance_modules(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_imported_instance_modules.
mq_info_get_exported_instances_flag(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_exported_instances_flag.
mq_info_get_found_undef_type(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_found_undef_type.
mq_info_get_found_undef_inst(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_found_undef_inst.
mq_info_get_found_undef_mode(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_found_undef_mode.
mq_info_get_found_undef_typeclass(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_found_undef_typeclass.
mq_info_get_suppress_found_undef(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_suppress_found_undef.
mq_info_get_should_report_errors(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_should_report_errors.
mq_info_get_should_warn_unused_imports_in_parents(Info, X) :-
    X = Info ^ mqi_sub_info ^ mqsi_warn_parents_imports.

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
% mq_info_get_recompilation_info is exported

:- pred mq_info_set_imported_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_imported_instance_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_exported_instances_flag(bool::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_found_undef_type(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_found_undef_inst(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_found_undef_mode(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_found_undef_typeclass(mq_info::in, mq_info::out) is det.

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
mq_info_set_found_undef_type(!Info) :-
    X = found_undef_type,
    !Info ^ mqi_sub_info ^ mqsi_found_undef_type := X.
mq_info_set_found_undef_inst(!Info) :-
    X = found_undef_inst,
    !Info ^ mqi_sub_info ^ mqsi_found_undef_inst := X.
mq_info_set_found_undef_mode(!Info) :-
    X = found_undef_mode,
    !Info ^ mqi_sub_info ^ mqsi_found_undef_mode := X.
mq_info_set_found_undef_typeclass(!Info) :-
    X = found_undef_typeclass,
    !Info ^ mqi_sub_info ^ mqsi_found_undef_typeclass := X.
mq_info_set_suppress_found_undef(X, !Info) :-
    !Info ^ mqi_sub_info ^ mqsi_suppress_found_undef := X.

%---------------------------------------------------------------------------%

:- pred get_mq_debug_output_stream(mq_info::in, io.text_output_stream::out,
    io::di, io::uo) is det.

get_mq_debug_output_stream(Info, DebugStream, !IO) :-
    mq_info_get_globals(Info, Globals),
    mq_info_get_this_module(Info, ModuleName),
    get_debug_output_stream(Globals, ModuleName, DebugStream, !IO).

%---------------------------------------------------------------------------%

:- pred mq_info_record_undef_mq_id(id_type::in,
    mq_info::in, mq_info::out) is det.

mq_info_record_undef_mq_id(IdType, !Info) :-
    mq_info_get_suppress_found_undef(!.Info, SuppressFoundUndef),
    (
        SuppressFoundUndef = suppress_found_undef
    ;
        SuppressFoundUndef = do_not_suppress_found_undef,
        (
            IdType = type_id,
            mq_info_set_found_undef_type(!Info)
        ;
            IdType = inst_id,
            mq_info_set_found_undef_inst(!Info)
        ;
            IdType = mode_id,
            mq_info_set_found_undef_mode(!Info)
        ;
            IdType = class_id,
            mq_info_set_found_undef_typeclass(!Info)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.
%---------------------------------------------------------------------------%
