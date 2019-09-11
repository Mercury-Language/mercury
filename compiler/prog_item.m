%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_item.m.
% Main author: fjh.
%
% This module, together with prog_data, defines a data structure for
% representing Mercury programs.
%
% This data structure specifies basically the same information as is
% contained in the source code, but in a parse tree rather than a flat file.
% This module defines the parts of the parse tree that are *not* needed
% by the various compiler backends; parts of the parse tree that
% are needed by the backends are contained in prog_data.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_item.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module recompilation.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------------%
%
% The parse_tree_{src,int,opt} types define the ASTs we use for source files,
% interface files and optimization files respectively.
%
% Nested submodules may appear in source files, but not in interface files
% or optimization files.
%
% We use cords of items instead of lists of items where we may need to add
% items to an already-existing partial parse tree.
%
% The contexts of module declarations below may be term.context_init
% if the actual context isn't known, but if the recorded context is
% not term.context_init, then it is valid.

:- type parse_tree_src
    --->    parse_tree_src(
                pts_module_name             :: module_name,

                % The context of the `:- module' declaration.
                pts_module_name_context     :: prog_context,

                % The contents of the module.
                pts_components              :: cord(module_component)
            ).

:- type module_component
    --->    mc_section(
                mcs_module_name             :: module_name,
                mcs_section_kind            :: module_section,

                % The context of the `:- interface' or `:- implementation'
                % declaration.
                mcs_section_context         :: prog_context,

                mcs_includes                :: cord(item_include),
                mcs_avails                  :: cord(item_avail),
                pti_fims                    :: cord(item_fim),
                mcs_items                   :: cord(item)
            )
    ;       mc_nested_submodule(
                % The name of the *including* module.
                mcns_module_name            :: module_name,

                % What kind of section is the submodule in?
                mcns_in_section_kind        :: module_section,

                % The context of the section that the submodule is in.
                mcns_in_section_context     :: prog_context,

                % The submodule itself.
                mcns_submodule              :: parse_tree_src
            ).

    % When comp_unit_interface.m creates the contents of an interface file,
    % it will always set the maybe_version_numbers field of that interface file
    % to `no_version_numbers'. If the value of that field is needed,
    % it will be filled in by the actually_write_interface_file predicate
    % in write_module_interface_files.m, which (unlike comp_unit_interface.m)
    % has access to the I/O state to read in the *previous* version
    % of that interface file.
:- type maybe_version_numbers
    --->    no_version_numbers
    ;       version_numbers(version_numbers).

    % The generic representation of all the different kinds of interface files.
    % XXX It should be replaced by the kind-specific representations below.
:- type parse_tree_int
    --->    parse_tree_int(
                pti_module_name             :: module_name,
                pti_int_file_kind           :: int_file_kind,

                % The context of the `:- module' declaration.
                pti_module_name_context     :: prog_context,

                % For .int0, .int and .int2; not for .int3.
                pti_maybe_version_numbers   :: maybe_version_numbers,

                % `:- include_module' declarations in the interface and
                % in the implementation.
                pti_int_includes            :: list(item_include),
                pti_imp_includes            :: list(item_include),

                % `:- import_module' and `:- use_module' declarations
                % in the interface and in the implementation.
                pti_int_avails              :: list(item_avail),
                pti_imp_avails              :: list(item_avail),

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti_int_fims                :: list(item_fim),
                pti_imp_fims                :: list(item_fim),

                % Items in the interface and in the implementation.
                pti_int_items               :: list(item),
                pti_imp_items               :: list(item)
            ).

% The representations specific to .int0, .int, .int2 and .int3 files.
% XXX We should replace the lists of items of various kinds with data
% structures that encode uniqueness properties, such as "each type constructor
% may be defined only once". Maps from primary keys such as type_ctors,
% or symnames/arity pairs in general, would work for this.

    % A version of parse_tree_int specialized to hold the contents of
    % .int0 files.
:- type parse_tree_int0
    --->    parse_tree_int0(
                pti0_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti0_module_name_context    :: prog_context,

                pti0_maybe_version_numbers  :: maybe_version_numbers,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface and implementation.
                pti0_int_included_modules   :: set(module_name),
                pti0_imp_included_modules   :: set(module_name),

                % The set of modules mentioned in `:- import_module'
                % declarations in the interface and implementation.
                pti0_int_imported_modules   :: set(module_name),
                pti0_int_used_modules       :: set(module_name),
                pti0_imp_imported_modules   :: set(module_name),
                pti0_imp_used_modules       :: set(module_name),

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti0_int_fims               :: set(fim_spec),
                pti0_imp_fims               :: set(fim_spec),

                % Items of various kinds in the interface.
                % XXX For the consumers of the .int0 file, in most cases
                % it makes no difference whether an item was in the parent's
                % interface or implementation section. We should make that
                % distinction here ONLY when we have to.
                pti0_int_type_defns         :: type_ctor_defn_map,
                pti0_int_inst_defns         :: inst_ctor_defn_map,
                pti0_int_mode_defns         :: mode_ctor_defn_map,
                pti0_int_typeclasses        :: list(item_typeclass_info),
                pti0_int_instances          :: list(item_instance_info),
                pti0_int_pred_decls         :: list(item_pred_decl_info),
                pti0_int_mode_decls         :: list(item_mode_decl_info),
                pti0_int_foreign_enums      :: type_ctor_foreign_enum_map,
                pti0_int_pragmas            :: list(item_pragma_info),
                pti0_int_promises           :: list(item_promise_info),
                % XXX We will probably need a list of item_type_repn_infos.

                % Items of various kinds in the implementation section.
                pti0_imp_type_defns         :: type_ctor_defn_map,
                pti0_imp_inst_defns         :: inst_ctor_defn_map,
                pti0_imp_mode_defns         :: mode_ctor_defn_map,
                pti0_imp_typeclasses        :: list(item_typeclass_info),
                pti0_imp_instances          :: list(item_instance_info),
                pti0_imp_pred_decls         :: list(item_pred_decl_info),
                pti0_imp_mode_decls         :: list(item_mode_decl_info),
                pti0_imp_foreign_enums      :: type_ctor_foreign_enum_map,
                pti0_imp_pragmas            :: list(item_pragma_info),
                pti0_imp_promises           :: list(item_promise_info)
                % XXX We will probably need a list of item_type_repn_infos.
            ).

    % A version of parse_tree_int specialized to hold the contents of
    % .int files.
:- type parse_tree_int1
    --->    parse_tree_int1(
                pti1_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti1_module_name_context    :: prog_context,

                pti1_maybe_version_numbers  :: maybe_version_numbers,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface and implementation.
                pti1_int_included_modules   :: set(module_name),
                pti1_imp_included_modules   :: set(module_name),

                % The set of modules mentioned in `:- import_module'
                % declarations in the interface and implementation.
                pti1_int_used_modules       :: set(module_name),
                pti1_imp_used_modules       :: set(module_name),

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti1_int_fims               :: set(fim_spec),
                pti1_imp_fims               :: set(fim_spec),

                % Items of various kinds in the interface.
                pti1_type_defns             :: type_ctor_defn_map,
                pti1_inst_defns             :: inst_ctor_defn_map,
                pti1_mode_defns             :: mode_ctor_defn_map,
                pti1_int_typeclasses        :: list(item_typeclass_info),
                pti1_int_instances          :: list(item_instance_info),
                pti1_int_pred_decls         :: list(item_pred_decl_info),
                pti1_int_mode_decls         :: list(item_mode_decl_info),
                pti1_int_foreign_enum_specs :: type_ctor_foreign_enum_map,
                pti1_int_pragmas            :: list(item_pragma_info),
                pti1_int_promises           :: list(item_promise_info),

                pti1_int_type_repns         :: type_ctor_repn_map,

                % Items of various kinds in the implementation.
                % Should not be needed after we switch to use type_repn items.
                pti1_imp_type_defns         :: type_ctor_defn_map,
                pti1_imp_foreign_enum_specs :: type_ctor_foreign_enum_map,
                pti1_imp_typeclasses        :: list(item_typeclass_info)
            ).

    % A version of parse_tree_int specialized to hold the contents of
    % .int2 files.
:- type parse_tree_int2
    --->    parse_tree_int2(
                pti2_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti2_module_name_context    :: prog_context,

                % XXX While it is clear that .int files need version number
                % fields while .int3 files do not, I (zs) don't see any
                % clear argument either way for .int2 files. Having
                % the field here preserves old behavior.
                pti2_maybe_version_numbers  :: maybe_version_numbers,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface.
                pti2_int_included_modules   :: set(module_name),

                % The set of modules mentioned in `:- import_module'
                % declarations in the interface.
                pti2_int_used_modules       :: set(module_name),

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti2_int_fims               :: set(fim_spec),
                pti2_imp_fims               :: set(fim_spec),

                % Items of various kinds in the interface.
                pti2_type_defns             :: type_ctor_defn_map,
                pti2_inst_defns             :: inst_ctor_defn_map,
                pti2_mode_defns             :: mode_ctor_defn_map,
                pti2_int_typeclasses        :: list(item_typeclass_info),
                pti2_int_instances          :: list(item_instance_info),
                pti2_int_type_repns         :: type_ctor_repn_map,

                % Items of various kinds in the implementation.
                % Should not be needed after we switch to use type_repn items.
                pti2_imp_type_defns         :: type_ctor_defn_map
            ).

    % A version of parse_tree_int specialized to hold the contents of
    % .int3 files.
:- type parse_tree_int3
    --->    parse_tree_int3(
                pti3_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti3_module_name_context    :: prog_context,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface.
                pti3_included_modules       :: set(module_name),

                % The set of modules mentioned in `:- import_module'
                % declarations in the interface.
                pti3_imported_modules       :: set(module_name),

                % Items of various kinds in the interface.
                pti3_type_defns             :: type_ctor_defn_map,
                pti3_inst_defns             :: inst_ctor_defn_map,
                pti3_mode_defns             :: mode_ctor_defn_map,
                pti3_typeclasses            :: list(item_typeclass_info),
                pti3_instances              :: list(item_instance_info),
                pti3_type_repns             :: type_ctor_repn_map
            ).

%-----------------------------------------------------------------------------%
%
% The intended semantics of a type_ctor_defn_map is a map of
% all the type constructors defined in a given SECTION of a given
% interface file to all its definitions in that section.
%
% There are four intended uses of a type_ctor_defn_map. The most
% important is the fourth one.
%
% One is to eliminate unnecessary items from interface files.
% For example, library/set.m contains two definitions of the set
% type constructor: an abstract definition in the publicly documented
% interface section, and an actual du definition in another interface
% section that we do not include in the automatically generated
% documentation but we *do* export to other modules. In situations
% like this, the abstract definition is redundant. Never including it
% in an interface file lets that interface file to remain unchanged
% in the event that the user deletes the abstract definition from
% the source file as well.
%
% The second use is to canonicalize the parts of interface files
% containing type definitions.
%
% The third use is to help deal with sets of definitions make sense.
% There are many rules that a set of definitions for a given type
% constructor must meet (such as "there may be at most one definition
% for a type constructor that is a du, equivalence or solver definition),
% and bugs may manifest themselves as violations of these rules.
%
% We have a choice in when these violations are detected.
%
% - If we allow the inclusion of inconsistent sets of type definitions
%   in interface files, then we must detect and handle these
%   inconsistencies every time a compiler invocation reads that interface
%   file. These invocations won't generate error messages for these
%   inconsistencies since the type constructor won't be local, but
%   they may generate messages for other "errors" that look like errors
%   only because the compiler's resolution of the inconsistency (i.e.
%   its choice of which type definitions to keep and which to throw out)
%   differs from the programmer's choice.
%
% - If we do NOT allow the inclusion of inconsistent sets of type
%   definitions in interface files, then we must report any violations
%   at interface file construction time, and make them cause that
%   construction to fail. Printing such error messages to stdout
%   instead of the module's .err file is less than ideal, but
%   this early detection can avoid avalanches of misleading diagnostics
%   of the kind mentioned in the previous point. It can also save
%   recompilations. If a module's source file contains inconsistent
%   definitions for a type constructor, then the programmer will
%   have to delete the unintended ones. Once this is done, the
%   interface file will have to be rebuilt. If we allow inconsistent
%   definitions in the interface file, its new contents will differ
%   from its old contents, which means that all the compilations
%   of *other* modules that read the old contents will have been wasted.
%   If we cause the construction of the interface file to fail instead,
%   those compilations won't have taken place.
%
% The fourth and most motivating use is that having all the definitions
% of a type_ctor, *and* all the foreign_enum pragmas that apply to that
% type_ctor, all together at once will make the code that decides
% the proper representation of that type significantly simpler.
%
% Everything above except the fourth use also applies to the inst_
% and mode_ctor_defn_maps, though for those, the consistency rules are
% much simpler: that each inst and mode constructor must have at most one
% non-abstract definition.
%

:- type type_ctor_defn_map == map(type_ctor, type_ctor_all_defns).
:- type type_ctor_all_defns
    --->    type_ctor_all_defns(
                tcad_abstract_solver        :: list(item_type_defn_info),
                tcad_solver                 :: list(item_type_defn_info),

                tcad_abstract_nonsolver     :: list(item_type_defn_info),
                tcad_eqv                    :: list(item_type_defn_info),
                tcad_du                     :: list(item_type_defn_info),
                tcad_foreign_c              :: list(item_type_defn_info),
                tcad_foreign_java           :: list(item_type_defn_info),
                tcad_foreign_csharp         :: list(item_type_defn_info),
                tcad_foreign_erlang         :: list(item_type_defn_info)
            ).

% The map key is sym_name_and_arity because we don't have an inst_ctor type.
:- type inst_ctor_defn_map == map(sym_name_and_arity, inst_ctor_all_defns).
:- type inst_ctor_all_defns
    --->    inst_ctor_all_defns(
                icad_abstract               :: list(item_inst_defn_info),
                icad_eqv                    :: list(item_inst_defn_info)
            ).

% The map key is sym_name_and_arity because we don't have a mode_ctor type.
:- type mode_ctor_defn_map == map(sym_name_and_arity, mode_ctor_all_defns).
:- type mode_ctor_all_defns
    --->    mode_ctor_all_defns(
                mcad_abstract               :: list(item_mode_defn_info),
                mcad_eqv                    :: list(item_mode_defn_info)
            ).

:- type type_ctor_foreign_enum_map ==
    map(type_ctor, type_ctor_all_foreign_enums).

:- type type_ctor_all_foreign_enums
    --->    type_ctor_all_foreign_enums(
                tcafe_foreign_enums_c       :: list(foreign_enum_spec),
                tcafe_foreign_enums_java    :: list(foreign_enum_spec),
                tcafe_foreign_enums_csharp  :: list(foreign_enum_spec),
                tcafe_foreign_enums_erlang  :: list(foreign_enum_spec)
            ).

:- type type_ctor_repn_map == map(type_ctor, item_type_repn_info).

%-----------------------------------------------------------------------------%

:- func convert_parse_tree_int0_to_parse_tree_int(parse_tree_int0)
    = parse_tree_int.
:- func convert_parse_tree_int1_to_parse_tree_int(parse_tree_int1)
    = parse_tree_int.
:- func convert_parse_tree_int2_to_parse_tree_int(parse_tree_int2)
    = parse_tree_int.
:- func convert_parse_tree_int3_to_parse_tree_int(parse_tree_int3)
    = parse_tree_int.

:- pred convert_parse_tree_int_parse_tree_int0(
    parse_tree_int::in, parse_tree_int0::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred convert_parse_tree_int_parse_tree_int1(
    parse_tree_int::in, parse_tree_int1::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred convert_parse_tree_int_parse_tree_int2(
    parse_tree_int::in, parse_tree_int2::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred convert_parse_tree_int_parse_tree_int3(
    parse_tree_int::in, parse_tree_int3::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- func type_ctor_defn_items_to_map(list(item_type_defn_info))
    = type_ctor_defn_map.
:- func inst_ctor_defn_items_to_map(list(item_inst_defn_info))
    = inst_ctor_defn_map.
:- func mode_ctor_defn_items_to_map(list(item_mode_defn_info))
    = mode_ctor_defn_map.
:- func type_ctor_repn_items_to_map(list(item_type_repn_info))
    = type_ctor_repn_map.
:- func type_ctor_foreign_enum_specs_to_map(list(foreign_enum_spec))
    = type_ctor_foreign_enum_map.

%-----------------------------------------------------------------------------%

:- type parse_tree_opt
    --->    parse_tree_opt(
                pto_module_name             :: module_name,
                pto_opt_file_kind           :: opt_file_kind,

                % The context of the `:- module' declaration.
                pto_module_name_context     :: prog_context,

                % `:- use_module' (not `:- import_module') declarations.
                pto_uses                    :: list(avail_use_info),

                pto_fims                    :: list(item_fim),
                pto_items                   :: list(item)
            ).

%-----------------------------------------------------------------------------%
%
% A raw compilation unit is one module to be compiled. A parse_tree_src
% that contains N nested submodules corresponds to 1 + N raw_compilation_units,
% one for the top level module and one for each (possibly deeply) nested
% submodule.
%
% A raw compilation unit consists of some raw item blocks, with each raw
% item block containing the items in an interface or implementation section
% of its module.
%
% Before we convert a raw compilation unit into the HLDS, we augment it
% with the contents of the interface files of the modules it imports
% (directly or indirectly), and if requested, with the contents of the
% optimization files of those modules as well. The augmented compilation unit
% will consist of
%
% - the src_item_blocks, i.e. the item blocks of the original raw compilation
%   unit, some of which may be marked as implementation but being exported to
%   submodules,
% - the int_item_blocks, which were read from the interfaces of other,
%   directly imported modules,
% - the opt_item_blocks, which were read from the interfaces or optimization
%   files of other, indirectly imported modules.
%
% As with the parse tree types above, the contexts in these types
% may be term.context_init if the actual context isn't known, but if the
% recorded context is not term.context_init, then it is valid.

:- type raw_compilation_unit
    --->    raw_compilation_unit(
                % The name of the module.
                rci_module_name                 :: module_name,

                % The context of the `:- module' declaration.
                rci_module_name_context         :: prog_context,

                % The items in the module.
                rci_raw_item_blocks             :: list(raw_item_block)
            ).

:- type aug_compilation_unit
    --->    aug_compilation_unit(
                % The name of the module.
                aci_module_name                 :: module_name,

                % The context of the `:- module' declaration.
                aci_module_name_context         :: prog_context,

                % The module_version_numbers records in all the imported
                % interface files.
                aci_module_version_numbers_map  :: module_version_numbers_map,

                % The items in the source code of the module.
                aci_src_item_blocks             :: list(src_item_block),

                % The items in the interface files of directly imported
                % modules.
                aci_direct_int_item_blocks      :: list(int_item_block),

                % The items in the interface files of indirectly imported
                % modules.
                aci_indirect_int_item_blocks    :: list(int_item_block),

                % The items in the optimization files of directly or indirectly
                % imported modules.
                aci_opt_item_blocks             :: list(opt_item_block),

                % The items in the interface files needed to make sense
                % of those optimization files.
                aci_int_item_blocks_for_opt     :: list(int_for_opt_item_block)
            ).

:- type raw_item_block == item_block(module_section).

:- type src_item_block == item_block(src_module_section).
:- type int_item_block == item_block(int_module_section).
:- type opt_item_block == item_block(opt_module_section).
:- type int_for_opt_item_block == item_block(int_for_opt_module_section).

:- type item_block(MS)
    --->    item_block(
                module_name,
                MS,
                list(item_include),
                list(item_avail),
                list(item_fim),
                list(item)
            ).

:- type module_names_contexts == multi_map(module_name, prog_context).

:- pred src_to_raw_item_block(src_item_block::in, raw_item_block::out) is det.

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
            % to the submodules. This is done by grab_imported_modules_augment
            % in modules.m.

:- type imported_or_used
    --->    iou_imported
    ;       iou_used
    ;       iou_used_and_imported.

:- type int_module_section
    --->    ims_imported_or_used(module_name, int_file_kind, import_locn,
                imported_or_used)
            % These are used internally by the compiler, to identify
            % declarations which originally came from some other module
            % imported with a `:- import_module' or `:- use_module'
            % declaration. They record the name of the imported module,
            % and in which section the module was imported or used.
            % An iou_used_and_imported means that the module was the subject
            % of a `:- use_module' declaration in the interface and of an
            % `:- import_module' declaration in the implementation; its
            % import_locn will be the one in the implementation.

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

    % An import_locn is used to describe the place where an item was
    % imported from.
:- type import_locn
    --->    import_locn_implementation
            % The item is from a module imported in the implementation.

    ;       import_locn_interface
            % The item is from a module imported in the interface.

    ;       import_locn_import_by_ancestor
            % The item is from a module imported by an ancestor.
            % XXX Did the ancestor do the import in its interface, or not?

    ;       import_locn_ancestor_int0_interface
    ;       import_locn_ancestor_int0_implementation.
            % The item is from the interface or implementation section
            % of the .int0 file of an ancestor module.

%-----------------------------------------------------------------------------%

:- func make_ims_imported(import_locn, module_name, int_file_kind) =
    int_module_section.
:- func make_ims_used(import_locn, module_name, int_file_kind) =
    int_module_section.
:- func make_ims_used_and_imported(import_locn, module_name, int_file_kind) =
    int_module_section.
:- func make_ims_abstract_imported(module_name, int_file_kind) =
    int_module_section.
:- func make_ims_int3_implementation(module_name, int_file_kind) =
    int_module_section.

:- func make_oms_opt_imported(module_name, opt_file_kind) =
    opt_module_section.
:- func make_ioms_opt_imported(module_name, int_file_kind) =
    int_for_opt_module_section.

%-----------------------------------------------------------------------------%

:- func raw_compilation_unit_project_name(raw_compilation_unit) = module_name.
:- func aug_compilation_unit_project_name(aug_compilation_unit) = module_name.

:- pred make_and_add_item_block(module_name::in, MS::in,
    list(item_include)::in, list(item_avail)::in,
    list(item_fim)::in, list(item)::in,
    list(item_block(MS))::in, list(item_block(MS))::out) is det.

:- pred int_imp_items_to_item_blocks(module_name::in, MS::in, MS::in,
    list(item_include)::in, list(item_include)::in,
    list(item_avail)::in, list(item_avail)::in,
    list(item_fim)::in, list(item_fim)::in, list(item)::in, list(item)::in,
    list(item_block(MS))::out) is det.

%-----------------------------------------------------------------------------%

    % get_raw_components(RawItemBlocks, IntIncls, ImpIncls,
    %     IntAvails, ImpAvails, IntItems, ImpItems):
    %
    % Return the includes, avails (i.e. imports and uses), and items
    % from both the interface and implementation blocks in RawItemBlocks.
    %
:- pred get_raw_components(list(raw_item_block)::in,
    list(item_include)::out, list(item_include)::out,
    list(item_avail)::out, list(item_avail)::out,
    list(item_fim)::out, list(item_fim)::out,
    list(item)::out, list(item)::out) is det.

%-----------------------------------------------------------------------------%

    % get_imports_uses_maps(Avails, ImportsMap, UsesMap):
    %
    % Given the avails of a raw compilation unit, return the set of modules
    % imported and used in those sections, mapped to the list of locations
    % of those imports and uses.
    %
:- pred get_imports_uses_maps(list(item_avail)::in,
    module_names_contexts::out, module_names_contexts::out) is det.

    % The accumulator version of get_imports_uses_maps.
    %
:- pred accumulate_imports_uses_maps(list(item_avail)::in,
    module_names_contexts::in, module_names_contexts::out,
    module_names_contexts::in, module_names_contexts::out) is det.

%-----------------------------------------------------------------------------%
%
% The main parts of parse trees are items. There are many kinds of items,
% and most of those kinds have their own item-kind-specific type that stores
% all the information the parse tree has about an item of that kind.
%
% The sequence number fields in the item-kind-specific types are intended to
% allow the recreation of the original item sequence after we have processed
% it into more complex data structures. Negative sequence numbers represent
% items that were not in the original read-in sequence, but which were added
% by the compiler. It is possible for two items to have the same sequence
% number if one original term (e.g. one that imports two or more modules)
% is split apart (e.g. into several items that each import only one module).
%
% When we create interface files, we print out selected items in the module.
% If the sequence of items printed changes, all the other modules depending
% on that interface file will be recompiled.
%
% A nontrivial fraction of changes to a module affect only the *order*
% of the items included in the interface, not their *content*. To minimize
% the amount of recompilation we have to do, we sort (most of the kinds of)
% items in the interface file, so that a change in the item order in the
% source file does not change the order of the items in the interface file.
% To make this sorting effective, we put the fields we prefer to use as
% the sort keys at the start of the item-kind-specific types. These are
% usually those that define the name of the entity, and if it makes sense
% to have more than item with that name, the main fields that distinguish
% items of the same name from each other.
%

    % Did an item originate in user code or was it added by the compiler
    % as part of a source-to-source transformation, e.g. the initialise
    % declarations? If the latter, specify the information that the
    % make_hlds pass may need to answer questions about the item.
    %
:- type item_maybe_attrs
    --->    item_origin_user
    ;       item_origin_compiler(item_compiler_attributes).

:- type item_compiler_attributes
    --->    item_compiler_attributes(
                compiler_origin
            ).

:- type compiler_origin
    --->    compiler_origin_initialise
    ;       compiler_origin_finalise
    ;       compiler_origin_class_method
    ;       compiler_origin_solver_type(
                cost_type_ctor_name             :: sym_name,
                cost_type_ctor_arity            :: arity,
                cost_aux_pred_kind              :: solver_type_pred_kind
            )
    ;       compiler_origin_mutable(
                com_module_name                 :: module_name,
                com_mutable_name                :: string,
                com_aux_pred_kind               :: mutable_pred_kind
            )
    ;       compiler_origin_tabling(
                cot_pred_spec                   :: simple_call_id,
                cot_aux_pred_kind               :: tabling_aux_pred_kind
            ).

:- type item
    --->    item_clause(item_clause_info)
    ;       item_type_defn(item_type_defn_info)
    ;       item_inst_defn(item_inst_defn_info)
    ;       item_mode_defn(item_mode_defn_info)
    ;       item_pred_decl(item_pred_decl_info)
    ;       item_mode_decl(item_mode_decl_info)
    ;       item_foreign_enum(item_foreign_enum_info)
    ;       item_foreign_export_enum(item_foreign_export_enum_info)
    ;       item_pragma(item_pragma_info)
    ;       item_promise(item_promise_info)
    ;       item_typeclass(item_typeclass_info)
    ;       item_instance(item_instance_info)
    ;       item_initialise(item_initialise_info)
    ;       item_finalise(item_finalise_info)
    ;       item_mutable(item_mutable_info)
    ;       item_type_repn(item_type_repn_info).

:- type item_clause_info
    --->    item_clause_info(
                cl_predname                     :: sym_name,
                cl_pred_or_func                 :: pred_or_func,
                cl_head_args                    :: list(prog_term),
                cl_maybe_attrs                  :: item_maybe_attrs,
                cl_varset                       :: prog_varset,
                cl_body                         :: maybe1(goal),
                cl_context                      :: prog_context,
                cl_seq_num                      :: int
            ).

:- type item_type_defn_info
    --->    item_type_defn_info(
                % `:- type ...':
                % a definition of a type, or a declaration of an abstract type.
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: type_defn,
                td_tvarset                      :: tvarset,
                td_context                      :: prog_context,
                td_seq_num                      :: int
            ).

:- type item_inst_defn_info
    --->    item_inst_defn_info(
                % `:- inst ... = ...':
                % a definition of an inst.
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_maybe_for_type               :: maybe(type_ctor),
                id_inst_defn                    :: maybe_abstract_inst_defn,
                id_varset                       :: inst_varset,
                id_context                      :: prog_context,
                id_seq_num                      :: int
            ).

:- type maybe_abstract_inst_defn
    --->    abstract_inst_defn
    ;       nonabstract_inst_defn(inst_defn).

:- type item_mode_defn_info
    --->    item_mode_defn_info(
                % `:- mode ... = ...':
                % a definition of a mode.
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: maybe_abstract_mode_defn,
                md_varset                       :: inst_varset,
                md_context                      :: prog_context,
                md_seq_num                      :: int
            ).

:- type maybe_abstract_mode_defn
    --->    abstract_mode_defn
    ;       nonabstract_mode_defn(mode_defn).

:- type item_pred_decl_info
    --->    item_pred_decl_info(
                % `:- pred ...' or `:- func ...':
                % a predicate or function declaration.
                % This specifies the type of the predicate or function,
                % and it may optionally also specify the mode and determinism.
                pf_name                         :: sym_name,
                pf_p_or_f                       :: pred_or_func,
                pf_arg_decls                    :: list(type_and_mode),
                % The next two fields hold the `with_type` and `with_inst`
                % annotations. This syntactic sugar is expanded out by
                % equiv_type.m, which will then set these fields to `no'.
                pf_maybe_with_type              :: maybe(mer_type),
                pf_maybe_with_inst              :: maybe(mer_inst),
                pf_maybe_detism                 :: maybe(determinism),
                pf_maybe_attrs                  :: item_maybe_attrs,
                pf_tvarset                      :: tvarset,
                pf_instvarset                   :: inst_varset,
                pf_existqvars                   :: existq_tvars,
                pf_purity                       :: purity,
                pf_constraints                  :: prog_constraints,
                pf_context                      :: prog_context,
                pf_seq_num                      :: int
            ).

:- type item_mode_decl_info
    --->    item_mode_decl_info(
                % `:- mode ...':
                % a mode declaration for a predicate or function.
                pfm_name                        :: sym_name,
                pfm_p_or_f                      :: maybe(pred_or_func),
                pfm_arg_modes                   :: list(mer_mode),
                % The next field holds the `with_inst` annotation. This
                % syntactic sugar is expanded by equiv_type.m, which will
                % then set the field to `no'.
                pfm_maybe_with_inst             :: maybe(mer_inst),
                pfm_maybe_detism                :: maybe(determinism),
                pfm_instvarset                  :: inst_varset,
                pfm_context                     :: prog_context,
                pfm_seq_num                     :: int
            ).

:- type item_foreign_enum_info
    --->    item_foreign_enum_info(
                fe_language                     :: foreign_language,
                fe_type_ctor                    :: type_ctor,
                fe_values                       :: one_or_more(
                                                        pair(sym_name, string)),
                fe_context                      :: prog_context,
                fe_seq_num                      :: int
            ).

:- type item_foreign_export_enum_info
    --->    item_foreign_export_enum_info(
                fee_language                    :: foreign_language,
                fee_type_ctor                   :: type_ctor,
                fee_attributes                  :: export_enum_attributes,
                fee_overrides                   :: assoc_list(sym_name, string),
                fee_context                     :: prog_context,
                fee_seq_num                     :: int
            ).

:- type item_pragma_info
    --->    item_pragma_info(
                prag_type                       :: pragma_type,
                prag_maybe_attrs                :: item_maybe_attrs,
                prag_context                    :: prog_context,
                prag_seq_num                    :: int
            ).

:- type item_promise_info
    --->    item_promise_info(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: list(prog_var),
                prom_context                    :: prog_context,
                prom_seq_num                    :: int
            ).

:- type item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                tc_superclasses                 :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: int
            ).

:- type item_instance_info
    --->    item_instance_info(
                % The original types field preserves the types in the instance
                % declaration as written by the programmer. The types field
                % is subject to the expansion of equivalent types.
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_original_types               :: list(mer_type),
                ci_deriving_class               :: list(prog_constraint),
                ci_method_instances             :: instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: int
            ).

:- type item_initialise_info
    --->    item_initialise_info(
                % :- initialise pred_name.
                init_name                       :: sym_name,
                init_arity                      :: arity,
                init_maybe_attrs                :: item_maybe_attrs,
                init_context                    :: prog_context,
                init_seq_num                    :: int
            ).

:- type item_finalise_info
    --->    item_finalise_info(
                % :- finalise pred_name.
                final_name                      :: sym_name,
                final_arity                     :: arity,
                final_maybe_attrs               :: item_maybe_attrs,
                final_context                   :: prog_context,
                final_seq_num                   :: int
            ).

:- type item_mutable_info
    --->    item_mutable_info(
                % :- mutable(var_name, type, inst, value, attrs).
                mut_name                        :: string,
                % The mut_type and mut_inst fields are subject to expansion
                % in equiv_type.m; the mut_orig_type and mut_orig_inst fields
                % are not. The latter are used to improve error reporting.
                mut_orig_type                   :: mer_type,
                mut_type                        :: mer_type,
                mut_orig_inst                   :: mer_inst,
                mut_inst                        :: mer_inst,
                mut_init_value                  :: prog_term,
                mut_init_value_varset           :: prog_varset,
                mut_attrs                       :: mutable_var_attributes,
                mut_context                     :: prog_context,
                mut_seq_num                     :: int
            ).

:- type item_type_repn_info
    --->    item_type_repn_info(
                % `:- type_representation ...':
                % An item added by the compiler to a .int3 file
                % to tell readers of that file the information they need
                % to correctly reconstruct the representation of the given
                % type constructor, even when that information is supposed
                % to be invisible to them semantically.
                % There should be at most one such item for any type_ctor
                % in the .int3 file of its defining module.
                % The sym_name should be fully qualified.
                tr_ctor                         :: sym_name,
                tr_ctor_arg_tvars               :: list(tvar),
                tr_ctor_repn_info               :: type_ctor_repn_info,
                tr_tvarset                      :: tvarset,
                tr_context                      :: prog_context,
                tr_seq_num                      :: int
            ).

:- func get_item_context(item) = prog_context.

:- func item_desc_pieces(item) = list(format_component).

%-----------------------------------------------------------------------------%
%
% Declarations of relationships between modules.
%

:- type item_include
    --->    item_include(
                % The representation of an `:- include_module' declaration
                % is a list of one or more item_includes, each of which
                % declares the named module to be a submodule of the
                % current module,
                %
                % If this item_include occurs in module x.y, then
                % the module_name here is guaranteed to have the form x.y.z.
                % In other words, the included module is guaranteed to be
                % an immediate descendant of the including module.
                % Any attempt to include a non-descendant module or a
                % non-immediate descendant module will be caught and
                % diagnosed by the parser.

                incl_module                     :: module_name,

                % The context and item sequence number of the declaration.
                incl_context                    :: prog_context,
                incl_seq_num                    :: int
            ).

    % Add the name of the included module to the given set.
    %
:- pred add_included_module_name(item_include::in,
    set(module_name)::in, set(module_name)::out) is det.

    % Add the name of the included module to the given map.
    %
:- pred get_included_modules_in_item_include_acc(item_include::in,
    module_names_contexts::in, module_names_contexts::out) is det.

:- type import_or_use
    --->    import_decl
    ;       use_decl.

    % Return "import_module" or "use_module", depending on the argument.
    %
:- func import_or_use_decl_name(import_or_use) = string.

    % The representation of an `:- import_module' or an `:- use_module'
    % declaration is a list of one or more item_avails, each of which
    % makes available to the current module the entities in the interface
    % of the module named in the declaration.
    %
    % With avail_use, references to these entities must be module qualified;
    % with avail_import, they don't have to be.

:- type item_avail
    --->    avail_import(avail_import_info)
    ;       avail_use(avail_use_info).

    % The structures of avail_import_info and avail_use_info are the same,
    % with the first argument being the name of the module that is the subject
    % of the import_module or use_module declaration, and the second and third
    % being the context and item sequence number of the declaration.
    %
    % The two types are separate to allow parse_tree_opts to contain only
    % values of a type that makes it clear that they contain information
    % ONLY about use_module declarations, not import_module declarations.
:- type avail_import_info
    --->    avail_import_info(
                aii_module_name     :: module_name,
                aii_context         :: prog_context,
                aii_seq_num         :: int
            ).
:- type avail_use_info
    --->    avail_use_info(
                aui_module_name     :: module_name,
                aui_context         :: prog_context,
                aui_seq_num         :: int
            ).

:- func item_include_module_name(item_include) = module_name.

:- func get_avail_context(item_avail) = prog_context.
:- func get_import_context(avail_import_info) = prog_context.
:- func get_use_context(avail_use_info) = prog_context.

:- pred avail_is_import(item_avail::in, avail_import_info::out) is semidet.
:- pred avail_is_use(item_avail::in, avail_use_info::out) is semidet.

:- func wrap_avail_import(avail_import_info) = item_avail.
:- func wrap_avail_use(avail_use_info) = item_avail.

:- pred avail_imports_uses(list(item_avail)::in,
    list(avail_import_info)::out, list(avail_use_info)::out) is det.

:- func get_avail_module_name(item_avail) = module_name.
:- func get_import_module_name(avail_import_info) = module_name.
:- func get_use_module_name(avail_use_info) = module_name.

:- type item_fim
    --->    item_fim(
                % A `:- pragma foreign_import_module(Lang, ModuleName)'
                % declaration, which tells the compiler to include the
                % header file we automatically generate for Module
                % in the target language Lang when we compile this module
                % to that language, and, if this occurs in the interface,
                % when we compile the modules importing this one
                % to that same target language.
                %
                % Equivalent to
                % `:- pragma foreign_decl(Lang, "#include <module>.h")',
                % except that the name of the header file is not hard-coded,
                % and mmake can use the dependency information.
                %
                % Throughout most parts of the compiler, we use "FIM"
                % as shorthand for foreign_import_module.

                fim_lang                        :: foreign_language,
                fim_module_name                 :: module_name,
                fim_context                     :: prog_context,
                fim_seq_num                     :: int
            ).

:- type fim_spec
    --->    fim_spec(
                % The specification of a foreign_import_module declaration,
                % without information about where it came from, used where
                % it *does not matter* where it came from.
                fimspec_lang                    :: foreign_language,
                fimspec_module_name             :: module_name
            ).

:- func fim_item_to_spec(item_fim) = fim_spec.
:- func fim_spec_to_item(fim_spec) = item_fim.

%-----------------------------------------------------------------------------%
%
% Type classes.
%

    % The class_decl type represents any declaration that occurs
    % in the body of a type class definition.
    %
    % Such declarations may either declare class methods, or they may declare
    % the modes of class methods.
    %
:- type class_decl
    --->    class_decl_pred_or_func(class_pred_or_func_info)
    ;       class_decl_mode(class_mode_info).

:- type class_pred_or_func_info
    --->    class_pred_or_func_info(
                % This is a `pred ...' or `func ...' declaration in a
                % type class body, which declares a predicate or function
                % method. Such declarations specify the types of the
                % arguments, and may optionally also specify argument modes
                % and the determinism.

                % The name of the predicate or function.
                sym_name,
                pred_or_func,

                % The arguments' types, and maybe modes.
                list(type_and_mode),

                % Any `with_type` and/or `with_inst` annotation.
                maybe(mer_type),
                maybe(mer_inst),

                % The determinism declaration, if any.
                maybe(determinism),

                % The varsets of the type and inst variables.
                tvarset,
                inst_varset,

                % The existentially quantified type variables, if any.
                existq_tvars,

                % Any purity annotation.
                purity,

                % The typeclass constraints on the declaration.
                prog_constraints,

                prog_context
            ).

:- type class_mode_info
    --->    class_mode_info(
                % This is a `mode ...' declaration in a type class body.
                % Such a declaration declares a mode for one of the methods
                % of the type class.

                % The name of the predicate or function.
                sym_name,

                % Whether the method is a predicate or a function.
                % For declarations using `with_inst`, we don't know
                % which it is until we have expanded the inst.
                maybe(pred_or_func),

                % The arguments' modes.
                list(mer_mode),

                % Any `with_inst` annotation.
                maybe(mer_inst),

                % Any determinism declaration.
                maybe(determinism),

                % The varset of the inst variables.
                inst_varset,

                prog_context
            ).

%-----------------------------------------------------------------------------%
%
% Mutable variables.
%

    % Indicates if updates to the mutable are trailed or untrailed.
    %
:- type mutable_trailed
    --->    mutable_untrailed
    ;       mutable_trailed.

    % Indicates if a mutable is attached to the I/O state or not.
    %
:- type mutable_attach_to_io_state
    --->    mutable_dont_attach_to_io_state
    ;       mutable_attach_to_io_state.

    % Indicates if a mutable is constant or not.
    %
:- type mutable_constant
    --->    mutable_not_constant
    ;       mutable_constant.

    % Indicates if a mutable is thread-local or not.
    %
:- type mutable_thread_local
    --->    mutable_not_thread_local
    ;       mutable_thread_local.

    % Has the user specified a name for us to use on the target code side
    % of the FLI?
    %
:- type foreign_name
    --->    foreign_name(
                foreign_name_lang :: foreign_language,
                foreign_name_name :: string
            ).

    % An abstract type for representing a set of mutable variable
    % attributes.
    %
:- type mutable_var_attributes.

    % Return the default attributes for a mutable variable.
    %
:- func default_mutable_attributes = mutable_var_attributes.

    % Access functions for the `mutable_var_attributes' structure.
    %
:- func mutable_var_trailed(mutable_var_attributes) = mutable_trailed.
:- func mutable_var_maybe_foreign_names(mutable_var_attributes)
    = maybe(list(foreign_name)).
:- func mutable_var_attach_to_io_state(mutable_var_attributes)
    = mutable_attach_to_io_state.
:- func mutable_var_constant(mutable_var_attributes) = mutable_constant.
:- func mutable_var_thread_local(mutable_var_attributes)
    = mutable_thread_local.

:- pred set_mutable_var_trailed(mutable_trailed::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_add_foreign_name(foreign_name::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_attach_to_io_state(mutable_attach_to_io_state::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_constant(mutable_constant::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_thread_local(mutable_thread_local::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

%-----------------------------------------------------------------------------%
%
% Information about the representations of types defined in other modules.
%

:- type type_ctor_repn_info
    --->    tcrepn_is_direct_dummy
    ;       tcrepn_is_notag
    ;       tcrepn_is_eqv_to(
                % XXX TYPE_REPN maybe nonword
                % XXX TYPE_REPN maybe notag
                mer_type
            )
    ;       tcrepn_fits_in_n_bits(int, fill_kind)
    ;       tcrepn_is_word_aligned_ptr
    ;       tcrepn_has_direct_arg_functors(list(sym_name_and_arity))
    ;       tcrepn_du(du_repn)
    ;       tcrepn_maybe_foreign(
                % If the foreign language of the current backend
                % is the key of an entry in the list, then
                % the representation of this type_ctor is given
                % by the associated foreign_type_repn.
                %
                % The list may mention a language at most once.
                % If it mentions *every* foreign language once,
                % then the maybe may be "no", since in that case
                % any Mercury definition of the type would never be used.
                %
                % If the foreign language of the current backend
                % is not the key of any entry in the assoc_list, then
                % the maybe must be a "yes" whose argument gives the
                % representation of the Mercury version of the type.
                one_or_more(pair(foreign_language, foreign_type_repn)),
                maybe(du_repn)
            ).

:- type foreign_type_repn
    --->    foreign_type_repn(
                % The name of the foreign type that represents values
                % of this Mercury type.
                frd_foreign_type        :: string,

                % The assertions about this foreign type.
                frd_assertions          :: foreign_type_assertions
            ).

    % There should be exactly one applicable du_repn for any given type_ctor.
    % That means that a non-foreign-enum
    % du_repn for a type_ctor may coexist with one or
    % more foreign enum du_repn for that same type_ctor,
    % *provided* that the foreign enum du_repn are
    % all for different types.
:- type du_repn
    --->    dur_notag(notag_repn)
    ;       dur_direct_dummy(direct_dummy_repn)
    ;       dur_enum(enum_repn)
    ;       dur_gen(gen_du_repn).

:- type notag_repn
    --->    notag_repn(
                % The name of the one functor in the type, which must be
                % arity 1. Its representation will be no_tag.
                % The representation of the argument be *recorded*
                % as a full word at offset 0, but this should never be
                % looked up, since the argument will actually be stored
                % wherever the whole term is stored.
                notag_functor_name      :: string
            ).

:- type direct_dummy_repn
    --->    direct_dummy_repn(
                % The name of the one functor in the type, which must be
                % arity 0. Its representation will be dummy_tag.
                dummy_functor_name      :: string
            ).

:- type enum_repn
    --->    enum_repn(
                % The list of the functor names (all arity 0).
                enum_functors           :: one_or_more(string),

                % The representation of functor #N in Mercury will be
                % int_tag(int_tag_int(N)), with counting starting at 0.
                % However, if the enum_foreign field has an element
                % whose key is the current backend's language, the
                % representation of functor #N will be the Nth string
                % in the associated value.
                %
                % We do not care about the 32 vs 64 bit distinction here,
                % because the definition of an enum type with more than 2^32
                % function symbols will cause a compiler to run out of memory
                % for a *very* long time to come.
                %
                % The set of foreign languages mentioned here must be
                % disjoint from the set of foreign languages mentioned
                % in any tcrepn_conditional wrapped around this du_repn.
                enum_foreign            :: assoc_list(foreign_language,
                                                one_or_more(string))
            ).

:- type gen_du_repn
    --->    gen_du_repn_only_functor(
                % The name of the data constructor. The arity is implicit
                % in the length of the argument list, which must be the same
                % in the 64 and 32 bit versions.
                gduofd_functor          :: string,
                gduofd_args_64          :: gen_du_only_functor_args,
                gduofd_args_32          :: gen_du_only_functor_args
            )
    ;       gen_du_repn_more_functors(
                gdumfd_functors         :: one_or_more(gen_du_functor)
            ).

:- type gen_du_only_functor_args
    --->    gen_du_only_functor_local_args(
                % The cons_tag is local_args_tag(local_args_only_functor).
                % The ptag is 0. The local sectag is 0 bits.
                % For all args, ArgOnlyOffset and CellOffset are both -2.
                gduofl_args             :: one_or_more(local_pos_size)
            )
    ;       gen_du_only_functor_remote_args(
                % The cons_tag is remote_args_tag(remote_args_only_functor).
                % The ptag is 0. The remote sectag size is 0 bits.
                gduofr_args             :: one_or_more(arg_pos_size)
            ).

:- type gen_du_functor
    --->    gen_du_constant_functor(
                % The name of the data constructor. The arity is 0.
                % The ptag is 0. The local sectag is 0. The *size*
                % of the local sectag may depend on the word size.
                gducf_functor           :: string,
                gducf_sectag_size_64    :: sectag_size,
                gducf_sectag_size_32    :: sectag_size
            )
    ;       gen_du_nonconstant_functor(
                % The name of the data constructor. The arity is implicit
                % in the length of the argument list, which must be the same
                % in the 64 and 32 bit versions.
                gduncf_functor          :: string,
                gduncf_tags_64          :: ptag_sectag,
                gduncf_tags_32          :: ptag_sectag,
                gduncf_args_64          :: one_or_more(maybe_direct_arg),
                gduncf_args_32          :: one_or_more(maybe_direct_arg)
            ).

:- type ptag_sectag
    --->    ptag_sectag(
                gdu_ptag                :: uint,
                gdu_maybe_sectag        :: maybe_sectag
            ).

:- type maybe_sectag
    --->    no_sectag
    ;       local_sectag(uint, sectag_size)
    ;       remote_sectag(uint, sectag_size).

:- type sectag_size
    --->    sectag_rest_of_word
    ;       sectag_bits(uint).

:- type maybe_direct_arg
    --->    nondirect_arg(arg_pos_size)
    ;       direct_arg(uint).   % The ptag.

:- type local_pos_size
    --->    local_pos_size(
                lps_shift               :: uint,
                lps_fill                :: fill_kind_size
            ).

:- type arg_pos_size
    --->    pos_full(
                pf_arg_only_offset      :: arg_only_offset,
                pf_cell_offset          :: cell_offset
            )
    ;       pos_double(
                pd_arg_only_offset      :: arg_only_offset,
                pd_cell_offset          :: cell_offset,
                pd_kind                 :: double_word_kind
            )
    ;       pos_partial_first(
                ppf_arg_only_offset     :: arg_only_offset,
                ppf_cell_offset         :: cell_offset,
                ppf_shift               :: uint,
                ppf_fill                :: fill_kind_size
            )
    ;       pos_partial_shifted(
                pps_arg_only_offset     :: arg_only_offset,
                pps_cell_offset         :: cell_offset,
                pps_shift               :: uint,
                pps_fill                :: fill_kind_size
            )
    ;       pos_none_shifted(
                pns_arg_only_offset     :: arg_only_offset,
                pns_cell_offset         :: cell_offset
            )
    ;       pos_none_nowhere.

:- type fill_kind_size
    --->    fk_enum(uint)
    ;       fk_int8
    ;       fk_int16
    ;       fk_int32
    ;       fk_uint8
    ;       fk_uint16
    ;       fk_uint32
    ;       fk_char21.

%-----------------------------------------------------------------------------%
%
% Pragmas.
%

    % XXX We should consider splitting this type into several types, based on
    %
    % - whether a pragma may appear in the interface section of a source file;
    % - whether a pragma may appear in a source file at all,
    %   or whether it is only used to record the results of analyses
    %   in automatically generated .int* and/or .*opt files.
    %
:- type pragma_type
    --->    pragma_foreign_decl(pragma_info_foreign_decl)
    ;       pragma_foreign_code(pragma_info_foreign_code)
    ;       pragma_foreign_proc(pragma_info_foreign_proc)
    ;       pragma_foreign_proc_export(pragma_info_foreign_proc_export)
    ;       pragma_external_proc(pragma_info_external_proc)
    ;       pragma_type_spec(pragma_info_type_spec)
    ;       pragma_inline(pred_name_arity)
    ;       pragma_no_inline(pred_name_arity)
    ;       pragma_consider_used(pred_name_arity)
    ;       pragma_unused_args(pragma_info_unused_args)
    ;       pragma_exceptions(pragma_info_exceptions)
    ;       pragma_trailing_info(pragma_info_trailing_info)
    ;       pragma_mm_tabling_info(pragma_info_mm_tabling_info)
    ;       pragma_obsolete(pred_name_arity, list(sym_name_and_arity))
    ;       pragma_no_detism_warning(pred_name_arity)
    ;       pragma_require_tail_recursion(pragma_info_require_tail_recursion)
    ;       pragma_tabled(pragma_info_tabled)
    ;       pragma_fact_table(pragma_info_fact_table)
    ;       pragma_oisu(pragma_info_oisu)
    ;       pragma_promise_eqv_clauses(pred_name_arity)
    ;       pragma_promise_pure(pred_name_arity)
    ;       pragma_promise_semipure(pred_name_arity)
    ;       pragma_termination_info(pragma_info_termination_info)
    ;       pragma_termination2_info(pragma_info_termination2_info)
    ;       pragma_terminates(pred_name_arity)
    ;       pragma_does_not_terminate(pred_name_arity)
    ;       pragma_check_termination(pred_name_arity)
    ;       pragma_mode_check_clauses(pred_name_arity)
    ;       pragma_structure_sharing(pragma_info_structure_sharing)
    ;       pragma_structure_reuse(pragma_info_structure_reuse)
    ;       pragma_require_feature_set(pragma_info_require_feature_set).

    % Check whether a particular `pragma' declaration is allowed
    % in the interface section of a module.
    %
:- func pragma_allowed_in_interface(pragma_type) = bool.

:- func pragma_desc_pieces(pragma_type) = list(format_component).

    % Foreign language interfacing pragmas.

:- type pragma_info_foreign_decl
    --->    pragma_info_foreign_decl(
                % A foreign language declaration, such as C header code.
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: foreign_literal_or_include
            ).

:- type pragma_info_foreign_code
    --->    pragma_info_foreign_code(
                code_lang               :: foreign_language,
                code_code               :: foreign_literal_or_include
            ).

:- type pragma_info_foreign_proc
    --->    pragma_info_foreign_proc(
                % Set of foreign proc attributes, such as:
                %   what language this code is in
                %   whether or not the code may call Mercury,
                %   whether or not the code is thread-safe
                % PredName, Predicate or Function, Vars/Mode,
                % VarNames, Foreign Code Implementation Info
                proc_attrs              :: pragma_foreign_proc_attributes,
                proc_name               :: sym_name,
                proc_p_or_f             :: pred_or_func,
                proc_vars               :: list(pragma_var),
                proc_varset             :: prog_varset,
                proc_instvarset         :: inst_varset,
                proc_impl               :: pragma_foreign_proc_impl
            ).

:- type pragma_info_foreign_proc_export
    --->    pragma_info_foreign_proc_export(
                % Predname, Predicate/function, Modes, foreign function name.
                exp_language            :: foreign_language,
                exp_pred_id             :: pred_name_modes_pf,
                exp_foreign_name        :: string
            ).

:- type pragma_info_external_proc
    --->    pragma_info_external_proc(
                % The specified procedure(s) is/are implemented outside
                % of Mercury code, for the named backend if there is one,
                % or if there isn't a named backend, then for all backends.
                external_pred_name      :: sym_name,
                external_pred_arity     :: arity,
                external_p_or_f         :: pred_or_func,
                external_maybe_backend  :: maybe(backend)
            ).

    % Optimization pragmas.

:- type pragma_info_type_spec
    --->    pragma_info_type_spec(
                % PredName, SpecializedPredName, Arity, PredOrFunc,
                % Modes if a specific procedure was specified, type
                % substitution (using the variable names from the pred
                % declaration), TVarSet, Equivalence types used
                tspec_pred_name         :: sym_name,
                tspec_new_name          :: sym_name,
                tspec_arity             :: arity,
                tspec_p_or_f            :: maybe(pred_or_func),
                tspec_modes             :: maybe(list(mer_mode)),
                tspec_tsubst            :: type_subst,
                tspec_tvarset           :: tvarset,
                tspec_items             :: set(item_id)
            ).

:- type pragma_info_unused_args
    --->    pragma_info_unused_args(
                % PredName, Arity, Mode number, Removed arguments.
                % Used for intermodule unused argument removal. Should only
                % appear in .opt files.
                unused_proc_id          :: pred_name_arity_pf_mn,
                unused_args             :: list(int)
            ).

:- type pragma_info_exceptions
    --->    pragma_info_exceptions(
                % PredName, Arity, Mode number, Exception status.
                % Should only appear in `.opt' or `.trans_opt' files.
                exceptions_proc_id      :: pred_name_arity_pf_mn,
                exceptions_status       :: exception_status
            ).

:- type pragma_info_trailing_info
    --->    pragma_info_trailing_info(
                % PredName, Arity, Mode number, Trailing status.
                % Should on appear in `.opt' or `.trans_opt' files.
                trailing_info_proc_id   :: pred_name_arity_pf_mn,
                trailing_info_status    :: trailing_status
            ).

:- type pragma_info_mm_tabling_info
    --->    pragma_info_mm_tabling_info(
                % PredName, Arity, Mode number, MM Tabling status.
                % Should on appear in `.opt' or `.trans_opt' files.
                mm_tabling_info_proc_id :: pred_name_arity_pf_mn,
                mm_tabling_info_status  :: mm_tabling_status
            ).

:- type pragma_info_require_tail_recursion
    --->    pragma_info_require_tail_recursion(
                rtr_proc_id             :: pred_name_arity_mpf_mmode,
                rtr_require_tailrec     :: require_tail_recursion

                % This parameter only makes sense when options contains
                % either rtro_mutual_rec_only or rtro_all_recursion.
                % TODO, currently unused, may be used later to implement one
                % of Zoltan's suggestions here:
                % http://www.mercurylang.org/list-archives/developers/
                %   2015-November/016482.html
                % rtr_maybe_scc           :: maybe(list(
                %                             pred_name_arity_mpf_mmode))
            ).

    % Evaluation method pragmas.

:- type pragma_info_tabled
    --->    pragma_info_tabled(
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
                tabled_method           :: eval_method,
                tabled_name             :: pred_name_arity_mpf,
                tabled_mode             :: maybe(list(mer_mode)),
                tabled_attributes       :: maybe(table_attributes)
            ).

:- type pragma_info_fact_table
    --->    pragma_info_fact_table(
                % Predname and Arity, Fact file name.
                fact_table_pred         :: pred_name_arity,
                fact_table_filename     :: string
            ).

:- type pragma_info_oisu
    --->    pragma_info_oisu(
                oisu_type_ctor          :: type_ctor,
                oisu_creator_preds      :: list(pred_name_arity),
                oisu_transformer_preds  :: list(pred_name_arity),
                oisu_destroyer_preds    :: list(pred_name_arity)
            ).

    % Termination analysis pragmas.

:- type pragma_info_termination_info
    --->    pragma_info_termination_info(
                % The list(mer_mode) is the declared argmodes of the
                % procedure, unless there are no declared argmodes, in which
                % case the inferred argmodes are used. This pragma is used to
                % define information about a predicates termination
                % properties. It is most useful where the compiler has
                % insufficient information to be able to analyse the
                % predicate. This includes c_code, and imported predicates.
                % termination_info pragmas are used in opt and trans_opt
                % files.
                terminfo_pred_id        :: pred_name_modes_pf,
                terminfo_args           :: maybe(pragma_arg_size_info),
                terminfo_term           :: maybe(pragma_termination_info)
            ).

:- type pragma_info_termination2_info
    --->    pragma_info_termination2_info(
                terminfo2_pred_id       :: pred_name_modes_pf,
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info)
            ).

    % CTGC pragmas: structure sharing / structure reuse analysis.

:- type pragma_info_structure_sharing
    --->    pragma_info_structure_sharing(
                % After structure sharing analysis, the compiler generates
                % structure sharing pragmas to be stored in and read from
                % optimization interface files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                sharing_pred_id         :: pred_name_modes_pf,
                sharing_headvars        :: list(prog_var),
                sharing_headvartypes    :: list(mer_type),
                sharing_description     :: maybe(structure_sharing_domain)
            ).

:- type pragma_info_structure_reuse
    --->    pragma_info_structure_reuse(
                % After reuse analysis, the compiler generates structure reuse
                % pragmas to be stored in and read from optimization interface
                % files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                % The last sym_name (reuse_optimised_name) stores the name
                % of the optimised version of the exported predicate.
                reuse_pred_id           :: pred_name_modes_pf,
                reuse_headvars          :: list(prog_var),
                reuse_headvartypes      :: list(mer_type),
                reuse_description       :: maybe(structure_reuse_domain)
            ).

    % Misc pragmas.

:- type pragma_info_require_feature_set
    --->    pragma_info_require_feature_set(
                rfs_feature_set         :: set(required_feature)
            ).

:- type foreign_enum_spec
    --->    foreign_enum_spec(
                foreign_language,
                type_ctor,
                one_or_more(pair(sym_name, string))
            ).

    % These types identify procedures in pragmas.

:- type pred_name_arity
    --->    pred_name_arity(
                pna_pred_name           :: sym_name,
                pna_arity               :: arity
            ).

:- type pred_name_arity_pf
    --->    pred_name_arity_pf(
                pnap_pred_name          :: sym_name,
                pnap_arity              :: arity,
                pnap_pf                 :: pred_or_func
            ).

:- type pred_name_arity_pf_mn
    --->    pred_name_arity_pf_mn(
                pnapm_pred_name         :: sym_name,
                pnapm_arity             :: arity,
                pnapm_pf                :: pred_or_func,
                pnapm_mode_num          :: mode_num
            ).

:- type pred_name_arity_mpf_mmode
    --->    pred_name_arity_mpf_mmode(
                pnampm_pred_name        :: sym_name,
                pnampm_arity            :: arity,
                pnampm_maybe_pf         :: maybe(pred_or_func),
                pnampm_maybe_mode       :: maybe(list(mer_mode))
            ).

:- type pred_name_modes_pf
    --->    pred_name_modes_pf(
                pnmp_pred_name          :: sym_name,
                pnmp_arity              :: list(mer_mode),
                pnmp_pf                 :: pred_or_func
            ).

:- type pred_name_arity_mpf
    --->    pred_name_arity_mpf(
                pnam_pred_name          :: sym_name,
                pnam_arity              :: arity,
                pnam_maybe_pf           :: maybe(pred_or_func)
            ).

%-----------------------------------------------------------------------------%
%
% Goals.
%

    % Here is how goals are represented in the parse tree.
    % The three most frequent kinds of goals are first, to give them
    % their own primary tags on 32 bit machines, and
    % the seven most frequent kinds of goals are first, to give them
    % their own primary tags on 64 bit machines.
    %
    % During a bootcheck in august 2015, the frequencies of occurrence
    % of the various goal kinds were these:
    %
    % goal_unify                1360701
    % goal_conj                 1316066
    % goal_call                 1263403
    %
    % goal_true                  135352
    % goal_if_then_else          128052
    % goal_disj                  116547
    % goal_not                     7080
    %
    % goal_fail                    5219
    % goal_pro_purity              1492
    % goal_trace                   1356
    % goal_pro_eqv_solns            913
    % goal_some_state_vars          620 now goal_quant/some/state
    % goal_some                     192 now goal_quant/some/ordinary
    % goal_req_compl_switch         172
    % goal_par_conj                 132
    % goal_implies                  129
    % goal_all                       78 now goal_quant/all/ordinary
    % goal_req_detism                49
    % goal_try                       35
    % goal_equivalent                18
    % goal_event                     17
    % goal_req_arm_detism            14
    % goal_pro_arbitrary             12
    % goal_pro_eqv_soln_sets          8
    % goal_atomic                     2
    % goal_all_state_vars             0 now goal_quant/all/state

:- type quant_type
    --->    quant_some
    ;       quant_all.

:- type quant_vars_kind
    --->    quant_ordinary_vars
    ;       quant_state_vars.

:- type plain_or_dot_var
    --->    podv_plain(prog_var)
            % V: a plain variable.
    ;       podv_dot(prog_var).
            % !.SV: the current state of this state variable.

:- type goal
    % The most frequent kinds of goals.
    --->    unify_expr(prog_context, prog_term, prog_term, purity)
    ;       call_expr(prog_context, sym_name, list(prog_term), purity)

    ;       conj_expr(prog_context, goal, goal)
            % nonempty plain conjunction

    ;       true_expr(prog_context)
            % empty conjunction

    ;       if_then_else_expr(
                prog_context,
                list(prog_var), % SomeVars
                list(prog_var), % StateVars
                goal,           % Cond
                goal,           % Then
                goal            % Else
            )
    ;       disj_expr(prog_context, goal, goal)
            % nonempty disjunction

    ;       not_expr(prog_context, goal)

    % The other kinds of goals.

    ;       fail_expr(prog_context)
            % empty disjunction

    ;       par_conj_expr(prog_context, goal, goal)
            % nonempty parallel conjunction

    ;       quant_expr(
                % Existential or universal quantification?
                quant_type,

                % Are the variables ordinary variables or state variables?
                quant_vars_kind,

                prog_context,
                list(prog_var),
                goal
            )

    ;       promise_purity_expr(prog_context, purity, goal)
    ;       promise_equivalent_solutions_expr(
                prog_context,
                list(prog_var),  % OrdinaryVars
                list(prog_var),  % StateVars (!V)
                list(prog_var),  % DotStateVars (!.V)
                list(prog_var),  % ColonStateVars (!:V)
                goal
            )
    ;       promise_equivalent_solution_sets_expr(
                prog_context,
                list(prog_var),  % OrdinaryVars
                list(prog_var),  % StateVars (!V)
                list(prog_var),  % DotStateVars (!.V)
                list(prog_var),  % ColonStateVars (!:V)
                goal
            )
    ;       promise_equivalent_solution_arbitrary_expr(
                prog_context,
                list(prog_var),  % OrdinaryVars
                list(prog_var),  % StateVars (!V)
                list(prog_var),  % DotStateVars (!.V)
                list(prog_var),  % ColonStateVars (!:V)
                goal
            )
    ;       require_detism_expr(
                prog_context,
                determinism,
                goal
            )
    ;       require_complete_switch_expr(
                prog_context,
                plain_or_dot_var,
                goal
            )
    ;       require_switch_arms_detism_expr(
                prog_context,
                plain_or_dot_var,
                determinism,
                goal
            )
    ;       disable_warnings_expr(
                % Disable the given one or more warnings
                % in the goal inside the scope.
                prog_context,
                goal_warning,
                list(goal_warning),
                goal
            )
    ;       trace_expr(
                texpr_context       :: prog_context,
                texpr_compiletime   :: maybe(trace_expr(trace_compiletime)),
                texpr_runtime       :: maybe(trace_expr(trace_runtime)),
                texpr_maybe_io      :: maybe(prog_var),
                texpr_mutable_vars  :: list(trace_mutable_var),
                texpr_goal          :: goal
            )
    ;       atomic_expr(
                % Subgoals of the atomic goal are parsed into the following
                % datatype. During the creation of the parse tree, all
                % subterms of the "orelse" operator are flattened and placed
                % into a list. If this is the case, the first "orelse"
                % alternative is stored in "main_goal" whilst the other
                % alternatives are stored in "orelse_alternatives". If there
                % are no "or_else" operators within the atomic subgoal,
                % the subgoal is stored in "main_goal" whilst the
                % "orelse_alternatives" list remains empty.

                aexpr_context           :: prog_context,
                aexpr_outer             :: atomic_component_state,
                aexpr_inner             :: atomic_component_state,
                aexpr_output_vars       :: maybe(list(prog_var)),
                aexpr_main_goal         :: goal,
                aexpr_orelse_goals      :: list(goal)
            )
    ;       try_expr(
                tryexpr_context         :: prog_context,
                tryexpr_maybe_io        :: maybe(prog_var),
                tryexpr_goal            :: goal,
                tryexpr_then            :: goal,
                tryexpr_maybe_else      :: maybe(goal),
                tryexpr_catches         :: list(catch_expr),
                tryexpr_maybe_catch_any :: maybe(catch_any_expr)
            )

    ;       implies_expr(prog_context, goal, goal)
            % implies_expr(_, A, B) represents either A => B or B <= A.

    ;       equivalent_expr(prog_context, goal, goal)
            % equivalent_expr(_, A, B) represents A <=> B.

    ;       event_expr(prog_context, string, list(prog_term)).

:- type catch_expr
    --->    catch_expr(
                catch_pattern   :: prog_term,
                catch_goal      :: goal
            ).

:- type catch_any_expr
    --->    catch_any_expr(
                catch_any_var   :: prog_var,
                catch_any_goal  :: goal
            ).

:- func goal_get_context(goal) = prog_context.

%-----------------------------------------------------------------------------%

:- type contains_foreign_code
    --->    contains_foreign_code(set(foreign_language))
    ;       contains_no_foreign_code
    ;       contains_foreign_code_unknown.

:- type contains_foreign_export
    --->    contains_foreign_export
    ;       contains_no_foreign_export.

:- pred get_foreign_code_indicators_from_item_blocks(globals::in,
    list(item_block(MS))::in,
    set(foreign_language)::out, foreign_import_modules::out,
    foreign_include_file_infos::out, contains_foreign_export::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_foreign.

:- import_module require.
:- import_module term.

%-----------------------------------------------------------------------------%

convert_parse_tree_int0_to_parse_tree_int(ParseTreeInt0) = ParseTreeInt :-
    ParseTreeInt0 = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntImportedModuleNames, IntUsedModuleNames,
        ImpImportedModuleNames, ImpUsedModuleNames,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpPragmas, ImpPromises),
    IntIncls = list.map(wrap_include, set.to_sorted_list(IntInclModuleNames)),
    ImpIncls = list.map(wrap_include, set.to_sorted_list(ImpInclModuleNames)),
    IntImportAvails = list.map(wrap_import_avail,
        set.to_sorted_list(IntImportedModuleNames)),
    IntUseAvails = list.map(wrap_use_avail,
        set.to_sorted_list(IntUsedModuleNames)),
    ImpImportAvails = list.map(wrap_import_avail,
        set.to_sorted_list(ImpImportedModuleNames)),
    ImpUseAvails = list.map(wrap_use_avail,
        set.to_sorted_list(ImpUsedModuleNames)),
    IntAvails = IntImportAvails ++ IntUseAvails,
    ImpAvails = ImpImportAvails ++ ImpUseAvails,

    set.to_sorted_list(set.map(fim_spec_to_item, IntFIMSpecs), IntFIMs),
    set.to_sorted_list(set.map(fim_spec_to_item, ImpFIMSpecs), ImpFIMs),
    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        list.map(wrap_pred_decl_item, IntPredDecls) ++
        list.map(wrap_mode_decl_item, IntModeDecls) ++
        type_ctor_foreign_enum_map_to_items(IntForeignEnumMap) ++
        list.map(wrap_pragma_item, IntPragmas) ++
        list.map(wrap_promise_item, IntPromises),
    ImpItems =
        type_ctor_defn_map_to_items(ImpTypeDefnMap) ++
        inst_ctor_defn_map_to_items(ImpInstDefnMap) ++
        mode_ctor_defn_map_to_items(ImpModeDefnMap) ++
        list.map(wrap_typeclass_item, ImpTypeClasses) ++
        list.map(wrap_instance_item, ImpInstances) ++
        list.map(wrap_pred_decl_item, ImpPredDecls) ++
        list.map(wrap_mode_decl_item, ImpModeDecls) ++
        type_ctor_foreign_enum_map_to_items(ImpForeignEnumMap) ++
        list.map(wrap_pragma_item, ImpPragmas) ++
        list.map(wrap_promise_item, ImpPromises),

    ParseTreeInt = parse_tree_int(ModuleName, ifk_int0, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems).

:- pred add_avail_to_rev_list(module_name::in, import_or_use::in,
    list(item_avail)::in, list(item_avail)::out) is det.

add_avail_to_rev_list(ModuleName, ImportOrUse, !RevAvails) :-
    Context = term.context_init,
    SeqNum = -1,
    (
        ImportOrUse = import_decl,
        Avail = avail_import(avail_import_info(ModuleName, Context, SeqNum))
    ;
        ImportOrUse = use_decl,
        Avail = avail_use(avail_use_info(ModuleName, Context, SeqNum))
    ),
    !:RevAvails = [Avail | !.RevAvails].

convert_parse_tree_int1_to_parse_tree_int(ParseTreeInt1) = ParseTreeInt :-
    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntUsedModuleNames, ImpUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses),

    IntIncls = list.map(wrap_include,
        set.to_sorted_list(IntInclModuleNames)),
    ImpIncls = list.map(wrap_include,
        set.to_sorted_list(ImpInclModuleNames)),
    IntAvails = list.map(wrap_use_avail,
        set.to_sorted_list(IntUsedModuleNames)),
    ImpAvails = list.map(wrap_use_avail,
        set.to_sorted_list(ImpUsedModuleNames)),
    set.to_sorted_list(set.map(fim_spec_to_item, IntFIMSpecs), IntFIMs),
    set.to_sorted_list(set.map(fim_spec_to_item, ImpFIMSpecs), ImpFIMs),

    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        list.map(wrap_pred_decl_item, IntPredDecls) ++
        list.map(wrap_mode_decl_item, IntModeDecls) ++
        type_ctor_foreign_enum_map_to_items(IntForeignEnumMap) ++
        list.map(wrap_pragma_item, IntPragmas) ++
        list.map(wrap_promise_item, IntPromises) ++
        type_ctor_repn_map_to_items(IntTypeRepnMap),
    ImpItems =
        type_ctor_defn_map_to_items(ImpTypeDefnMap) ++
        type_ctor_foreign_enum_map_to_items(ImpForeignEnumMap) ++
        list.map(wrap_typeclass_item, ImpTypeClasses),

    ParseTreeInt = parse_tree_int(ModuleName, ifk_int1, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems).

convert_parse_tree_int2_to_parse_tree_int(ParseTreeInt2) = ParseTreeInt :-
    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        MaybeVersionNumbers,
        IntInclModuleNames, IntUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap),

    IntIncls = list.map(wrap_include,
        set.to_sorted_list(IntInclModuleNames)),
    IntAvails = list.map(wrap_use_avail,
        set.to_sorted_list(IntUsedModuleNames)),
    set.to_sorted_list(set.map(fim_spec_to_item, IntFIMSpecs), IntFIMs),
    set.to_sorted_list(set.map(fim_spec_to_item, ImpFIMSpecs), ImpFIMs),

    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        type_ctor_repn_map_to_items(IntTypeRepnMap),
    ImpItems =
        type_ctor_defn_map_to_items(ImpTypeDefnMap),
    ParseTreeInt = parse_tree_int(ModuleName, ifk_int2, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [],
        IntFIMs, ImpFIMs, IntItems, ImpItems).

convert_parse_tree_int3_to_parse_tree_int(ParseTreeInt3) = ParseTreeInt :-
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclModuleNames, IntImportModuleNames,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),

    MaybeVersionNumbers = no_version_numbers,
    IntIncls = list.map(wrap_include,
        set.to_sorted_list(IntInclModuleNames)),
    IntAvails = list.map(wrap_import_avail,
        set.to_sorted_list(IntImportModuleNames)),
    IntItems =
        type_ctor_defn_map_to_items(IntTypeDefnMap) ++
        inst_ctor_defn_map_to_items(IntInstDefnMap) ++
        mode_ctor_defn_map_to_items(IntModeDefnMap) ++
        list.map(wrap_typeclass_item, IntTypeClasses) ++
        list.map(wrap_instance_item, IntInstances) ++
        type_ctor_repn_map_to_items(IntTypeRepnMap),
    ParseTreeInt = parse_tree_int(ModuleName, ifk_int3, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, [], IntAvails, [],
        [], [], IntItems, []).

%-----------------------------------------------------------------------------%

:- func type_ctor_defn_map_to_items(type_ctor_defn_map) = list(item).

type_ctor_defn_map_to_items(TypeCtorDefnMap) = Items :-
    map.foldl_values(accumulate_type_ctor_defns, TypeCtorDefnMap,
        cord.init, TypeDefnsCord),
    TypeDefns = cord.list(TypeDefnsCord),
    Items = list.map(wrap_type_defn_item, TypeDefns).

:- pred accumulate_type_ctor_defns(type_ctor_all_defns::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out) is det.

accumulate_type_ctor_defns(CtorAllDefns, !TypeDefns) :-
    CtorAllDefns = type_ctor_all_defns(AbstractSolverDefns, SolverDefns,
        AbstractNonSolverDefns, EqvDefns, DuDefns,
        ForeignDefnsC, ForeignDefnsJava,
        ForeignDefnsCsharp, ForeignDefnsErlang),
    !:TypeDefns = !.TypeDefns ++
        cord.from_list(AbstractSolverDefns) ++
        cord.from_list(SolverDefns) ++
        cord.from_list(AbstractNonSolverDefns) ++
        cord.from_list(EqvDefns) ++
        cord.from_list(DuDefns) ++
        cord.from_list(ForeignDefnsC) ++
        cord.from_list(ForeignDefnsJava) ++
        cord.from_list(ForeignDefnsCsharp) ++
        cord.from_list(ForeignDefnsErlang).

:- func inst_ctor_defn_map_to_items(inst_ctor_defn_map) = list(item).

inst_ctor_defn_map_to_items(InstCtorDefnMap) = Items :-
    map.foldl_values(accumulate_inst_ctor_defns, InstCtorDefnMap,
        cord.init, InstDefnsCord),
    InstDefns = cord.list(InstDefnsCord),
    Items = list.map(wrap_inst_defn_item, InstDefns).

:- pred accumulate_inst_ctor_defns(inst_ctor_all_defns::in,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out) is det.

accumulate_inst_ctor_defns(CtorAllDefns, !InstDefns) :-
    CtorAllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns),
    !:InstDefns = !.InstDefns ++
        cord.from_list(AbstractDefns) ++
        cord.from_list(EqvDefns).

:- func mode_ctor_defn_map_to_items(mode_ctor_defn_map) = list(item).

mode_ctor_defn_map_to_items(ModeCtorDefnMap) = Items :-
    map.foldl_values(accumulate_mode_ctor_defns, ModeCtorDefnMap,
        cord.init, ModeDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord),
    Items = list.map(wrap_mode_defn_item, ModeDefns).

:- pred accumulate_mode_ctor_defns(mode_ctor_all_defns::in,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out) is det.

accumulate_mode_ctor_defns(CtorAllDefns, !ModeDefns) :-
    CtorAllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns),
    !:ModeDefns = !.ModeDefns ++
        cord.from_list(AbstractDefns) ++
        cord.from_list(EqvDefns).

:- func type_ctor_repn_map_to_items(type_ctor_repn_map) = list(item).

type_ctor_repn_map_to_items(TypeCtorRepnMap) = Items :-
    map.foldl_values(accumulate_type_ctor_repns, TypeCtorRepnMap,
        cord.init, TypeRepnsCord),
    TypeRepns = cord.list(TypeRepnsCord),
    Items = list.map(wrap_type_repn_item, TypeRepns).

:- pred accumulate_type_ctor_repns(item_type_repn_info::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

accumulate_type_ctor_repns(TypeRepn, !TypeRepns) :-
    !:TypeRepns = cord.snoc(!.TypeRepns, TypeRepn).

:- func type_ctor_foreign_enum_map_to_items(type_ctor_foreign_enum_map)
    = list(item).

type_ctor_foreign_enum_map_to_items(ForeignEnumMap) = Items :-
    map.foldl_values(accumulate_foreign_enum_specs, ForeignEnumMap,
        cord.init, ForeignEnumSpecsCord),
    ForeignEnumSpecs = cord.list(ForeignEnumSpecsCord),
    Items = list.map(make_foreign_enum_item, ForeignEnumSpecs).

:- pred accumulate_foreign_enum_specs(type_ctor_all_foreign_enums::in,
    cord(foreign_enum_spec)::in, cord(foreign_enum_spec)::out) is det.

accumulate_foreign_enum_specs(CtorAllDefns, !ForeignEnumSpecs) :-
    CtorAllDefns = type_ctor_all_foreign_enums(ForeignEnumsC,
        ForeignEnumsJava, ForeignEnumsCsharp, ForeignEnumsErlang),
    !:ForeignEnumSpecs = !.ForeignEnumSpecs ++
        cord.from_list(ForeignEnumsC) ++
        cord.from_list(ForeignEnumsJava) ++
        cord.from_list(ForeignEnumsCsharp) ++
        cord.from_list(ForeignEnumsErlang).

%-----------------------------------------------------------------------------%

:- func wrap_include(module_name) = item_include.

wrap_include(ModuleName) = Include :-
    Include = item_include(ModuleName, term.context_init, -1).

:- func wrap_import_avail(module_name) = item_avail.

wrap_import_avail(ModuleName) = Avail :-
    ImportInfo = avail_import_info(ModuleName, term.context_init, -1),
    Avail = avail_import(ImportInfo).

:- func wrap_use_avail(module_name) = item_avail.

wrap_use_avail(ModuleName) = Avail :-
    UseInfo = avail_use_info(ModuleName, term.context_init, -1),
    Avail = avail_use(UseInfo).

:- func wrap_type_defn_item(item_type_defn_info) = item.
:- func wrap_inst_defn_item(item_inst_defn_info) = item.
:- func wrap_mode_defn_item(item_mode_defn_info) = item.
:- func wrap_typeclass_item(item_typeclass_info) = item.
:- func wrap_instance_item(item_instance_info) = item.
:- func wrap_pred_decl_item(item_pred_decl_info) = item.
:- func wrap_mode_decl_item(item_mode_decl_info) = item.
:- func wrap_foreign_enum_item(item_foreign_enum_info) = item.
:- func wrap_pragma_item(item_pragma_info) = item.
:- func wrap_promise_item(item_promise_info) = item.
:- func wrap_type_repn_item(item_type_repn_info) = item.

wrap_type_defn_item(X) = item_type_defn(X).
wrap_inst_defn_item(X) = item_inst_defn(X).
wrap_mode_defn_item(X) = item_mode_defn(X).
wrap_typeclass_item(X) = item_typeclass(X).
wrap_instance_item(X) = item_instance(X).
wrap_pred_decl_item(X) = item_pred_decl(X).
wrap_mode_decl_item(X) = item_mode_decl(X).
wrap_foreign_enum_item(X) = item_foreign_enum(X).
wrap_pragma_item(X) = item_pragma(X).
wrap_promise_item(X) = item_promise(X).
wrap_type_repn_item(X) = item_type_repn(X).

:- func make_foreign_enum_item(foreign_enum_spec) = item.

make_foreign_enum_item(ForeignEnumSpec) = Item :-
    ForeignEnumSpec = foreign_enum_spec(Lang, TypeCtor, Values),
    ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, Values,
        term.context_init, -1),
    Item = item_foreign_enum(ItemForeignEnum).

%-----------------------------------------------------------------------------%

convert_parse_tree_int_parse_tree_int0(ParseTreeInt, ParseTreeInt0, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int0), $pred,
        "trying to convert non-ifk_int0 parse_tree_int to parse_tree_int0"),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    list.foldl(add_included_module_name, ImpIncls,
        set.init, ImpInclModuleNames),

    avail_imports_uses(IntAvails, IntImports, IntUses),
    avail_imports_uses(ImpAvails, ImpImports, ImpUses),
    IntImportedModules = list.map(get_import_module_name, IntImports),
    ImpImportedModules = list.map(get_import_module_name, ImpImports),
    IntUsedModules = list.map(get_use_module_name, IntUses),
    ImpUsedModules = list.map(get_use_module_name, ImpUses),
    set.list_to_set(IntImportedModules, IntImportedModuleNames),
    set.list_to_set(ImpImportedModules, ImpImportedModuleNames),
    set.list_to_set(IntUsedModules, IntUsedModuleNames),
    set.list_to_set(ImpUsedModules, ImpUsedModuleNames),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int0_items_int_or_imp(IntItems, [], IntTypeDefns,
        [], IntInstDefns, [], IntModeDefns,
        [], IntTypeClasses0, [], IntInstances0,
        [], IntPredDecls0, [], RevIntModeDecls,
        [], IntForeignEnums, [], IntPragmas0, [], IntPromises0, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    list.sort(IntPredDecls0, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    IntForeignEnumMap = type_ctor_foreign_enum_specs_to_map(IntForeignEnums),
    list.sort(IntPragmas0, IntPragmas),
    list.sort(IntPromises0, IntPromises),

    classify_int0_items_int_or_imp(ImpItems, [], ImpTypeDefns,
        [], ImpInstDefns, [], ImpModeDefns,
        [], ImpTypeClasses0, [], ImpInstances0,
        [], ImpPredDecls0, [], RevImpModeDecls,
        [], ImpForeignEnums, [], ImpPragmas0, [], ImpPromises0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns),
    ImpInstDefnMap = inst_ctor_defn_items_to_map(ImpInstDefns),
    ImpModeDefnMap = mode_ctor_defn_items_to_map(ImpModeDefns),
    list.sort(ImpTypeClasses0, ImpTypeClasses),
    list.sort(ImpInstances0, ImpInstances),
    list.sort(ImpPredDecls0, ImpPredDecls),
    list.reverse(RevImpModeDecls, ImpModeDecls),
    ImpForeignEnumMap = type_ctor_foreign_enum_specs_to_map(ImpForeignEnums),
    list.sort(ImpPragmas0, ImpPragmas),
    list.sort(ImpPromises0, ImpPromises),

    ParseTreeInt0 = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntImportedModuleNames, IntUsedModuleNames,
        ImpImportedModuleNames, ImpUsedModuleNames,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpPragmas, ImpPromises).

:- pred classify_int0_items_int_or_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(foreign_enum_spec)::in, list(foreign_enum_spec)::out,
    list(item_pragma_info)::in, list(item_pragma_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int0_items_int_or_imp([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !Specs).
classify_int0_items_int_or_imp([Item | Items], !TypeDefns,
        !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_pred_decl(ItemPredDecl),
        !:PredDecls = [ItemPredDecl | !.PredDecls]
    ;
        Item = item_mode_decl(ItemModeDecl),
        !:RevModeDecls = [ItemModeDecl | !.RevModeDecls]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, Values,
            _Context, _SeqNum),
        ForeignEnumSpec = foreign_enum_spec(Lang, TypeCtor, Values),
        !:ForeignEnums = [ForeignEnumSpec | !.ForeignEnums]
    ;
        Item = item_pragma(ItemPragma),
        !:Pragmas = [ItemPragma | !.Pragmas]
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(PromiseType, _, _, _, Context, _),
        (
            ( PromiseType = promise_type_exclusive
            ; PromiseType = promise_type_exhaustive
            ; PromiseType = promise_type_exclusive_exhaustive
            ),
            !:Promises = [ItemPromise | !.Promises]
        ;
            PromiseType = promise_type_true,
            Pieces = [words("A .int0 file may not contain")] ++
                item_desc_pieces(Item) ++ [suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int0 file may not contain")] ++
            item_desc_pieces(Item) ++ [suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int0_items_int_or_imp(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !RevModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !Specs).

%-----------------------------------------------------------------------------%

convert_parse_tree_int_parse_tree_int1(ParseTreeInt, ParseTreeInt1, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int1), $pred,
        "trying to convert non-ifk_int1 parse_tree_int to parse_tree_int1"),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    list.foldl(add_included_module_name, ImpIncls,
        set.init, ImpInclModuleNames),

    avail_imports_uses(IntAvails, IntImports, IntUses),
    avail_imports_uses(ImpAvails, ImpImports, ImpUses),
    IntIntImportContexts = list.map(get_import_context,
        IntImports ++ ImpImports),
    (
        IntIntImportContexts = []
    ;
        IntIntImportContexts = [FirstImportContext | _],
        IntImportPieces = [words("A .int2 file may not contain any"),
            decl("import_module"), words("declarations."), nl],
        IntImportSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstImportContext, [always(IntImportPieces)])]),
        !:Specs = [IntImportSpec | !.Specs]
    ),
    IntUsedModules = list.map(get_use_module_name, IntUses),
    ImpUsedModules = list.map(get_use_module_name, ImpUses),
    set.list_to_set(IntUsedModules, IntUsedModuleNames),
    set.list_to_set(ImpUsedModules, ImpUsedModuleNames),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int1_items_int(IntItems, [], IntTypeDefns,
        [], IntInstDefns, [], IntModeDefns,
        [], IntTypeClasses0, [], IntInstances0,
        [], IntPredDecls0, [], RevIntModeDecls,
        [], IntForeignEnums, [], IntPragmas0, [], IntPromises0,
        [], IntTypeRepns, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    list.sort(IntPredDecls0, IntPredDecls),
    list.reverse(RevIntModeDecls, IntModeDecls),
    IntForeignEnumMap = type_ctor_foreign_enum_specs_to_map(IntForeignEnums),
    list.sort(IntPragmas0, IntPragmas),
    list.sort(IntPromises0, IntPromises),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns),

    classify_int1_items_imp(ImpItems, [], ImpTypeDefns0,
        [], ImpForeignEnums0, [], ImpTypeClasses0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns0),
    ImpForeignEnumMap = type_ctor_foreign_enum_specs_to_map(ImpForeignEnums0),
    list.sort(ImpTypeClasses0, ImpTypeClasses),

    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, IntInclModuleNames, ImpInclModuleNames,
        IntUsedModuleNames, ImpUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses).

:- pred classify_int1_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_pred_decl_info)::in, list(item_pred_decl_info)::out,
    list(item_mode_decl_info)::in, list(item_mode_decl_info)::out,
    list(foreign_enum_spec)::in, list(foreign_enum_spec)::out,
    list(item_pragma_info)::in, list(item_pragma_info)::out,
    list(item_promise_info)::in, list(item_promise_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int1_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !TypeRepns, !Specs).
classify_int1_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !TypeRepns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        Item = item_pred_decl(ItemPredDecl),
        !:PredDecls = [ItemPredDecl | !.PredDecls]
    ;
        Item = item_mode_decl(ItemModeDecl),
        !:ModeDecls = [ItemModeDecl | !.ModeDecls]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, Values,
            _Context, _SeqNum),
        ForeignEnumSpec = foreign_enum_spec(Lang, TypeCtor, Values),
        !:ForeignEnums = [ForeignEnumSpec | !.ForeignEnums]
    ;
        Item = item_pragma(ItemPragma),
        !:Pragmas = [ItemPragma | !.Pragmas]
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(PromiseType, _, _, _, Context, _),
        (
            ( PromiseType = promise_type_exclusive
            ; PromiseType = promise_type_exhaustive
            ; PromiseType = promise_type_exclusive_exhaustive
            ),
            !:Promises = [ItemPromise | !.Promises]
        ;
            PromiseType = promise_type_true,
            Pieces = [words("A .int1 file may not contain")] ++
                item_desc_pieces(Item) ++
                [words("in its interface section."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int1 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int1_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !PredDecls, !ModeDecls,
        !ForeignEnums, !Pragmas, !Promises, !TypeRepns, !Specs).

:- pred classify_int1_items_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(foreign_enum_spec)::in, list(foreign_enum_spec)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int1_items_imp([], !TypeDefns, !ForeignEnums, !TypeClasses, !Specs).
classify_int1_items_imp([Item | Items], !TypeDefns, !ForeignEnums,
        !TypeClasses, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, Values,
            _Context, _SeqNum),
        FESpec = foreign_enum_spec(Lang, TypeCtor, Values),
        !:ForeignEnums = [FESpec | !.ForeignEnums]
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_instance(_)
        ; Item = item_pred_decl(_)
        ; Item = item_clause(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its implementation section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int1_items_imp(Items, !TypeDefns, !ForeignEnums,
        !TypeClasses, !Specs).

%-----------------------------------------------------------------------------%

convert_parse_tree_int_parse_tree_int2(ParseTreeInt, ParseTreeInt2, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int2), $pred,
        "trying to convert non-ifk_int2 parse_tree_int to parse_tree_int2"),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    (
        ImpIncls = []
    ;
        ImpIncls = [FirstImpIncl | _],
        ImpInclPieces = [words("A .int2 file may not contain any"),
            decl("include_module"), words("declarations"),
            words("in its implementation section."), nl],
        ImpInclSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstImpIncl ^ incl_context,
                [always(ImpInclPieces)])]),
        !:Specs = [ImpInclSpec | !.Specs]
    ),

    avail_imports_uses(IntAvails, IntImports, IntUses),
    (
        IntImports = []
    ;
        IntImports = [FirstIntImport | _],
        IntImportPieces = [words("A .int2 file may not contain any"),
            decl("import_module"), words("declarations."), nl],
        IntImportSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstIntImport ^ aii_context,
                [always(IntImportPieces)])]),
        !:Specs = [IntImportSpec | !.Specs]
    ),
    IntUsedModules = list.map(get_use_module_name, IntUses),
    set.list_to_set(IntUsedModules, IntUsedModuleNames),
    (
        ImpAvails = []
    ;
        ImpAvails = [FirstImpAvail | _],
        ImpAvailPieces = [words("A .int2 file may not contain any"),
            decl("import_module"), words("or"), decl("use_module"),
            words("declarations in its implementation section."), nl],
        ImpAvailSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_avail_context(FirstImpAvail),
                [always(ImpAvailPieces)])]),
        !:Specs = [ImpAvailSpec | !.Specs]
    ),

    set.list_to_set(list.map(fim_item_to_spec, IntFIMs), IntFIMSpecs),
    set.list_to_set(list.map(fim_item_to_spec, ImpFIMs), ImpFIMSpecs),

    classify_int2_items_int(IntItems, [], IntTypeDefns0,
        [], IntInstDefns0, [], IntModeDefns0,
        [], IntTypeClasses0, [], IntInstances0, [], IntTypeRepns0, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns0),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns0),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns0),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns0),

    classify_int2_items_imp(ImpItems, [], ImpTypeDefns0, !Specs),
    ImpTypeDefnMap = type_ctor_defn_items_to_map(ImpTypeDefns0),

    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        MaybeVersionNumbers,
        IntInclModuleNames, IntUsedModuleNames, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap).

:- pred classify_int2_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int2_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).
classify_int2_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_clause(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int2 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int2_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).

:- pred classify_int2_items_imp(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int2_items_imp([], !TypeDefns, !Specs).
classify_int2_items_imp([Item | Items], !TypeDefns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_pred_decl(_)
        ; Item = item_clause(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        ),
        Pieces = [words("A .int2 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its implementation section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int2_items_imp(Items, !TypeDefns, !Specs).

%-----------------------------------------------------------------------------%

convert_parse_tree_int_parse_tree_int3(ParseTreeInt, ParseTreeInt3, !Specs) :-
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),

    expect(unify(IntFileKind, ifk_int3), $pred,
        "trying to convert non-ifk_int3 parse_tree_int to parse_tree_int3"),

    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(_),
        VNPieces = [words("A .int3 file may not contain"),
            words("version number information."), nl],
        % MaybeVersionNumbers itself contains no context information.
        VNSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(ModuleNameContext, [always(VNPieces)])]),
        !:Specs = [VNSpec | !.Specs]
    ),

    list.foldl(add_included_module_name, IntIncls,
        set.init, IntInclModuleNames),
    avail_imports_uses(IntAvails, IntImports, IntUses),
    IntImportModules = list.map(get_import_module_name, IntImports),
    set.list_to_set(IntImportModules, IntImportModuleNames),
    (
        IntUses = []
    ;
        IntUses = [FirstIntUse | _],
        IntUsePieces = [words("A .int3 file may not contain any"),
            decl("use_module"), words("declarations."), nl],
        IntUseSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstIntUse ^ aui_context, [always(IntUsePieces)])]),
        !:Specs = [IntUseSpec | !.Specs]
    ),

    (
        IntFIMs = []
    ;
        IntFIMs = [FirstIntFIM | _],
        IntFIMPieces = [words("A .int3 file may not contain any"),
            pragma_decl("foreign_import_module"), words("declarations."), nl],
        IntFIMSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(FirstIntFIM ^ fim_context, [always(IntFIMPieces)])]),
        !:Specs = [IntFIMSpec | !.Specs]
    ),

    classify_int3_items_int(IntItems, [], IntTypeDefns0,
        [], IntInstDefns0, [], IntModeDefns0,
        [], IntTypeClasses0, [], IntInstances0, [], IntTypeRepns0, !Specs),
    IntTypeDefnMap = type_ctor_defn_items_to_map(IntTypeDefns0),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns0),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns0),
    list.sort(IntTypeClasses0, IntTypeClasses),
    list.sort(IntInstances0, IntInstances),
    IntTypeRepnMap = type_ctor_repn_items_to_map(IntTypeRepns0),

    some [!ImpContexts]
    (
        !:ImpContexts = [],
        (
            ImpIncls = []
        ;
            ImpIncls = [HeadIncl | _],
            !:ImpContexts = [HeadIncl ^ incl_context | !.ImpContexts]
        ),
        (
            ImpAvails = []
        ;
            ImpAvails = [HeadAvail | _],
            !:ImpContexts = [get_avail_context(HeadAvail) | !.ImpContexts]
        ),
        (
            ImpFIMs = []
        ;
            ImpFIMs = [HeadFIM | _],
            !:ImpContexts = [HeadFIM ^ fim_context | !.ImpContexts]
        ),
        (
            ImpItems = []
        ;
            ImpItems = [HeadImpItem | _],
            !:ImpContexts = [get_item_context(HeadImpItem) | !.ImpContexts]
        ),
        list.sort(!ImpContexts),
        (
            !.ImpContexts = []
        ;
            !.ImpContexts = [FirstImpContext | _],
            ImpItemPieces = [words("A .int3 file must not have"),
                words("an implementation section."), nl],
            ImpItemSpec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(FirstImpContext, [always(ImpItemPieces)])]),
            !:Specs = [ImpItemSpec | !.Specs]
        )
    ),
    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclModuleNames, IntImportModuleNames,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap).

:- pred classify_int3_items_int(list(item)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_inst_defn_info)::in, list(item_inst_defn_info)::out,
    list(item_mode_defn_info)::in, list(item_mode_defn_info)::out,
    list(item_typeclass_info)::in, list(item_typeclass_info)::out,
    list(item_instance_info)::in, list(item_instance_info)::out,
    list(item_type_repn_info)::in, list(item_type_repn_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_int3_items_int([], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).
classify_int3_items_int([Item | Items], !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        !:TypeDefns = [ItemTypeDefn | !.TypeDefns]
    ;
        Item = item_inst_defn(ItemInstDefn),
        !:InstDefns = [ItemInstDefn | !.InstDefns]
    ;
        Item = item_mode_defn(ItemModeDefn),
        !:ModeDefns = [ItemModeDefn | !.ModeDefns]
    ;
        Item = item_typeclass(ItemTypeClass),
        !:TypeClasses = [ItemTypeClass | !.TypeClasses]
    ;
        Item = item_instance(ItemInstance),
        !:Instances = [ItemInstance | !.Instances]
    ;
        Item = item_type_repn(ItemTypeRepn),
        !:TypeRepns = [ItemTypeRepn | !.TypeRepns]
    ;
        ( Item = item_pred_decl(_)
        ; Item = item_clause(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        Pieces = [words("A .int3 file may not contain")] ++
            item_desc_pieces(Item) ++
            [words("in its interface section."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_item_context(Item), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    classify_int3_items_int(Items, !TypeDefns, !InstDefns, !ModeDefns,
        !TypeClasses, !Instances, !TypeRepns, !Specs).

%-----------------------------------------------------------------------------%

type_ctor_defn_items_to_map(TypeDefnInfos) = TypeDefnMap :-
    list.foldl(add_type_defn_to_map, TypeDefnInfos, map.init, TypeDefnMap).

:- pred add_type_defn_to_map(item_type_defn_info::in,
    type_ctor_defn_map::in, type_ctor_defn_map::out) is det.

add_type_defn_to_map(TypeDefnInfo, !TypeDefnMap) :-
    TypeDefnInfo = item_type_defn_info(SymName, Params, TypeDefn,
        _TypeVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    some [!AbstractSolverDefns, !SolverDefns,
        !AbstractNonSolverDefns, !EqvDefns, !DuDefns,
        !ForeignDefnsC, !ForeignDefnsJava,
        !ForeignDefnsCsharp, !ForeignDefnsErlang]
    (
        ( if map.search(!.TypeDefnMap, TypeCtor, AllDefns0) then
            AllDefns0 = type_ctor_all_defns(
                !:AbstractSolverDefns, !:SolverDefns,
                !:AbstractNonSolverDefns, !:EqvDefns, !:DuDefns,
                !:ForeignDefnsC, !:ForeignDefnsJava,
                !:ForeignDefnsCsharp, !:ForeignDefnsErlang)
        else
            !:AbstractSolverDefns = [],
            !:SolverDefns = [],

            !:AbstractNonSolverDefns = [],
            !:EqvDefns = [],
            !:DuDefns = [],
            !:ForeignDefnsC = [],
            !:ForeignDefnsJava = [],
            !:ForeignDefnsCsharp = [],
            !:ForeignDefnsErlang = []
        ),
        (
            TypeDefn = parse_tree_abstract_type(DetailsAbstract),
            (
                DetailsAbstract = abstract_solver_type,
                !:AbstractSolverDefns = !.AbstractSolverDefns ++ [TypeDefnInfo]
            ;
                ( DetailsAbstract = abstract_type_general
                ; DetailsAbstract = abstract_type_fits_in_n_bits(_)
                ; DetailsAbstract = abstract_dummy_type
                ; DetailsAbstract = abstract_notag_type
                ),
                !:AbstractNonSolverDefns = !.AbstractNonSolverDefns ++
                    [TypeDefnInfo]
            )
        ;
            TypeDefn = parse_tree_solver_type(_DetailsSolver),
            !:SolverDefns = !.SolverDefns ++ [TypeDefnInfo]
        ;
            TypeDefn = parse_tree_eqv_type(_DetailsEqv),
            !:EqvDefns = !.EqvDefns ++ [TypeDefnInfo]
        ;
            TypeDefn = parse_tree_du_type(_DetailsDu),
            !:DuDefns = !.DuDefns ++ [TypeDefnInfo]
        ;
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(LangType, _, _),
            (
                LangType = c(_),
                !:ForeignDefnsC = !.ForeignDefnsC ++ [TypeDefnInfo]
            ;
                LangType = java(_),
                !:ForeignDefnsJava = !.ForeignDefnsJava ++ [TypeDefnInfo]
            ;
                LangType = csharp(_),
                !:ForeignDefnsCsharp = !.ForeignDefnsCsharp ++ [TypeDefnInfo]
            ;
                LangType = erlang(_),
                !:ForeignDefnsErlang = !.ForeignDefnsErlang ++ [TypeDefnInfo]
            )
        ),
        AllDefns = type_ctor_all_defns(!.AbstractSolverDefns, !.SolverDefns,
            !.AbstractNonSolverDefns, !.EqvDefns, !.DuDefns,
            !.ForeignDefnsC, !.ForeignDefnsJava,
            !.ForeignDefnsCsharp, !.ForeignDefnsErlang)
    ),
    map.set(TypeCtor, AllDefns, !TypeDefnMap).

inst_ctor_defn_items_to_map(InstDefnInfos) = InstDefnMap :-
    list.foldl(add_inst_defn_to_map, InstDefnInfos, map.init, InstDefnMap).

:- pred add_inst_defn_to_map(item_inst_defn_info::in,
    inst_ctor_defn_map::in, inst_ctor_defn_map::out) is det.

add_inst_defn_to_map(InstDefnInfo, !InstDefnMap) :-
    InstDefnInfo = item_inst_defn_info(SymName, Params, _MaybeForTypeCtor,
        MaybeAbstractInstDefn, _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    SNA = sym_name_arity(SymName, Arity),
    ( if map.search(!.InstDefnMap, SNA, AllDefns0) then
        AllDefns0 = inst_ctor_all_defns(AbstractDefns0, NonAbstractDefns0),
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AbstractDefns = [InstDefnInfo | AbstractDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns, NonAbstractDefns0)
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(_),
            NonAbstractDefns = [InstDefnInfo | NonAbstractDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns0, NonAbstractDefns)
        ),
        map.det_update(SNA, AllDefns, !InstDefnMap)
    else
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AllDefns = inst_ctor_all_defns([InstDefnInfo], [])
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(_),
            AllDefns = inst_ctor_all_defns([], [InstDefnInfo])
        ),
        map.det_insert(SNA, AllDefns, !InstDefnMap)
    ).

mode_ctor_defn_items_to_map(ModeDefnInfos) = ModeDefnMap :-
    list.foldl(add_mode_defn_to_map, ModeDefnInfos, map.init, ModeDefnMap).

:- pred add_mode_defn_to_map(item_mode_defn_info::in,
    mode_ctor_defn_map::in, mode_ctor_defn_map::out) is det.

add_mode_defn_to_map(ModeDefnInfo, !ModeDefnMap) :-
    ModeDefnInfo = item_mode_defn_info(SymName, Params, MaybeAbstractModeDefn,
        _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    SNA = sym_name_arity(SymName, Arity),
    ( if map.search(!.ModeDefnMap, SNA, AllDefns0) then
        AllDefns0 = mode_ctor_all_defns(AbstractDefns0, NonAbstractDefns0),
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AbstractDefns = [ModeDefnInfo | AbstractDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns, NonAbstractDefns0)
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(_),
            NonAbstractDefns = [ModeDefnInfo | NonAbstractDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns0, NonAbstractDefns)
        ),
        map.det_update(SNA, AllDefns, !ModeDefnMap)
    else
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AllDefns = mode_ctor_all_defns([ModeDefnInfo], [])
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(_),
            AllDefns = mode_ctor_all_defns([], [ModeDefnInfo])
        ),
        map.det_insert(SNA, AllDefns, !ModeDefnMap)
    ).

type_ctor_repn_items_to_map(TypeRepnInfos) = TypeRepnMap :-
    list.foldl(add_type_repn_to_map, TypeRepnInfos, map.init, TypeRepnMap).

:- pred add_type_repn_to_map(item_type_repn_info::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_type_repn_to_map(TypeRepnInfo, !TypeRepnMap) :-
    TypeRepnInfo = item_type_repn_info(SymName, Params, _TypeRepn,
        _TypeVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    % This could be a map.det_insert, because
    %
    % - we ensure the invariant that an interface file contains at most one
    %   type_repn item for a given type_ctor, and
    %
    % - source files should not contain any type_ctor items, because they are
    %   not a publicly documented part of the language.
    %
    % However, until we have code to filter out all type_repn items from
    % source files, they may contain duplicates, so we keep this as map.set
    % for now.
    map.set(TypeCtor, TypeRepnInfo, !TypeRepnMap).

type_ctor_foreign_enum_specs_to_map(ForeignEnumSpecs) = ForeignEnumMap :-
    list.foldl(add_foreign_enum_spec_to_map, ForeignEnumSpecs,
        map.init, ForeignEnumMap).

:- pred add_foreign_enum_spec_to_map(foreign_enum_spec::in,
    type_ctor_foreign_enum_map::in, type_ctor_foreign_enum_map::out) is det.

add_foreign_enum_spec_to_map(ForeignEnumSpec, !ForeignEnumMap) :-
    ForeignEnumSpec = foreign_enum_spec(Lang, TypeCtor, _Values),
    some [!ForeignEnumsC, !ForeignEnumsJava,
        !ForeignEnumsCsharp, !ForeignEnumsErlang]
    (
        ( if map.search(!.ForeignEnumMap, TypeCtor, AllEnums0) then
            AllEnums0 = type_ctor_all_foreign_enums(!:ForeignEnumsC,
                !:ForeignEnumsJava, !:ForeignEnumsCsharp, !:ForeignEnumsErlang)
        else
            !:ForeignEnumsC = [],
            !:ForeignEnumsJava = [],
            !:ForeignEnumsCsharp = [],
            !:ForeignEnumsErlang = []
        ),
        (
            Lang = lang_c,
            !:ForeignEnumsC = !.ForeignEnumsC ++ [ForeignEnumSpec]
        ;
            Lang = lang_java,
            !:ForeignEnumsJava = !.ForeignEnumsJava ++ [ForeignEnumSpec]
        ;
            Lang = lang_csharp,
            !:ForeignEnumsCsharp = !.ForeignEnumsCsharp ++ [ForeignEnumSpec]
        ;
            Lang = lang_erlang,
            !:ForeignEnumsErlang = !.ForeignEnumsErlang ++ [ForeignEnumSpec]
        ),
        AllEnums = type_ctor_all_foreign_enums(!.ForeignEnumsC,
            !.ForeignEnumsJava, !.ForeignEnumsCsharp, !.ForeignEnumsErlang),
        map.set(TypeCtor, AllEnums, !ForeignEnumMap)
    ).

%-----------------------------------------------------------------------------%

src_to_raw_item_block(SrcItemBlock, RawItemBlock) :-
    SrcItemBlock = item_block(ModuleName, SrcSection,
        Incls, Avails, FIMs, Items),
    (
        SrcSection = sms_interface,
        RawSection = ms_interface
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        RawSection = ms_implementation
    ),
    RawItemBlock = item_block(ModuleName, RawSection,
        Incls, Avails, FIMs, Items).

%-----------------------------------------------------------------------------%

make_ims_imported(ImportLocn, ModuleName, IntFileKind) =
    ims_imported_or_used(ModuleName, IntFileKind, ImportLocn, iou_imported).
make_ims_used(ImportLocn, ModuleName, IntFileKind) =
    ims_imported_or_used(ModuleName, IntFileKind, ImportLocn, iou_used).
make_ims_used_and_imported(ImportLocn, ModuleName, IntFileKind) =
    ims_imported_or_used(ModuleName, IntFileKind, ImportLocn,
        iou_used_and_imported).
make_ims_abstract_imported(ModuleName, IntFileKind) =
    ims_abstract_imported(ModuleName, IntFileKind).
make_ims_int3_implementation(_ModuleName, _IntFileKind) = _ :-
    unexpected($pred,
        "An .int3 file should not have an implementation section").

make_oms_opt_imported(ModuleName, OptFileKind) =
    oms_opt_imported(ModuleName, OptFileKind).
make_ioms_opt_imported(ModuleName, OptFileKind) =
    ioms_opt_imported(ModuleName, OptFileKind).

%-----------------------------------------------------------------------------%

raw_compilation_unit_project_name(RawCompUnit) =
    RawCompUnit ^ rci_module_name.

aug_compilation_unit_project_name(AugCompUnit) =
    AugCompUnit ^ aci_module_name.

make_and_add_item_block(ModuleName, Section, Incls, Avails, FIMs, Items,
        !ItemBlocks) :-
    ( if
        Incls = [],
        Avails = [],
        FIMs = [],
        Items = []
    then
        true
    else
        Block = item_block(ModuleName, Section,
            Incls, Avails, FIMs, Items),
        !:ItemBlocks = [Block | !.ItemBlocks]
    ).

int_imp_items_to_item_blocks(ModuleName, IntSection, ImpSection,
        IntIncls, ImpIncls, IntAvails, ImpAvails, IntFIMs, ImpFIMs,
        IntItems, ImpItems, !:ItemBlocks) :-
    make_and_add_item_block(ModuleName, ImpSection,
        ImpIncls, ImpAvails, ImpFIMs, ImpItems, [], !:ItemBlocks),
    make_and_add_item_block(ModuleName, IntSection,
        IntIncls, IntAvails, IntFIMs, IntItems, !ItemBlocks).

%-----------------------------------------------------------------------------%

get_raw_components(RawItemBlocks, !:IntIncls, !:ImpIncls,
        !:IntAvails, !:ImpAvails, !:IntFIMs, !:ImpFIMs,
        !:IntItems, !:ImpItems) :-
    % While lists of items can be very long, this just about never happens
    % with lists of item BLOCKS, so we don't need tail recursion.
    (
        RawItemBlocks = [],
        !:IntIncls = [],
        !:ImpIncls = [],
        !:IntAvails = [],
        !:ImpAvails = [],
        !:IntFIMs = [],
        !:ImpFIMs = [],
        !:IntItems = [],
        !:ImpItems = []
    ;
        RawItemBlocks = [HeadRawItemBlock | TailRawItemBlocks],
        get_raw_components(TailRawItemBlocks, !:IntIncls, !:ImpIncls,
            !:IntAvails, !:ImpAvails, !:IntFIMs, !:ImpFIMs,
            !:IntItems, !:ImpItems),
        HeadRawItemBlock = item_block(_, Section, Incls, Avails, FIMs, Items),
        (
            Section = ms_interface,
            !:IntIncls = Incls ++ !.IntIncls,
            !:IntAvails = Avails ++ !.IntAvails,
            !:IntFIMs = FIMs ++ !.IntFIMs,
            !:IntItems = Items ++ !.IntItems
        ;
            Section = ms_implementation,
            !:ImpIncls = Incls ++ !.ImpIncls,
            !:ImpAvails = Avails ++ !.ImpAvails,
            !:ImpFIMs = FIMs ++ !.ImpFIMs,
            !:ImpItems = Items ++ !.ImpItems
        )
    ).

%-----------------------------------------------------------------------------%

get_imports_uses_maps(Avails, ImportsMap, UsesMap) :-
    accumulate_imports_uses_maps(Avails,
        multi_map.init, ImportsMap, multi_map.init, UsesMap).

accumulate_imports_uses_maps([], !ImportsMap, !UsesMap).
accumulate_imports_uses_maps([Avail | Avails], !ImportsMap, !UsesMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _)),
        multi_map.add(ModuleName, Context, !ImportsMap)
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _)),
        multi_map.add(ModuleName, Context, !UsesMap)
    ),
    accumulate_imports_uses_maps(Avails, !ImportsMap, !UsesMap).

%-----------------------------------------------------------------------------%

get_item_context(Item) = Context :-
    (
        Item = item_clause(ItemClause),
        Context = ItemClause ^ cl_context
    ;
        Item = item_type_defn(ItemTypeDefn),
        Context = ItemTypeDefn ^ td_context
    ;
        Item = item_inst_defn(ItemInstDefn),
        Context = ItemInstDefn ^ id_context
    ;
        Item = item_mode_defn(ItemModeDefn),
        Context = ItemModeDefn ^ md_context
    ;
        Item = item_pred_decl(ItemPredDecl),
        Context = ItemPredDecl ^ pf_context
    ;
        Item = item_mode_decl(ItemModeDecl),
        Context = ItemModeDecl ^ pfm_context
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        Context = ItemForeignEnum ^ fe_context
    ;
        Item = item_foreign_export_enum(ItemForeignExportEnum),
        Context = ItemForeignExportEnum ^ fee_context
    ;
        Item = item_pragma(ItemPragma),
        Context = ItemPragma ^ prag_context
    ;
        Item = item_promise(ItemPromise),
        Context = ItemPromise ^ prom_context
    ;
        Item = item_typeclass(ItemTypeClass),
        Context = ItemTypeClass ^ tc_context
    ;
        Item = item_instance(ItemInstance),
        Context = ItemInstance ^ ci_context
    ;
        Item = item_initialise(ItemInitialise),
        Context = ItemInitialise ^ init_context
    ;
        Item = item_finalise(ItemFinalise),
        Context = ItemFinalise ^ final_context
    ;
        Item = item_mutable(ItemMutable),
        Context = ItemMutable ^ mut_context
    ;
        Item = item_type_repn(ItemTypeRepn),
        Context = ItemTypeRepn ^ tr_context
    ).

item_desc_pieces(Item) = Pieces :-
    (
        Item = item_clause(_),
        Pieces = [words("a clause")]
    ;
        Item = item_type_defn(_),
        Pieces = [words("a type definition")]
    ;
        Item = item_inst_defn(_),
        Pieces = [words("an inst definition")]
    ;
        Item = item_mode_defn(_),
        Pieces = [words("a mode definition")]
    ;
        Item = item_pred_decl(ItemPredDecl),
        PorF = ItemPredDecl ^ pf_p_or_f,
        (
            PorF = pf_predicate,
            Pieces = [words("a predicate declaration")]
        ;
            PorF = pf_function,
            Pieces = [words("a function declaration")]
        )
    ;
        Item = item_mode_decl(_),
        Pieces = [words("a mode declaration")]
    ;
        Item = item_foreign_enum(_),
        Pieces = [pragma_decl("foreign_enum"), words("declaration")]
    ;
        Item = item_foreign_export_enum(_),
        Pieces = [pragma_decl("foreign_export_enum"), words("declaration")]
    ;
        Item = item_pragma(ItemPragma),
        Pieces = pragma_desc_pieces(ItemPragma ^ prag_type)
    ;
        Item = item_promise(ItemPromise),
        PromiseType = ItemPromise ^ prom_type,
        (
            PromiseType = promise_type_exclusive,
            Pieces = [words("an exclusivity promise")]
        ;
            PromiseType = promise_type_exhaustive,
            Pieces = [words("an exhaustivity promise")]
        ;
            PromiseType = promise_type_exclusive_exhaustive,
            Pieces = [words("an exclusivity and exhaustivity promise")]
        ;
            PromiseType = promise_type_true,
            Pieces = [words("an assertion")]
        )
    ;
        Item = item_typeclass(_),
        Pieces = [words("a typeclass declaration")]
    ;
        Item = item_instance(_),
        Pieces = [words("an instance declaration")]
    ;
        Item = item_initialise(_),
        Pieces = [words("an initialise declaration")]
    ;
        Item = item_finalise(_),
        Pieces = [words("a finalise declaration")]
    ;
        Item = item_mutable(_),
        Pieces = [words("the declaration of a mutable")]
    ;
        Item = item_type_repn(_),
        Pieces = [words("a type representation description")]
    ).

%-----------------------------------------------------------------------------%

add_included_module_name(Incl, !ModuleNames) :-
    set.insert(item_include_module_name(Incl), !ModuleNames).

get_included_modules_in_item_include_acc(Incl, !IncludedModuleNames) :-
    Incl = item_include(ModuleName, Context, _SeqNum),
    multi_map.add(ModuleName, Context, !IncludedModuleNames).

import_or_use_decl_name(import_decl) = "import_module".
import_or_use_decl_name(use_decl) = "use_module".

item_include_module_name(Incl) = ModuleName :-
    Incl = item_include(ModuleName, _Context, _SeqNum).

get_avail_context(avail_import(avail_import_info(_, Context, _))) = Context.
get_avail_context(avail_use(avail_use_info(_, Context, _))) = Context.

get_import_context(avail_import_info(_, Context, _)) = Context.

get_use_context(avail_use_info(_, Context, _)) = Context.

avail_is_import(Avail, ImportInfo) :-
    require_complete_switch [Avail]
    (
        Avail = avail_import(ImportInfo)
    ;
        Avail = avail_use(_),
        fail
    ).

avail_is_use(Avail, UseInfo) :-
    require_complete_switch [Avail]
    (
        Avail = avail_import(_),
        fail
    ;
        Avail = avail_use(UseInfo)
    ).

wrap_avail_import(AvailImportInfo) = avail_import(AvailImportInfo).

wrap_avail_use(AvailUseInfo) = avail_use(AvailUseInfo).

avail_imports_uses([], [], []).
avail_imports_uses([Avail | Avails], !:Imports, !:Uses) :-
    avail_imports_uses(Avails, !:Imports, !:Uses),
    (
        Avail = avail_import(AvailImportInfo),
        !:Imports = [AvailImportInfo | !.Imports]
    ;
        Avail = avail_use(AvailUseInfo),
        !:Uses = [AvailUseInfo | !.Uses]
    ).

get_avail_module_name(ItemAvail) = ModuleName :-
    (
        ItemAvail = avail_import(AvailImportInfo),
        AvailImportInfo = avail_import_info(ModuleName, _, _)
    ;
        ItemAvail = avail_use(AvailUseInfo),
        AvailUseInfo = avail_use_info(ModuleName, _, _)
    ).

get_import_module_name(AvailImportInfo) = ModuleName :-
    AvailImportInfo = avail_import_info(ModuleName, _, _).

get_use_module_name(AvailUseInfo) = ModuleName :-
    AvailUseInfo = avail_use_info(ModuleName, _, _).

fim_item_to_spec(FIM) = FIMSpec :-
    FIM = item_fim(Lang, ModuleName, _, _),
    FIMSpec = fim_spec(Lang, ModuleName).

fim_spec_to_item(FIMSpec) = FIM :-
    FIMSpec = fim_spec(Lang, ModuleName),
    FIM = item_fim(Lang, ModuleName, term.context_init, -1).

%-----------------------------------------------------------------------------%
%
% Mutable variables.
%

    % Attributes for mutable variables.
    %
:- type mutable_var_attributes
    --->    mutable_var_attributes(
                mutable_trailed             :: mutable_trailed,
                mutable_foreign_names       :: maybe(list(foreign_name)),
                mutable_attach_to_io_state  :: mutable_attach_to_io_state,
                mutable_constant            :: mutable_constant,
                mutable_thread_local        :: mutable_thread_local
            ).

default_mutable_attributes =
    mutable_var_attributes(mutable_trailed, no,
        mutable_dont_attach_to_io_state, mutable_not_constant,
        mutable_not_thread_local).

mutable_var_trailed(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_trailed.
mutable_var_maybe_foreign_names(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_foreign_names.
mutable_var_attach_to_io_state(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_attach_to_io_state.
mutable_var_constant(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_constant.
mutable_var_thread_local(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_thread_local.

set_mutable_var_trailed(Trailed, !Attributes) :-
    !Attributes ^ mutable_trailed := Trailed.
set_mutable_add_foreign_name(ForeignName, !Attributes) :-
    MaybeForeignNames0 = !.Attributes ^ mutable_foreign_names,
    (
        MaybeForeignNames0 = no,
        MaybeForeignNames  = yes([ForeignName])
    ;
        MaybeForeignNames0 = yes(ForeignNames0),
        ForeignNames = [ForeignName | ForeignNames0],
        MaybeForeignNames   = yes(ForeignNames)
    ),
    !Attributes ^ mutable_foreign_names := MaybeForeignNames.
set_mutable_var_attach_to_io_state(AttachToIOState, !Attributes) :-
    !Attributes ^ mutable_attach_to_io_state := AttachToIOState.
set_mutable_var_constant(Constant, !Attributes) :-
    !Attributes ^ mutable_constant := Constant.
set_mutable_var_thread_local(ThreadLocal, !Attributes) :-
    !Attributes ^ mutable_thread_local := ThreadLocal.

%-----------------------------------------------------------------------------%

pragma_allowed_in_interface(Pragma) = Allowed :-
    % XXX This comment is out of date.
    % pragma `obsolete', `terminates', `does_not_terminate'
    % `termination_info', `check_termination', `foreign_enum' and
    % pragma declarations are supposed to go in the interface,
    % but all other pragma declarations are implementation details only,
    % and should go in the implementation.

    (
        ( Pragma = pragma_foreign_code(_)
        ; Pragma = pragma_foreign_decl(_)
        ; Pragma = pragma_foreign_proc_export(_)
        ; Pragma = pragma_foreign_proc(_)
        ; Pragma = pragma_external_proc(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_consider_used(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_require_tail_recursion(_)
        ; Pragma = pragma_fact_table(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_require_feature_set(_)
        ),
        Allowed = no
    ;
        ( Pragma = pragma_obsolete(_, _)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_check_termination(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_oisu(_)
        ),
        Allowed = yes
    ).

pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = pragma_foreign_code(_),
        Pieces = [pragma_decl("foreign_code"), words("declaration")]
    ;
        Pragma = pragma_foreign_decl(_),
        Pieces = [pragma_decl("foreign_decl"), words("declaration")]
    ;
        Pragma = pragma_foreign_proc_export(_),
        Pieces = [pragma_decl("foreign_export"), words("declaration")]
    ;
        Pragma = pragma_foreign_proc(_),
        Pieces = [pragma_decl("foreign_proc"), words("declaration")]
    ;
        Pragma = pragma_external_proc(External),
        External = pragma_info_external_proc(_, _, PorF, _),
        (
            PorF = pf_predicate,
            Pieces = [pragma_decl("external_pred"), words("declaration")]
        ;
            PorF = pf_function,
            Pieces = [pragma_decl("external_func"), words("declaration")]
        )
    ;
        Pragma = pragma_inline(_),
        Pieces = [pragma_decl("inline"), words("declaration")]
    ;
        Pragma = pragma_no_inline(_),
        Pieces = [pragma_decl("no_inline"), words("declaration")]
    ;
        Pragma = pragma_consider_used(_),
        Pieces = [pragma_decl("consider_used"), words("declaration")]
    ;
        Pragma = pragma_no_detism_warning(_),
        Pieces = [pragma_decl("no_determinism_warning"), words("declaration")]
    ;
        Pragma = pragma_require_tail_recursion(_),
        Pieces = [pragma_decl("require_tail_recursion"), words("declaration")]
    ;
        Pragma = pragma_fact_table(_),
        Pieces = [pragma_decl("fact_table"), words("declaration")]
    ;
        Pragma = pragma_tabled(Tabled),
        Tabled = pragma_info_tabled(EvalMethod, _, _, _),
        (
            EvalMethod = eval_memo,
            Pieces = [pragma_decl("memo"), words("declaration")]
        ;
            EvalMethod = eval_loop_check,
            Pieces = [pragma_decl("loop_check"), words("declaration")]
        ;
            EvalMethod = eval_minimal(_),
            Pieces = [pragma_decl("minimal_model"), words("declaration")]
        ;
            EvalMethod = eval_table_io(_, _),
            unexpected($pred, "eval_table_io")
        ;
            EvalMethod = eval_normal,
            unexpected($pred, "eval_normal")
        )
    ;
        Pragma = pragma_promise_pure(_),
        Pieces = [pragma_decl("promise_pure"), words("declaration")]
    ;
        Pragma = pragma_promise_semipure(_),
        Pieces = [pragma_decl("promise_semipure"), words("declaration")]
    ;
        Pragma = pragma_promise_eqv_clauses(_),
        Pieces = [pragma_decl("promise_equivalent_clauses"),
            words("declaration")]
    ;
        Pragma = pragma_unused_args(_),
        Pieces = [pragma_decl("unused_args"), words("declaration")]
    ;
        Pragma = pragma_exceptions(_),
        Pieces = [pragma_decl("exceptions"), words("declaration")]
    ;
        Pragma = pragma_trailing_info(_),
        Pieces = [pragma_decl("trailing_info"), words("declaration")]
    ;
        Pragma = pragma_mm_tabling_info(_),
        Pieces = [pragma_decl("mm_tabling_info"), words("declaration")]
    ;
        Pragma = pragma_require_feature_set(_),
        Pieces = [pragma_decl("require_feature_set"), words("declaration")]
    ;
        Pragma = pragma_obsolete(_, _),
        Pieces = [pragma_decl("obsolete"), words("declaration")]
    ;
        Pragma = pragma_type_spec(_),
        Pieces = [pragma_decl("type_spec"), words("declaration")]
    ;
        Pragma = pragma_termination_info(_),
        Pieces = [pragma_decl("termination_info"), words("declaration")]
    ;
        Pragma = pragma_termination2_info(_),
        Pieces = [pragma_decl("termination2_info"), words("declaration")]
    ;
        Pragma = pragma_terminates(_),
        Pieces = [pragma_decl("terminates"), words("declaration")]
    ;
        Pragma = pragma_does_not_terminate(_),
        Pieces = [pragma_decl("does_not_terminate"), words("declaration")]
    ;
        Pragma = pragma_check_termination(_),
        Pieces = [pragma_decl("check_termination"), words("declaration")]
    ;
        Pragma = pragma_structure_sharing(_),
        Pieces = [pragma_decl("structure_sharing"), words("declaration")]
    ;
        Pragma = pragma_structure_reuse(_),
        Pieces = [pragma_decl("structure_reuse"), words("declaration")]
    ;
        Pragma = pragma_mode_check_clauses(_),
        Pieces = [pragma_decl("mode_check_clauses"), words("declaration")]
    ;
        Pragma = pragma_oisu(_),
        Pieces = [pragma_decl("oisu"), words("declaration")]
    ).

%-----------------------------------------------------------------------------%

goal_get_context(Goal) = Context :-
    ( Goal = conj_expr(Context, _, _)
    ; Goal = par_conj_expr(Context, _, _)
    ; Goal = true_expr(Context)
    ; Goal = disj_expr(Context, _, _)
    ; Goal = fail_expr(Context)
    ; Goal = quant_expr(_, _, Context, _, _)
    ; Goal = promise_purity_expr(Context, _, _)
    ; Goal = promise_equivalent_solutions_expr(Context, _, _, _, _, _)
    ; Goal = promise_equivalent_solution_sets_expr(Context, _, _, _, _, _)
    ; Goal = promise_equivalent_solution_arbitrary_expr(Context, _, _, _, _, _)
    ; Goal = require_detism_expr(Context, _, _)
    ; Goal = require_complete_switch_expr(Context, _, _)
    ; Goal = require_switch_arms_detism_expr(Context, _, _, _)
    ; Goal = disable_warnings_expr(Context, _, _, _)
    ; Goal = trace_expr(Context, _, _, _, _, _)
    ; Goal = atomic_expr(Context, _, _, _, _, _)
    ; Goal = try_expr(Context, _, _, _, _, _, _)
    ; Goal = implies_expr(Context, _, _)
    ; Goal = equivalent_expr(Context, _, _)
    ; Goal = not_expr(Context, _)
    ; Goal = if_then_else_expr(Context, _, _, _, _, _)
    ; Goal = event_expr(Context, _, _)
    ; Goal = call_expr(Context, _, _, _)
    ; Goal = unify_expr(Context, _, _, _)
    ).

%-----------------------------------------------------------------------------%

:- type module_foreign_info
    --->    module_foreign_info(
                used_foreign_languages      :: set(foreign_language),
                foreign_proc_languages      :: map(sym_name, foreign_language),
                all_foreign_import_modules  :: foreign_import_modules,
                all_foreign_include_files   :: foreign_include_file_infos,
                module_has_foreign_export   :: contains_foreign_export
            ).

get_foreign_code_indicators_from_item_blocks(Globals, ItemBlocks,
        LangSet, ForeignImports, ForeignIncludeFiles, ContainsForeignExport) :-
    Info0 = module_foreign_info(set.init, map.init,
        init_foreign_import_modules, cord.init, contains_no_foreign_export),
    list.foldl(get_foreign_code_indicators_from_item_block(Globals),
        ItemBlocks, Info0, Info),
    Info = module_foreign_info(LangSet0, LangMap, ForeignImports,
        ForeignIncludeFiles, ContainsForeignExport),
    ForeignProcLangs = map.values(LangMap),
    LangSet = set.insert_list(LangSet0, ForeignProcLangs).

:- pred get_foreign_code_indicators_from_item_block(globals::in,
    item_block(MS)::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_foreign_code_indicators_from_item_block(Globals, ItemBlock, !Info) :-
    ItemBlock = item_block(_, _, _, _, FIMs, Items),
    list.foldl(get_foreign_code_indicators_from_fim(Globals), FIMs, !Info),
    list.foldl(get_foreign_code_indicators_from_item(Globals), Items, !Info).

:- pred get_foreign_code_indicators_from_fim(globals::in, item_fim::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_foreign_code_indicators_from_fim(Globals, FIM, !Info) :-
    FIM = item_fim(Lang, ImportedModule, _Context, _SeqNum),
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    ( if list.member(Lang, BackendLangs) then
        ForeignImportModules0 = !.Info ^ all_foreign_import_modules,
        add_foreign_import_module(Lang, ImportedModule,
            ForeignImportModules0, ForeignImportModules),
        !Info ^ all_foreign_import_modules := ForeignImportModules
    else
        true
    ).

:- pred get_foreign_code_indicators_from_item(globals::in, item::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_foreign_code_indicators_from_item(Globals, Item, !Info) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        get_pragma_foreign_code(Globals, Pragma, !Info)
    ;
        Item = item_mutable(_),
        % Mutables introduce foreign_procs, but mutable declarations
        % won't have been expanded by the time we get here, so we need
        % to handle them separately.
        UsedForeignLanguages0 = !.Info ^ used_foreign_languages,
        set.insert_list(all_foreign_languages,
            UsedForeignLanguages0, UsedForeignLanguages),
        !Info ^ used_foreign_languages := UsedForeignLanguages
    ;
        ( Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        % Intialise/finalise declarations introduce export pragmas, but
        % again they won't have been expanded by the time we get here.
        UsedForeignLanguages0 = !.Info ^ used_foreign_languages,
        set.insert_list(all_foreign_languages,
            UsedForeignLanguages0, UsedForeignLanguages),
        !Info ^ used_foreign_languages := UsedForeignLanguages,
        !Info ^ module_has_foreign_export := contains_foreign_export
    ;
        ( Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_type_repn(_)
        )
    ).

:- pred get_pragma_foreign_code(globals::in, pragma_type::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_pragma_foreign_code(Globals, Pragma, !Info) :-
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    globals.get_target(Globals, Target),

    (
        % We do NOT count foreign_decls here. We only link in a foreign object
        % file if mlds_to_gcc called mlds_to_c.m to generate it, which it
        % will only do if there is some foreign_code, not just foreign_decls.
        % Counting foreign_decls here causes problems with intermodule
        % optimization.
        Pragma = pragma_foreign_decl(FDInfo),
        FDInfo = pragma_info_foreign_decl(Lang, _IsLocal, LiteralOrInclude),
        do_get_item_foreign_include_file(Lang, LiteralOrInclude, !Info)
    ;
        Pragma = pragma_foreign_code(FCInfo),
        FCInfo = pragma_info_foreign_code(Lang, LiteralOrInclude),
        ( if list.member(Lang, BackendLangs) then
            Langs0 = !.Info ^ used_foreign_languages,
            set.insert(Lang, Langs0, Langs),
            !Info ^ used_foreign_languages := Langs
        else
            true
        ),
        do_get_item_foreign_include_file(Lang, LiteralOrInclude, !Info)
    ;
        Pragma = pragma_foreign_proc(FPInfo),
        FPInfo = pragma_info_foreign_proc(Attrs, Name, _, _, _, _, _),
        NewLang = get_foreign_language(Attrs),
        FPLangs0 = !.Info ^ foreign_proc_languages,
        ( if map.search(FPLangs0, Name, OldLang) then
            % is it better than an existing one?
            PreferNew = prefer_foreign_language(Globals, Target,
                OldLang, NewLang),
            (
                PreferNew = yes,
                map.det_update(Name, NewLang, FPLangs0, FPLangs),
                !Info ^ foreign_proc_languages := FPLangs
            ;
                PreferNew = no
            )
        else
            % is it one of the languages we support?
            ( if list.member(NewLang, BackendLangs) then
                map.det_insert(Name, NewLang, FPLangs0, FPLangs),
                !Info ^ foreign_proc_languages := FPLangs
            else
                true
            )
        )
    ;
        % XXX `pragma export' should not be treated as foreign, but currently
        % mlds_to_gcc.m doesn't handle that declaration, and instead just
        % punts it on to mlds_to_c.m, thus generating C code for it,
        % rather than assembler code. So we need to treat `pragma export'
        % like the other pragmas for foreign code.
        Pragma = pragma_foreign_proc_export(FPEInfo),
        FPEInfo = pragma_info_foreign_proc_export(Lang, _, _),
        ( if list.member(Lang, BackendLangs) then
            !Info ^ used_foreign_languages :=
                set.insert(!.Info ^ used_foreign_languages, Lang),
            !Info ^ module_has_foreign_export :=
                contains_foreign_export
        else
            true
        )
    ;
        Pragma = pragma_fact_table(_),
        (
            % We generate some C code for fact tables, so we need to treat
            % modules containing fact tables as if they contain foreign code.
            Target = target_c,
            !Info ^ used_foreign_languages :=
                set.insert(!.Info ^ used_foreign_languages, lang_c)
        ;
            ( Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            )
        )
    ;
        ( Pragma = pragma_check_termination(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_external_proc(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_consider_used(_)
        ; Pragma = pragma_obsolete(_, _)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_require_feature_set(_)
        ; Pragma = pragma_require_tail_recursion(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_oisu(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_unused_args(_)
        )
        % Do nothing.
    ).

:- pred do_get_item_foreign_include_file(foreign_language::in,
    foreign_literal_or_include::in, module_foreign_info::in,
    module_foreign_info::out) is det.

do_get_item_foreign_include_file(Lang, LiteralOrInclude, !Info) :-
    (
        LiteralOrInclude = floi_literal(_)
    ;
        LiteralOrInclude = floi_include_file(FileName),
        IncludeFile = foreign_include_file_info(Lang, FileName),
        IncludeFilesCord0 = !.Info ^ all_foreign_include_files,
        IncludeFilesCord = cord.snoc(IncludeFilesCord0, IncludeFile),
        !Info ^ all_foreign_include_files := IncludeFilesCord
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_item.
%-----------------------------------------------------------------------------%
