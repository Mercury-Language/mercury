%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_parse_tree.m.
% Main author: zs.
%
% The Mercury implementation uses several different kinds of files.
% Besides source files, it uses four kinds of interface files and
% two kinds of optimization files. The parse trees of these files
% contain a structured representation of the information in these files.
% This module defines the top levels of these parse trees, which are
% the parts that differ between the different kinds of files. The lower
% levels of the representation, which are used by most or all kinds
% of Mercury files, are defined in prog_item.m, and in prog_data*.m.
%
% This module and prog_item.m together define the parts of parse trees
% that *are not* needed after the construction of the initial HLDS, while
% the parts that *are* needed after that point in time are contained in
% prog_data*.m.
%
%---------------------------------------------------------------------------%
%
% One important consideration in the design of the parse trees is that
% they have two different use cases:
%
% - to represent files being read in, and
% - to represent files being written out.
%
% The two have slightly different requirements, because
%
% - we will never knowingly write out erroneous Mercury code, but
% - we know that we *will* read in some.
%
% In the past, we used to include some information (such as which modules
% are imported and/or used in which section of a module) in two different
% forms, one of which allowed the presence of errors, and one which did not.
% We have now (mostly) transitioned to a scheme where we check for and report
% (in the sense of generating warnings or errors for) any problems in the
% input, and include in the parse tree only the cleaned-up form in which any
% contradictions in the input have been resolved. This allows us to continue
% to process the input, and find and report as many more problems as we can.
% If the problems we found while constructing the parse tree included errors
% and not just warnings, we will of course have to stop at a the point where
% the risk of any errors we find and report has too high a chance of being
% an avalanche error, caused not by the code we are looking at, but by
% an incorrect resolution by the compiler of an earlier problem.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_parse_tree.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module recompilation.
:- import_module recompilation.item_types.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more_map.
:- import_module set.

%---------------------------------------------------------------------------%

:- type module_names_contexts == one_or_more_map(module_name, prog_context).

%---------------------------------------------------------------------------%

:- type include_module_map == map(module_name, include_module_info).
:- type int_include_module_map == map(module_name, int_include_module_info).
:- type include_module_info
    --->    include_module_info(module_section, prog_context).
            % The "include_module" declaration occurs in the given section
            % of the relevant file, and at the given context.

:- type int_include_module_info =< include_module_info
    --->    include_module_info(int_module_section, prog_context).

:- type int_module_section =< module_section
    --->    ms_interface.

%---------------------%

% The module being compiled can have another module made available to it
% either explicitly or implicitly.
%
% An explicit availability can happen either through an `:- import_module'
% or a `:- use_module' declaration (import or use, for short), and these
% declarations can occur in either the interface section or in the
% implementation section.
%
% The values of section_import_and_or_use specify the possible valid ways
% that a module may be made available explicitly. The first four specify
% the usual ways: an import or use in either section, with the one context.
% The last one says that the module was named in an use_module declaration
% in the interface section and in an import_module declaration
% in the implementation section, and give the two contexts respectively.
%
% The values of the implicit_import_or_use type specify the possible ways
% that a module may be made available implicitly. Most implicit availability
% is to give the compiler access to the declarations of predicates and
% functions that the compiler will automatically insert calls to as part of
% the implementation of some language feature, such as table resets for
% memoed procedures. Since these automatically generated references
% will be created fully module qualified, an import of the target modules
% is not needed; a use is enough. However, the public builtin module
% is implicitly imported into every Mercury module, and this one
% does get imported, not used.
%
% Note that we do not record contexts for implicit uses. In general,
% there is no *single specific* context that makes an implicit use needed,
% and we don't need contexts for any error messages about implicit imports,
% since we don't want to require Mercury programmers to have to know
% such details of the Mercury implementation. We *could* collect the set of
% contexts that make a given implicit use needed, for internal compiler
% purposes, but we do not (yet) have any need for that information.

:- type section_import_and_or_use
    --->    int_import(prog_context)
    ;       int_use(prog_context)
    ;       imp_import(prog_context)
    ;       imp_use(prog_context)
    ;       int_use_imp_import(prog_context, prog_context).

:- type section_use =< section_import_and_or_use
    --->    int_use(prog_context)
    ;       imp_use(prog_context).

:- type int_section_import =< section_import_and_or_use
    --->    int_import(prog_context).

:- type implicit_import_or_use
    --->    implicit_int_import
    ;       implicit_int_use
    ;       implicit_imp_use.

:- type maybe_implicit_import_and_or_use
    --->    explicit_avail(
                section_import_and_or_use
            )
    ;       implicit_avail(
                implicit_import_or_use,
                maybe(section_import_and_or_use)
            ).

    % Values of this type specify how each module we have available
    % was *made* available. If a module had redundant import_module
    % and/or use_module declarations, each of these has had a warning
    % generated for it and was then discarded. One of these declarations
    % can be made redundant redundant not only by another declaration
    % of the same kind in the same section, but also by more permissive
    % declarations; import_module declarations grant more permissions
    % than use_module declarations, and declarations in the interface
    % give more permissions than the declarations of the same kind
    % in the implementation section.
:- type section_import_and_or_use_map ==
    map(module_name, section_import_and_or_use).
:- type section_use_map ==
    map(module_name, section_use).
:- type int_import_map ==
    map(module_name, int_section_import).
:- type import_and_or_use_map ==
    map(module_name, maybe_implicit_import_and_or_use).

%---------------------------------------------------------------------------%
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
% The contexts of module declarations below may be term_context.dummy_context
% if the actual context isn't known, but if the recorded context is
% not term_context.dummy_context, then it is valid.
%

    % Values of this type represent the contents of a .m source file.
    %
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

    % Values of this type represent one module in a .m source file.
    % (A source file may contain more than one module, though most Mercury
    % source files contain just one.)
    %
:- type parse_tree_module_src
    --->    parse_tree_module_src(
                ptms_module_name            :: module_name,

                % The context of the `:- module' declaration.
                ptms_module_name_context    :: prog_context,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface and in the implementation,
                % in a cleaned-up form. The cleanup requires that
                %
                % - no module be included more than once in a section, and
                % - no module be included in both sections.
                %
                % Any violations of these requirements will have had
                % an error message generated for it before the
                % parse_tree_module_src structure is constructed.
                ptms_include_map            :: include_module_map,

                % A specification of the set of modules mentioned in
                % `:- import_module' and/or `:- use_module' declarations
                % in each section, possibly augmented with implicit
                % imports/uses, in a cleaned-up form. The cleanup requires
                % that
                %
                % - no module be imported more than once in a section,
                % - no module be used more than once in a section,
                % - no module be both imported and used used in a section, and
                % - no module be imported or used in more than once section,
                %   with the one permitted exception is when a module is
                %   used in the interface and imported in the implementation.
                %
                % Any violations of these requirements will have had
                % an error message generated for it before the
                % parse_tree_module_src structure is constructed.
                ptms_import_use_map         :: import_and_or_use_map,

                % A cleaned-up version of the set of explicit
                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                % The cleaned-up part means that we we have reported both
                %
                % - FIMs that occur more than once in a given section, and
                % - FIMs that occur in both sections.
                %
                % We keep the context of only the first FIM for a given
                % fim_spec in each section, and if a fim_spec occurs in
                % both sections, we keep only the (first) occurrence in the
                % interface section.
                %
                % We don't have a field containing the original, non-cleaned-up
                % data, since no part of the compiler (yet) need this.
                ptms_int_fims               :: map(fim_spec, prog_context),
                ptms_imp_fims               :: map(fim_spec, prog_context),

                % The set of foreign languages for which this module
                % should have implicit foreign_import_module declaration
                % for itself, in the interface and implementation respectively.
                ptms_int_self_fim_langs     :: set(foreign_language),
                ptms_imp_self_fim_langs     :: set(foreign_language),

                ptms_type_defns             :: type_ctor_checked_map,
                ptms_inst_defns             :: inst_ctor_checked_map,
                ptms_mode_defns             :: mode_ctor_checked_map,
                % The error messages generated during the construction
                % of ptms_type_defns. We have found some invalid types if
                % some of these error_specs (a) are severity_error, and
                % (b) are phase_tim_check_invalid_type.
                ptms_type_specs             :: list(error_spec),
                % The error messages generated during the construction
                % of ptms_inst_defns and ptms_mode_defns. We have found
                % some invalid insts and/or modes if some of these error_specs
                % (a) are severity_error, and (b) are
                % phase_tim_check_invalid_inst_mode.
                ptms_inst_mode_specs        :: list(error_spec),

                % Items of various kinds in the interface.
                % All these items are to be treated as being in the
                % interface section, with one exception.
                % If this module has some submodules, i.e. if the
                % ptms_include_map field above is nonempty, then we handle
                % any nonabstract instance items in the interface by
                % - treating only an abstract version of the item as being
                %   in the interface, and
                % - treating the original version as being in the
                %   implementation section, but exported to submodules.
                % (For abstract instances, there is no point in adding them
                % twice, once in each section, so we treat them as only
                % being in the interface.)
                ptms_int_typeclasses        :: list(item_typeclass_info),
                ptms_int_instances          :: list(item_instance_info),
                ptms_int_pred_decls         :: list(item_pred_decl_info),
                ptms_int_mode_decls         :: list(item_mode_decl_info),
                ptms_int_decl_pragmas       :: list(item_decl_pragma_info),
                ptms_int_decl_markers       :: list(item_decl_marker_info),
                ptms_int_promises           :: list(item_promise_info),

                % The set of predicate names for which the interface contains
                % either attempts at a definition (i.e. a clause or a
                % foreign_proc), or something else that tells us that
                % generating a warning about a lack of a definition
                % in the implementation section (if in fact there is
                % no definition there) would be more misleading than useful.
                ptms_int_bad_clauses        :: set(pred_pf_name_arity),

                % A repeat of everything above, but in the implementation
                % section, with the addition of some item kinds that may occur
                % *only* in implementation sections.
                %
                % However, note that the conversion process we now use
                % to generate parse_tree_module_srcs will put any impl pragmas,
                % initialises, finalises and mutables that were wrongly placed
                % in the interface section into their fields below, so that
                % if there is something wrong with them *beyond* their
                % location, the compiler can detect and report it in the
                % same compiler invocation. It would be easy to put these
                % misplaced items into separate fields of their own,
                % but so far there has been no need for that.
                %
                % If this module has no submodules, i.e. if the
                % ptms_include_map field above is empty, then all the items
                % in these fields are to be treated as in being in the
                % implementation section. However, if this module HAS
                % at least one submodule (in either section), then only
                % the following kinds of items are to be treated as being
                % private to this module:
                %
                %   clauses
                %   foreign_procs
                %   foreign_export_enums
                %   impl_pragmas
                %   initialises
                %   finalises
                %
                % All the other kinds of items are to be treated as being
                % exported to submodules.
                ptms_imp_typeclasses        :: list(item_typeclass_info),
                ptms_imp_instances          :: list(item_instance_info),
                ptms_imp_pred_decls         :: list(item_pred_decl_info),
                ptms_imp_mode_decls         :: list(item_mode_decl_info),
                ptms_imp_clauses            :: list(item_clause_info),
                ptms_imp_foreign_procs      :: list(item_foreign_proc_info),
                ptms_imp_foreign_export_enums ::
                                        list(item_foreign_export_enum_info),
                ptms_imp_decl_pragmas       :: list(item_decl_pragma_info),
                ptms_imp_decl_markers       :: list(item_decl_marker_info),
                ptms_imp_impl_pragmas       :: list(item_impl_pragma_info),
                ptms_imp_impl_markers       :: list(item_impl_marker_info),
                ptms_imp_promises           :: list(item_promise_info),
                ptms_imp_initialises        :: list(item_initialise_info),
                ptms_imp_finalises          :: list(item_finalise_info),
                ptms_imp_mutables           :: list(item_mutable_info)
            ).

%---------------------------------------------------------------------------%
%
% The representations specific to .int0, .int, .int2 and .int3 files.
%

    % A representation of the contents of .int0 files.
    %
:- type parse_tree_int0
    --->    parse_tree_int0(
                pti0_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti0_module_name_context    :: prog_context,

                pti0_maybe_version_numbers  :: maybe_version_numbers,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface and implementation,
                % and their locations.
                pti0_include_map            :: include_module_map,

                % The set of modules mentioned in `:- import_module'
                % declarations in the interface and implementation,
                % and their locations.
                pti0_import_use_map         :: section_import_and_or_use_map,

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti0_int_fims               :: set(fim_spec),
                pti0_imp_fims               :: set(fim_spec),

                % Type, inst and mode definitions from both
                % the interface and implementation sections.
                pti0_type_defns             :: type_ctor_checked_map,
                pti0_inst_defns             :: inst_ctor_checked_map,
                pti1_mode_defns             :: mode_ctor_checked_map,

                % Items of various kinds in the interface.
                % XXX For the consumers of the .int0 file, in most cases
                % it makes no difference whether an item was in the parent's
                % interface or implementation section. We should make that
                % distinction here ONLY when we have to.
                pti0_int_typeclasses        :: list(item_typeclass_info),
                pti0_int_instances        :: list(item_abstract_instance_info),
                pti0_int_pred_decls         :: list(item_pred_decl_info),
                pti0_int_mode_decls         :: list(item_mode_decl_info),
                pti0_int_decl_pragmas       :: list(item_decl_pragma_info),
                pti0_int_decl_markers       :: list(item_decl_marker_info),
                pti0_int_promises           :: list(item_promise_info),

                % Items of various kinds in the implementation section.
                pti0_imp_typeclasses        :: list(item_typeclass_info),
                pti0_imp_instances        :: list(item_abstract_instance_info),
                pti0_imp_pred_decls         :: list(item_pred_decl_info),
                pti0_imp_mode_decls         :: list(item_mode_decl_info),
                pti0_imp_decl_pragmas       :: list(item_decl_pragma_info),
                pti0_imp_decl_markers       :: list(item_decl_marker_info),
                pti0_imp_promises           :: list(item_promise_info)
            ).

    % A representation of the contents of .int files.
    %
:- type parse_tree_int1
    --->    parse_tree_int1(
                pti1_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti1_module_name_context    :: prog_context,

                pti1_maybe_version_numbers  :: maybe_version_numbers,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface and implementation,
                % and their contexts.
                pti1_include_map            :: include_module_map,

                % The set of modules mentioned in `:- use_module'
                % declarations in the interface and implementation,
                % and their locations.
                pti1_use_map                :: section_use_map,

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti1_int_fims               :: set(fim_spec),
                pti1_imp_fims               :: set(fim_spec),

                % Type, inst and mode definitions, all of which are
                % in the interface, with the exception of some type
                % definitions from the implementation section
                % (which should not be needed after we start actually
                % *using* type_repn items).
                pti1_type_defns             :: type_ctor_checked_map,
                pti1_inst_defns             :: inst_ctor_checked_map,
                pti1_mode_defns             :: mode_ctor_checked_map,

                % Items of various kinds in the interface.
                pti1_int_typeclasses        :: list(item_typeclass_info),
                pti1_int_instances      :: list(item_abstract_instance_info),
                pti1_int_pred_decls         :: list(item_pred_decl_info),
                pti1_int_mode_decls         :: list(item_mode_decl_info),
                pti1_int_decl_pragmas       :: list(item_decl_pragma_info),
                pti1_int_decl_markers       :: list(item_decl_marker_info),
                pti1_int_promises           :: list(item_promise_info),

                % The representations of all types defined in the module,
                % whether exported or not.
                pti1_type_repns             :: type_ctor_repn_map,

                % Items of various kinds in the implementation.
                pti1_imp_typeclasses    :: list(item_abstract_typeclass_info)
            ).

    % A representation of the contents of .int2 files.
    %
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
                % declarations in the interface, and their locations.
                pti2_int_includes           :: int_include_module_map,

                % The set of modules mentioned in `:- use_module'
                % declarations in the interface, and their locations.
                pti2_use_map                :: section_use_map,

                % `:- pragma foreign_import_module' declarations
                % in the interface and in the implementation.
                pti2_int_fims               :: set(fim_spec),
                pti2_imp_fims               :: set(fim_spec),

                % Type, inst and mode definitions, all of which are
                % in the interface, with the exception of some type
                % definitions from the implementation section
                % (which should not be needed after we start actually
                % *using* type_repn items).
                pti2_type_defns             :: type_ctor_checked_map,
                pti2_inst_defns             :: inst_ctor_checked_map,
                pti2_mode_defns             :: mode_ctor_checked_map,

                % Items of various kinds in the interface.
                pti2_int_typeclasses        :: list(item_typeclass_info),
                pti2_int_instances      :: list(item_abstract_instance_info),

                % The representations of all types defined in the module,
                % whether exported or not.
                pti2_type_repns             :: type_ctor_repn_map
            ).

    % A representation of the contents of .int3 files.
    %
:- type parse_tree_int3
    --->    parse_tree_int3(
                pti3_module_name            :: module_name,

                % The context of the `:- module' declaration.
                pti3_module_name_context    :: prog_context,

                % The set of modules mentioned in `:- include_module'
                % declarations in the interface, and their locations.
                pti3_int_includes           :: int_include_module_map,

                % The set of modules mentioned in `:- import_module'
                % declarations in the interface, and their locations.
                pti3_int_import_map         :: int_import_map,

                % Type, inst and mode definitions, all of which are
                % in the interface.
                pti3_type_defns             :: type_ctor_checked_map,
                pti3_inst_defns             :: inst_ctor_checked_map,
                pti3_mode_defns             :: mode_ctor_checked_map,

                % Items of various kinds in the interface.
                % XXX The typeclass definitions in .int3 files have invariants
                % that we cannot yet encode in a subtype. Specifically,
                % these typeclass definitions are not only guaranteed to have
                % class_interface_abstract as their tc_class_methods fields
                % (which is enforced by our use of item_abstract_typeclass_info
                % here), they are guaranteed to have empty lists as their
                % tc_superclasses and tc_fundeps fields. However, having
                % empty_list as a subtype of list is not yet feasible,
                % for the reason described next to the commented out
                % empty_list subtype definition in library/list.m.
                pti3_int_typeclasses    :: list(item_abstract_typeclass_info),
                pti3_int_instances      :: list(item_abstract_instance_info),
                pti3_int_type_repns         :: type_ctor_repn_map
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
    ;       version_numbers(module_item_version_numbers).

%---------------------------------------------------------------------------%

    % A representation of the contents of .opt files.
    %
:- type parse_tree_plain_opt
    --->    parse_tree_plain_opt(
                ptpo_module_name    :: module_name,

                % The context of the `:- module' declaration.
                ptpo_module_name_context :: prog_context,

                % `:- use_module' (not `:- import_module') declarations.
                ptpo_uses           :: module_names_contexts,
                ptpo_fims           :: set(fim_spec),
                ptpo_type_defns     :: list(item_type_defn_info),
                ptpo_foreign_enums  :: list(item_foreign_enum_info),
                ptpo_inst_defns     :: list(item_inst_defn_info),
                ptpo_mode_defns     :: list(item_mode_defn_info),
                ptpo_typeclasses    :: list(item_typeclass_info),
                ptpo_instances      :: list(item_instance_info),
                ptpo_pred_decls     :: list(item_pred_decl_info),
                ptpo_mode_decls     :: list(item_mode_decl_info),
                ptpo_clauses        :: list(item_clause_info),
                ptpo_foreign_procs  :: list(item_foreign_proc_info),
                ptpo_promises       :: list(item_promise_info),

                ptpo_decl_markers   :: list(item_decl_marker_info_opt),
                ptpo_impl_markers   :: list(item_impl_marker_info_opt),
                ptpo_type_specs     :: list(decl_pragma_type_spec_info),
                ptpo_unused_args    :: list(gen_pragma_unused_args_info),
                ptpo_termination    :: list(decl_pragma_termination_info),
                ptpo_termination2   :: list(decl_pragma_termination2_info),
                ptpo_exceptions     :: list(gen_pragma_exceptions_info),
                ptpo_trailing       :: list(gen_pragma_trailing_info),
                ptpo_mm_tabling     :: list(gen_pragma_mm_tabling_info),
                ptpo_struct_sharing :: list(decl_pragma_struct_sharing_info),
                ptpo_struct_reuse   :: list(decl_pragma_struct_reuse_info)
            ).

    % A representation of the contents of .trans_opt files.
    %
:- type parse_tree_trans_opt
    --->    parse_tree_trans_opt(
                ptto_module_name            :: module_name,

                % The context of the `:- module' declaration.
                ptto_module_name_context    :: prog_context,

                ptto_termination        :: list(decl_pragma_termination_info),
                ptto_termination2       :: list(decl_pragma_termination2_info),
                ptto_exceptions         :: list(gen_pragma_exceptions_info),
                ptto_trailing           :: list(gen_pragma_trailing_info),
                ptto_mm_tabling         :: list(gen_pragma_mm_tabling_info),
                ptto_struct_sharing :: list(decl_pragma_struct_sharing_info),
                ptto_struct_reuse   :: list(decl_pragma_struct_reuse_info)
            ).

%---------------------------------------------------------------------------%
%
% A parse_tree_module_src is one module to be compiled. A parse_tree_src that
% contains N nested submodules corresponds to 1 + N parse_tree_module_srcs,
% one for the top level module, and one for each (possibly deeply) nested
% submodule. The conversion from a parse_tree_src to one or more
% parse_tree_module_srcs is done by two modules. The split_parse_tree_src.m
% module splits up the source file into one or more modules, and then
% convert_parse_tree.m replaces the raw item lists in parse_tree_srcs
% with the more structured representation used by parse_tree_module_srcs.
%
% Before we convert a parse_tree_module_src into the HLDS, we augment it
% with the contents of the interface files of the modules it imports
% (directly or indirectly), and if requested, with the contents of the
% optimization files of those modules as well. The augmented compilation unit
% will consist of the following for compiler invocations that generate
% target language code. (Compiler invocations that generate .int0, .int
% and .int2 files will construct an aug_make_int_unit, not an
% aug_compilation_unit, while compiler invocations that generate .int3 files
% will construct neither.)
%
% - The module_src field contains the original parse_tree_module_src.
%
% - The ancestor_int_specs field contains the .int0 interface files of
%   the ancestors of this module, which are always implicitly imported.
%
% - The direct_int_specs field contains the .int files of the modules
%   directly imported or used by this module, with the "override" exception
%   noted below.
%
% - The indirect_int_specs field contains the .int2 files of the modules
%   indirectly imported or used by this module, again with the "override"
%   exception noted below.
%
%   In this case, module A "indirectly imports or uses" module C if
%   module A imports or uses a module B whose .int file uses module C.
%   (.int files only use modules; they do not import them.)
%
%   The exceptions above are that
%
%   o   if a module's .int0 file is in the ancestor_int_specs field,
%       we don't include its .int1 file in the direct_int_specs field,
%       or its .int2 file in the indirect_int_specs field. In effect,
%       the appearance of a module in the ancestor_int_specs field
%       overrides (i.e. prevents) its appearance in the direct_int_specs
%       or the indirect_int_specs fields.
%
%   o   if a module's .int file is in the direct_int_specs field,
%       we don't include its .int2 file in the indirect_int_specs field.
%       Again, the appearance of a module in the direct_int_specs field
%       overrides its appearance in the indirect_int_specs field.
%
%   The reason for the exceptions is that an .int0 file contains (or at least
%   is intended to contain, which *may* be different) every item that
%   the .int file for the same module contains, and the same relationship
%   holds between .int and .int2 files. The exceptions thus save the compiler
%   from doing work that (a) is unnecessary, and (b) would lead things
%   being declared or defined more than once.
%
% - Provided intermodule optimization is enabled, the plain_opts field
%   will contain
%
%   o   the .opt files of the modules whose .int0, .int or .int2 files
%       are in the ancestor_int_sprcs, direct_int_specs and indirect_int_specs
%       fields above, and
%
%   o   unless the compiler is invoked with --no-read-opt-files-transitively,
%       the .opt files of every other module the .opt files specified
%       by either the previous bullet point or *this* bullet point
%       import either explicitly or implicitly.
%
%   These .opt files are supposed to contain more information about
%   the ancestor-, direct- or indirect-imported modules than their
%   .int0, .int or .int2 files do. Unfortunately, they often also
%   *duplicate* items in those interface files, which leads to
%   double definitions, which the submodules of make_hlds.m have to
%   be prepared to detect and ignore.
%
% - Provided transitive intermodule optimization is enabled, the trans_opts
%   field will contain the .trans_opt files of the modules named in
%   the module's .d file as the module's trans_opt dependencies.
%   XXX This seems to me (zs) a bit too indirect.
%
% - If intermodule optimization is enabled, the int_for_opt_specs field
%   will contain
%
%   o   the .int0 files of the ancestor modules of the modules whose .opt files
%       are in the plain_opts field,
%
%   o   the .int files of the modules imported or used either explicitly
%       or implicitly by the modules whose .opt files are in the plain_opts
%       field, or by their ancestors, and
%
%   o   the .int2 files of the modules used by the .int files in the previous
%       bullet point.
%
%   The idea is that these interface files may in general be needed to define
%   entities (such as types, insts or modes) that the .opt files in the
%   plain_opts field may need.
%
%   XXX There is a problem here, which is that override exception does *not*
%   apply to the int_for_opt_specs field. It is possible for e.g. a module's
%   .int2 file to appear in the indirect_int_specs field, but its .int0 or
%   .int file to appear in the int_for_opt_specs field. This may also lead
%   to double definitions of e.g. types, insts or modes. The compiler does
%   ignore such double definitions, under the principle of generating error
%   messages for double definitions *only* when the entity being double-defined
%   has the module currently being compiled as its module qualifier.
%   Nevertheless, including more than one interface file for any given module
%   in the augmented compilation unit will lead to wasted work, which means
%   that we should avoid doing that if possible.
%

:- type aug_compilation_unit
    --->    aug_compilation_unit(
                % The source code of the module.
                acu_module_src      :: parse_tree_module_src,

                % The interface files of the ancestors of this module.
                % (If we have e.g. module foo.bar among the modules
                % we import int_for_opt, we also need to grab its ancestor foo,
                % but such .int0 files also go into the int_for_opt field.
                acu_ancestor_ints   :: map(module_name, ancestor_int_spec),

                % The interface files of directly imported modules.
                acu_direct_int1s    :: map(module_name, direct_int1_spec),

                % The interface files of indirectly imported modules.
                acu_indirect_int2s  :: map(module_name, indirect_int2_spec),

                % The optimization files of directly or indirectly
                % imported modules.
                acu_plain_opts      :: map(module_name, parse_tree_plain_opt),
                acu_trans_opts      :: map(module_name, parse_tree_trans_opt),

                % The interface files needed to make sense
                % of those optimization files.
                acu_int_for_opts    :: map(module_name, int_for_opt_spec),

                % Interface files that we read in only for the type
                % representation information they contain
                acu_type_repns      :: map(module_name, type_repn_spec),

                % The module_version_numbers records in all the imported
                % interface files.
                acu_item_version_map:: module_item_version_numbers_map
            ).

:- type aug_make_int_unit
    --->    aug_make_int_unit(
                % The source code of the module.
                %
                % Note that any "missing parent imports" 
                amiu_module_src     :: parse_tree_module_src,

                % The list of messages for errors in amiu_module_src
                % whose reporting we would like to delay until a compiler
                % invocation that generates target code.
                %
                % We report these messages *only* if the creation of the
                % .int[012] file for the module cannot succeed for
                % *other* reasons. In that case, not reporting them
                % would give the programmer
                an incomplete picture of what is wrong.
                amiu_delayed_specs  :: list(error_spec),

                % The interface files of the ancestors of this module.
                % (The read_why_int0 is always implicitly rwi0_section.)
                amiu_ancestor_ints  :: map(module_name, parse_tree_int0),

                % The interface files of directly imported modules.
                amiu_direct_int3s   :: map(module_name, direct_int3_spec),

                % The interface files of indirectly imported modules.
                amiu_indirect_int3s :: map(module_name, indirect_int3_spec),

                % The module_version_numbers records in all the imported
                % interface files.
                amiu_item_version_map :: module_item_version_numbers_map
            ).

    % init_aug_compilation_unit(ParseTreeModuleSrc, AugCompUnit):
    %
    % Initialize an augmented compilation unit structure. Put the given
    % ParseTreeModuleSrc into it, and leave the rest of the structure empty.
    % Our caller is the expected to fill in (i.e. augment) the structure
    % by calling the aug_compilation_unit_add_X predicates in grab_modules.
    % to add the parse trees of the interface and optimization files needed
    % to compile ParseTreeModuleSrc.
    %
:- pred init_aug_compilation_unit(parse_tree_module_src::in,
    aug_compilation_unit::out) is det.

:- type ancestor_int_spec
    --->    ancestor_int0(parse_tree_int0, read_why_int0).

:- type direct_int1_spec
    --->    direct_int1(parse_tree_int1, read_why_int1).

:- type direct_int3_spec
    --->    direct_int3(parse_tree_int3, read_why_int3).

:- type indirect_int2_spec
    --->    indirect_int2(parse_tree_int2, read_why_int2).

:- type indirect_int3_spec
    --->    indirect_int3(parse_tree_int3, read_why_int3).

:- type int_for_opt_spec
    --->    for_opt_int0(parse_tree_int0, read_why_int0)
    ;       for_opt_int1(parse_tree_int1, read_why_int1)
    ;       for_opt_int2(parse_tree_int2, read_why_int2).

:- type type_repn_spec
    --->    type_repn_spec_int1(parse_tree_int1).

    % All these record recomp_avail_int_import as recompilation reason.
    % (Since there is no recomp_avail_ancestor_import, yet).
:- type read_why_int0
    --->    rwi0_section
            % Add the parse tree to the set of directly-read interfaces.
    ;       rwi0_opt.
            % Add the parse tree to the set of read-int-for-opt interfaces.

:- type read_why_int1
    --->    rwi1_int_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_import as recompilation reason.
    ;       rwi1_int_use
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_use as recompilation reason.
    ;       rwi1_imp_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_imp_import as recompilation reason.
    ;       rwi1_imp_use
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_imp_use as recompilation reason.
    ;       rwi1_int_use_imp_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_use_imp_import as recompilation reason.
    ;       rwi1_opt
            % Add the parse tree to the set of read-int-for-opt interfaces.
            %
            % Record recomp_avail_imp_use as recompilation reason.
    ;       rwi1_type_repn.
            % The only items that should be paid attention to from this
            % .int file are the type_repn items. They don't need any
            % section markers.
            %
            % Add the parse tree to the type-repn interfaces.
            %
            % Record recomp_avail_int_import as recompilation reason.
            % XXX TYPE_REPN This is a lie, but it is the best we can do now,
            % because smart recompilation "cannot handle the truth",
            % due to not yet having been adapted to handle dependencies
            % on interface files that are needed only for type representation
            % information.

    % All these record recomp_avail_imp_use as recompilation reason.
:- type read_why_int2
    --->    rwi2_int_use
            % Add the parse tree to the set of indirectly-read interfaces.
    ;       rwi2_imp_use
            % Add the parse tree to the set of indirectly-read interfaces.
    ;       rwi2_abstract
            % Add the parse tree to the set of indirectly-read interfaces.
    ;       rwi2_opt.
            % Add the parse tree to the set of read-int-for-opt interfaces.
            % XXX TYPE_REPN Do we need a rwi2_type_repn?

:- type read_why_int3
    --->    rwi3_direct_ancestor_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_import as recompilation reason.
            % (Since there is no recomp_avail_ancestor_import, yet).
    ;       rwi3_direct_int_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_import as recompilation reason.
    ;       rwi3_direct_imp_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_imp_import as recompilation reason.
    ;       rwi3_direct_ancestor_use
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_use as recompilation reason.
            % (Since there is no recomp_avail_ancestor_use, yet).
    ;       rwi3_direct_int_use
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_use as recompilation reason.
    ;       rwi3_direct_imp_use
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_imp_use as recompilation reason.
    ;       rwi3_direct_int_use_imp_import
            % Add the parse tree to the set of directly-read interfaces.
            %
            % Record recomp_avail_int_use_imp_import as recompilation reason.
    ;       rwi3_indirect_int_use
            % Add the parse tree to the set of indirectly-read interfaces.
            %
            % Record recomp_avail_int_use as recompilation reason.
            % (Since there is no recomp_avail_indirect_use_int, yet).
    ;       rwi3_indirect_imp_use.
            % Add the parse tree to the set of indirectly-read interfaces.
            %
            % Record recomp_avail_imp_use as recompilation reason.
            % (Since there is no recomp_avail_indirect_use_imp, yet).

%---------------------------------------------------------------------------%

:- type module_section
    --->    ms_interface
    ;       ms_implementation.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

init_aug_compilation_unit(ParseTreeModuleSrc, AugCompUnit) :-
    map.init(AncestorIntSpecs),
    map.init(DirectIntSpecs),
    map.init(IndirectIntSpecs),
    map.init(PlainOpts),
    map.init(TransOpts),
    map.init(IntForOptSpecs),
    map.init(TypeRepnSpecs),
    map.init(VersionNumbers),
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs, VersionNumbers).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_parse_tree.
%---------------------------------------------------------------------------%
