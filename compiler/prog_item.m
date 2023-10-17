%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_item.m.
% Original author: fjh.
%
% This module, together with prog_data*.m, defines a data structure for
% representing Mercury programs.
%
% This data structure specifies basically the same information as is
% contained in the source code, but in a parse tree rather than a flat file.
% This module defines the parts of the parse tree that are *not* needed
% by the various compiler backends; parts of the parse tree that
% are needed by the backends are contained in prog_data*.m.
%
%---------------------------------------------------------------------------%
%
% One important consideration in the design of the parse trees is that
% they have two different use cases:
%
% - to represent files being read in, and
% - to represent files being written out.
%
% The two have slightly different requirements, which is why in several
% kinds of parse trees seemingly the same information is present in
% more than one set of fields. This is because while we will never
% knowingly write out erroneous Mercury code, we know that we *will*
% read in some. An example is import_module and use_module declarations.
% Each parse_tree_module_src contains four fields that respectively specify
%
% - the locations where a module has an import_module in the interface
% - the locations where a module has an use_module in the interface
% - the locations where a module has an import_module in the implementation
% - the locations where a module has an use_module in the implementation
%
% It is an error if a module has an entry in more than one of these maps,
% with the sole exception being the use_module in interface and import_module
% in implementation combination (because each grants a permission that the
% other does not). Yet we want the ability to represent even invalid
% combinations of these declarations, so that we can wait to generate
% the appropriate error messages until we know all the relevant facts.
% And in the process of checking for and reporting errors, we build up
% another data structure, the import_and_or_use_map, which contains
% a record of how the rest of the compiler should view, not just the
% import_module and use_module declarations explicitly present in the
% source code, but also the ones that get made available to it implicitly.
% (Examples include builtin and private_builtin, which are implicitly available
% to every module, and table_builtin, which is implicitly available
% to modules that do certain kinds of tabling.)
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_item.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module recompilation.                 % XXX undesirable dependency

:- import_module assoc_list.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module set.

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

:- type module_name_context == map(module_name, prog_context).
:- type module_names_contexts == one_or_more_map(module_name, prog_context).

    % Maps from module names to the includes, imports or uses
    % in the named section. The code creating these maps will have
    % detected and diagnosed any duplicate entries of the same kind
    % of declaration for the same module in the same section.
    % However, unlike include_module_maps or import_and_or_use_maps,
    % which summarize the information in the first two of the maps below
    % (for include_module_map) or the last four (for import_and_or_use_map),
    % these maps may contain redundant entries as long as they are all
    % in *different* maps (such as the module name A occurring in both
    % the int_import_context_map and the int_use_context_map of module B).
:- type int_incl_context_map
    --->    int_incl_context_map(module_name_context).
:- type imp_incl_context_map
    --->    imp_incl_context_map(module_name_context).
:- type int_import_context_map
    --->    int_import_context_map(module_name_context).
:- type int_use_context_map
    --->    int_use_context_map(module_name_context).
:- type imp_import_context_map
    --->    imp_import_context_map(module_name_context).
:- type imp_use_context_map
    --->    imp_use_context_map(module_name_context).

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
                % (b) are phase_type_inst_mode_check_invalid_type.
                ptms_type_specs             :: list(error_spec),
                % The error messages generated during the construction
                % of ptms_inst_defns and ptms_mode_defns. We have found
                % some invalid insts and/or modes if some of these error_specs
                % (a) are severity_error, and (b) are
                % phase_type_inst_mode_check_invalid_inst_mode.
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

% The representations specific to .int0, .int, .int2 and .int3 files.
% XXX We should replace the lists of items of various kinds with data
% structures that encode uniqueness properties, such as "each type constructor
% may be defined only once". Maps from primary keys such as type_ctors,
% or symnames/arity pairs in general, would work for this.

    % A representation of the contents of .int0 files.
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
                % empty_list subtype definition in librrary/list.m.
                pti3_int_typeclasses    :: list(item_abstract_typeclass_info),
                pti3_int_instances      :: list(item_abstract_instance_info),
                pti3_int_type_repns         :: type_ctor_repn_map
            ).

%---------------------------------------------------------------------------%
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
% The third use is to help deal with sets of definitions that
% don't make sense. There are many rules that a set of definitions
% for a given type constructor must meet (such as "there may be at most one
% definition for a type constructor that is a du, equivalence or solver
% definition), and bugs may manifest themselves as violations of these rules.
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
% We implement the first choice by checking whether each entry in
% a type_ctor_defn_map makes sense, and generating error messages
% when they don't. This is done by code in check_type_inst_mode_defns.m.
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
                % Abstract and nonabstract solver type definitions.
                tcad_abstract_solver    :: list(item_type_defn_info_abstract),
                tcad_solver             :: list(item_type_defn_info_solver),

                % Abstract and nonabstract nonsolver type definitions.
                tcad_abstract_std       :: list(item_type_defn_info_abstract),
                tcad_eqv                :: list(item_type_defn_info_eqv),
                tcad_du                 :: list(item_type_defn_info_du),
                tcad_sub                :: list(item_type_defn_info_sub),
                tcad_foreign            :: c_j_cs_defns
            ).

:- type type_ctor_maybe_defn
    --->    type_ctor_maybe_defn(
                % Abstract and nonabstract solver type definitions.
                tcmd_abstract_solver    :: maybe(item_type_defn_info_abstract),
                tcmd_solver             :: maybe(item_type_defn_info_solver),

                % Abstract and nonabstract nonsolver type definitions.
                tcmd_abstract_std       :: maybe(item_type_defn_info_abstract),
                tcmd_eqv                :: maybe(item_type_defn_info_eqv),
                tcmd_du                 :: maybe(item_type_defn_info_du),
                tcmd_sub                :: maybe(item_type_defn_info_sub),
                tcmd_foreign            :: c_j_cs_maybe_defn
            ).

    % We support foreign type definitions in all three of our target languages,
    % C, Java and C#. Likewise, we allow foreign enum declarations
    % in these three languages.
    %
    % There are several kinds of info that we may want to store for every
    % one of these foreign languages. This can be done in instances
    % of this type, whose fields always contain the info for C, Java and C#
    % (in that order).
:- type c_java_csharp(T)
    --->    c_java_csharp(T, T, T).

:- type c_j_cs_defns ==
    c_java_csharp(list(item_type_defn_info_foreign)).
:- type c_j_cs_maybe_defn ==
    c_java_csharp(maybe(item_type_defn_info_foreign)).
:- type c_j_cs_enums ==
    c_java_csharp(list(item_foreign_enum_info)).
:- type c_j_cs_maybe_enum ==
    c_java_csharp(maybe(item_foreign_enum_info)).
:- type c_j_cs_repn ==
    c_java_csharp(maybe(foreign_type_repn)).
:- type c_j_cs_enum_repn ==
    c_java_csharp(maybe(enum_foreign_repn)).

:- type inst_ctor_defn_map == map(inst_ctor, inst_ctor_all_defns).
:- type inst_ctor_all_defns
    --->    inst_ctor_all_defns(
                icad_abstract           :: list(item_inst_defn_info_abstract),
                icad_eqv                :: list(item_inst_defn_info_eqv)
            ).

:- type mode_ctor_defn_map == map(mode_ctor, mode_ctor_all_defns).
:- type mode_ctor_all_defns
    --->    mode_ctor_all_defns(
                mcad_abstract           :: list(item_mode_defn_info_abstract),
                mcad_eqv                :: list(item_mode_defn_info_eqv)
            ).

:- type type_ctor_foreign_enum_map == map(type_ctor, c_j_cs_enums).

:- type type_ctor_repn_map == map(type_ctor, item_type_repn_info).

%---------------------------------------------------------------------------%

:- type parse_tree_plain_opt
    --->    parse_tree_plain_opt(
                ptpo_module_name            :: module_name,

                % The context of the `:- module' declaration.
                ptpo_module_name_context    :: prog_context,

                % `:- use_module' (not `:- import_module') declarations.
                ptpo_uses                   :: module_names_contexts,
                ptpo_fims                   :: set(fim_spec),
                ptpo_type_defns             :: list(item_type_defn_info),
                ptpo_foreign_enums          :: list(item_foreign_enum_info),
                ptpo_inst_defns             :: list(item_inst_defn_info),
                ptpo_mode_defns             :: list(item_mode_defn_info),
                ptpo_typeclasses            :: list(item_typeclass_info),
                ptpo_instances              :: list(item_instance_info),
                ptpo_pred_decls             :: list(item_pred_decl_info),
                ptpo_mode_decls             :: list(item_mode_decl_info),
                ptpo_clauses                :: list(item_clause_info),
                ptpo_foreign_procs          :: list(item_foreign_proc_info),
                ptpo_promises               :: list(item_promise_info),

                ptpo_decl_markers       :: list(item_decl_marker_info_opt),
                ptpo_impl_markers       :: list(item_impl_marker_info_opt),
                ptpo_type_specs         :: list(decl_pragma_type_spec_info),
                ptpo_unused_args        :: list(gen_pragma_unused_args_info),
                ptpo_termination        :: list(decl_pragma_termination_info),
                ptpo_termination2       :: list(decl_pragma_termination2_info),
                ptpo_exceptions         :: list(gen_pragma_exceptions_info),
                ptpo_trailing           :: list(gen_pragma_trailing_info),
                ptpo_mm_tabling         :: list(gen_pragma_mm_tabling_info),
                ptpo_struct_sharing :: list(decl_pragma_struct_sharing_info),
                ptpo_struct_reuse   :: list(decl_pragma_struct_reuse_info)
            ).

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
% submodule.
%
% A raw compilation unit consists of some raw item blocks, with each raw
% item block containing the items in an interface or implementation section
% of its module.
%
% Before we convert a parse_tree_module_src into the HLDS, we augment it
% with the contents of the interface files of the modules it imports
% (directly or indirectly), and if requested, with the contents of the
% optimization files of those modules as well. The augmented compilation unit
% will consist of the following for compiler invocations that generate
% target language code. (Compiler invocations that generate .int and .int2
% files will construct an aug_make_int_unit, not an aug_compilation_unit.)
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
                acu_module_src                  :: parse_tree_module_src,

                % The interface files of the ancestors of this module.
                % (If we have e.g. module foo.bar among the modules
                % we import int_for_opt, we also need to grab its ancestor foo,
                % but such .int0 files also go into the int_for_opt field.
                acu_ancestor_int_specs          :: map(module_name,
                                                    ancestor_int_spec),

                % The interface files of directly imported modules.
                acu_direct_int1_specs           :: map(module_name,
                                                    direct_int1_spec),

                % The interface files of indirectly imported modules.
                acu_indirect_int2_specs         :: map(module_name,
                                                    indirect_int2_spec),

                % The optimization files of directly or indirectly
                % imported modules.
                acu_plain_opts                  :: map(module_name,
                                                    parse_tree_plain_opt),
                acu_trans_opts                  :: map(module_name,
                                                    parse_tree_trans_opt),

                % The interface files needed to make sense
                % of those optimization files.
                acu_int_for_opt_specs           :: map(module_name,
                                                    int_for_opt_spec),

                % Interface files that we read in only for the type
                % representation information they contain
                acu_type_repn_specs             :: map(module_name,
                                                    type_repn_spec),

                % The module_version_numbers records in all the imported
                % interface files.
                acu_module_item_version_numbers_map ::
                                                module_item_version_numbers_map
            ).

:- type aug_make_int_unit
    --->    aug_make_int_unit(
                % The source code of the module.
                amiu_module_src                 :: parse_tree_module_src,

                % The interface files of the ancestors of this module.
                % (The read_why_int0 is always implicitly rwi0_section.)
                amiu_ancestor_int_specs         :: map(module_name,
                                                    parse_tree_int0),

                % The interface files of directly imported modules.
                amiu_direct_int3_specs          :: map(module_name,
                                                    direct_int3_spec),

                % The interface files of indirectly imported modules.
                amiu_indirect_int3_specs        :: map(module_name,
                                                    indirect_int3_spec),

                % The module_version_numbers records in all the imported
                % interface files.
                amiu_module_item_version_numbers_map ::
                                                module_item_version_numbers_map
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
    ;       compiler_origin_class_method(
                cm_class_id                     :: class_id,
                cm_method                       :: pred_pf_name_arity
            )
    ;       compiler_origin_solver_repn(
                cosr_type_ctor                  :: type_ctor,
                cosr_aux_pred_kind              :: solver_type_pred_kind
            )
    ;       compiler_origin_mutable(
                com_module_name                 :: module_name,
                com_mutable_name                :: string,
                com_aux_pred_kind               :: mutable_pred_kind
            )
    ;       compiler_origin_tabling(
                cot_pred_spec                   :: pred_pf_name_arity,
                cot_aux_pred_kind               :: tabling_aux_pred_kind
            ).

:- type item
    --->    item_clause(item_clause_info)
    ;       item_type_defn(item_type_defn_info)
    ;       item_inst_defn(item_inst_defn_info)
    ;       item_mode_defn(item_mode_defn_info)
    ;       item_pred_decl(item_pred_decl_info)
    ;       item_mode_decl(item_mode_decl_info)
    ;       item_foreign_proc(item_foreign_proc_info)
    ;       item_foreign_enum(item_foreign_enum_info)
    ;       item_foreign_export_enum(item_foreign_export_enum_info)
    ;       item_decl_pragma(item_decl_pragma_info)
    ;       item_decl_marker(item_decl_marker_info)
    ;       item_impl_pragma(item_impl_pragma_info)
    ;       item_impl_marker(item_impl_marker_info)
    ;       item_generated_pragma(item_generated_pragma_info)
    ;       item_promise(item_promise_info)
    ;       item_typeclass(item_typeclass_info)
    ;       item_instance(item_instance_info)
    ;       item_initialise(item_initialise_info)
    ;       item_finalise(item_finalise_info)
    ;       item_mutable(item_mutable_info)
    ;       item_type_repn(item_type_repn_info).

:- type item_clause_info
    --->    item_clause_info(
                cl_pred_or_func                 :: pred_or_func,
                cl_predname                     :: sym_name,
                cl_head_args                    :: list(prog_term),
                cl_varset                       :: prog_varset,
                cl_body                         :: maybe2(goal,
                                                    list(warning_spec)),
                cl_context                      :: prog_context,
                cl_seq_num                      :: item_seq_num
            ).

:- type item_type_defn_info == item_type_defn_info_general(type_defn).

:- type item_type_defn_info_abstract
    == item_type_defn_info_general(type_details_abstract).
:- type item_type_defn_info_solver
    == item_type_defn_info_general(type_details_solver).
:- type item_type_defn_info_eqv
    == item_type_defn_info_general(type_details_eqv).
:- type item_type_defn_info_du
    == item_type_defn_info_general(type_details_du).
:- type item_type_defn_info_sub
    == item_type_defn_info_general(type_details_sub).
:- type item_type_defn_info_foreign
    == item_type_defn_info_general(type_details_foreign_generic).

:- type item_type_defn_info_general(T)
    --->    item_type_defn_info(
                % `:- type ...':
                % a definition of a type, or a declaration of an abstract type.
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: T,
                td_tvarset                      :: tvarset,
                td_context                      :: prog_context,
                td_seq_num                      :: item_seq_num
            ).

:- type item_inst_defn_info
    == item_inst_defn_info_general(maybe_abstract_inst_defn).

:- type item_inst_defn_info_abstract
    == item_inst_defn_info_general(no_inst_defn).
:- type item_inst_defn_info_eqv
    == item_inst_defn_info_general(inst_defn).

:- type item_inst_defn_info_general(T)
    --->    item_inst_defn_info(
                % `:- inst ... = ...':
                % a definition of an inst.
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_maybe_for_type               :: maybe(type_ctor),
                id_inst_defn                    :: T,
                id_varset                       :: inst_varset,
                id_context                      :: prog_context,
                id_seq_num                      :: item_seq_num
            ).

:- type no_inst_defn
    --->    no_inst_defn.

:- type maybe_abstract_inst_defn
    --->    abstract_inst_defn
    ;       nonabstract_inst_defn(inst_defn).

:- type item_mode_defn_info
    == item_mode_defn_info_general(maybe_abstract_mode_defn).

:- type item_mode_defn_info_abstract
    == item_mode_defn_info_general(no_mode_defn).
:- type item_mode_defn_info_eqv
    == item_mode_defn_info_general(mode_defn).

:- type item_mode_defn_info_general(T)
    --->    item_mode_defn_info(
                % `:- mode ... = ...':
                % a definition of a mode.
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: T,
                md_varset                       :: inst_varset,
                md_context                      :: prog_context,
                md_seq_num                      :: item_seq_num
            ).

:- type no_mode_defn
    --->    no_mode_defn.

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
                pf_seq_num                      :: item_seq_num
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
                pfm_seq_num                     :: item_seq_num
            ).

:- type item_foreign_proc_info
    --->    item_foreign_proc_info(
                % Set of foreign proc attributes, such as:
                %   what language this code is in
                %   whether or not the code may call Mercury,
                %   whether or not the code is thread-safe
                % PredName, Predicate or Function, Vars/Mode,
                % VarNames, Foreign Code Implementation Info
                proc_attrs                      :: foreign_proc_attributes,
                proc_name                       :: sym_name,
                proc_p_or_f                     :: pred_or_func,
                proc_vars                       :: list(pragma_var),
                proc_varset                     :: prog_varset,
                proc_instvarset                 :: inst_varset,
                proc_impl                       :: pragma_foreign_proc_impl,
                proc_context                    :: prog_context,
                proc_seq_num                    :: item_seq_num
            ).

:- type item_foreign_enum_info
    --->    item_foreign_enum_info(
                fe_language                     :: foreign_language,
                fe_type_ctor                    :: type_ctor,
                fe_values                       :: one_or_more(
                                                    pair(sym_name, string)),
                fe_context                      :: prog_context,
                fe_seq_num                      :: item_seq_num
            ).

:- type foreign_enum_spec
    --->    foreign_enum_spec(
                foreign_language,
                type_ctor,
                one_or_more(pair(sym_name, string))
            ).

:- type item_foreign_export_enum_info
    --->    item_foreign_export_enum_info(
                fee_language                    :: foreign_language,
                fee_type_ctor                   :: type_ctor,
                fee_attributes                  :: export_enum_attributes,
                fee_overrides                   :: assoc_list(sym_name,
                                                    string),
                fee_context                     :: prog_context,
                fee_seq_num                     :: item_seq_num
            ).

:- type item_promise_info
    --->    item_promise_info(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: list(prog_var),
                prom_context                    :: prog_context,
                prom_seq_num                    :: item_seq_num
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
                tc_seq_num                      :: item_seq_num
            ).

:- type item_abstract_typeclass_info =< item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                tc_superclasses                 :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: abstract_class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: item_seq_num
            ).

:- type item_abstract_int3_typeclass_info =< item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                % XXX Both of the following should be empty_lists,
                % if the definition of that subtype in library/list.m
                % worked.
                tc_superclasses                 :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: abstract_class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: item_seq_num
            ).

:- type item_instance_info
    --->    item_instance_info(
                % The original types field preserves the types in the instance
                % declaration as written by the programmer. The types field
                % is subject to the expansion of equivalence types.
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_original_types               :: list(mer_type),
                ci_deriving_class               :: list(prog_constraint),
                ci_method_instances             :: instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: item_seq_num
            ).

:- type item_abstract_instance_info =< item_instance_info
    --->    item_instance_info(
                % The original types field preserves the types in the instance
                % declaration as written by the programmer. The types field
                % is subject to the expansion of equivalence types.
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_original_types               :: list(mer_type),
                ci_deriving_class               :: list(prog_constraint),
                ci_method_instances             :: abstract_instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: item_seq_num
            ).

:- type item_initialise_info
    --->    item_initialise_info(
                % :- initialise pred_name.
                init_name                       :: sym_name,
                init_arity                      :: user_arity,
                init_maybe_attrs                :: item_maybe_attrs,
                init_context                    :: prog_context,
                init_seq_num                    :: item_seq_num
            ).

:- type item_finalise_info
    --->    item_finalise_info(
                % :- finalise pred_name.
                final_name                      :: sym_name,
                final_arity                     :: user_arity,
                final_maybe_attrs               :: item_maybe_attrs,
                final_context                   :: prog_context,
                final_seq_num                   :: item_seq_num
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
                mut_seq_num                     :: item_seq_num
            ).

:- type item_type_repn_info_eqv
    == item_type_repn_info_general(mer_type).
:- type item_type_repn_info_subtype
    == item_type_repn_info_general(type_ctor).
:- type item_type_repn_info
    == item_type_repn_info_general(type_ctor_repn_info).

:- type item_type_repn_info_general(T)
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
                tr_ctor_repn_info               :: T,
                tr_tvarset                      :: tvarset,
                tr_context                      :: prog_context,
                tr_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%
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
                incl_seq_num                    :: item_seq_num
            ).

:- type import_or_use
    --->    import_decl
    ;       use_decl.

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
                aii_seq_num         :: item_seq_num
            ).
:- type avail_use_info
    --->    avail_use_info(
                aui_module_name     :: module_name,
                aui_context         :: prog_context,
                aui_seq_num         :: item_seq_num
            ).

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
                fim_seq_num                     :: item_seq_num
            ).

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%
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

    % Attributes for mutable variables.
    %
:- type mutable_var_attributes
    --->    mutable_var_attributes(
                mutable_foreign_names       :: map(foreign_language, string),
                mutable_constant            :: mutable_maybe_constant
            ).

:- type mutable_maybe_constant
    --->    mutable_is_constant
            % implies mutable_dont_attach_to_io_state
            % implies mutable_untrailed
            % implies mutable_not_thread_local
    ;       mutable_is_not_constant(
                mutable_attach_to_io_state,
                mutable_maybe_thread_local
            ).

:- type mutable_maybe_thread_local
    --->    mutable_is_not_thread_local(
                mutable_trailed
            )
    ;       mutable_is_thread_local.
            % implies mutable_untrailed

:- func mutable_var_thread_local(mutable_maybe_constant)
    = mutable_thread_local.
:- func mutable_thread_local_trailed(mutable_maybe_thread_local)
    = mutable_trailed.

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of type and
% foreign enum definitions for every type constructor defined in a module.
%

:- type type_ctor_checked_map == map(type_ctor, type_ctor_checked_defn).

    % A type is either a solver type, or not.
:- type type_ctor_checked_defn
    --->    checked_defn_solver(solver_type_defn, src_defns_solver)
    ;       checked_defn_std(std_type_defn, src_defns_std).

%---------------------%

    % Replace this one general type with one type for each function symbol
    % in solver_type_defn.
:- type src_defns_solver
    --->    src_defns_solver(
                % The item_type_defn_info (if any) in the interface section.
                maybe(item_type_defn_info),

                % The item_type_defn_info (if any) in the impl section.
                maybe(item_type_defn_info)
            ).

    % Replace this one general type with one type for each function symbol
    % in std_type_defn.
:- type src_defns_std
    --->    src_defns_std(
                % The item_type_defn_infos in the interface section.
                list(item_type_defn_info),

                % The item_type_defn_infos and item_foreign_enum_infos
                % in the implementation section.
                list(item_type_defn_info),
                list(item_foreign_enum_info)
            ).

%---------------------%

:- type solver_type_defn
    --->    solver_type_abstract(
                abstract_solver_type_status,

                % The abstract definition. It may be in either section;
                % the status specifies the section.
                item_type_defn_info_abstract
            )
    ;       solver_type_full(
                % The abstract definition in the interface section,
                % if one exists.
                maybe(item_type_defn_info_abstract),

                % The full solver type definition, which must be in the
                % implementation section.
                item_type_defn_info_solver
            ).

:- type abstract_solver_type_status
    --->    abstract_solver_type_exported
            % The type name is exported. The abstract definition
            % is in the interface section.
    ;       abstract_solver_type_private.
            % The type name is not exported. The abstract definition
            % is in the implementation section.

%---------------------%

:- type std_type_defn
    --->    std_mer_type_eqv(
                std_eqv_type_status,

                % The equivalence type definition.
                item_type_defn_info_eqv
            )
    ;       std_mer_type_subtype(
                std_subtype_status,

                % The subtype definition.
                item_type_defn_info_sub
            )
    ;       std_mer_type_du_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition which represents
                % either a direct dummy type or an enum.
                item_type_defn_info_du,

                % The first functor name in the type, and any later functor
                % names. If there are no later functor names, then the type
                % is a direct dummy type, and must satisfy the requirements
                % of non_sub_du_type_is_dummy; if there are, then the type
                % is an enum type, and must satisfy the requirements of
                % non_sub_du_type_is_enum. (Function symbols that do not meet
                % the relevant requirements may be constants, but we
                % don't consider them *plain* constants.)
                string,
                list(string),

                % For each of our target foreign languages, this field
                % specifies whether we have either a foreign language
                % definition for this type, or a foreign enum definition.
                %
                % While the Mercury representation uses small integers
                % allocated consecutively from 0 to represent function symbols,
                % this is not true even for foreign enum definitions,
                % much less foreign type definitions.
                c_j_cs_maybe_defn_or_enum
            )
    ;       std_mer_type_du_not_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition which represents
                % a type *other* than a direct dummy type or an enum.
                item_type_defn_info_du,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            )
    ;       std_mer_type_abstract(
                std_abs_type_status,

                % The abstract declaration of the type (not a subtype).
                item_type_defn_info_abstract,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            ).

:- type maybe_only_constants
    --->    not_only_plain_constants
    ;       only_plain_constants(
                % The names of the constants, in the order of declaration.
                opc_head_name       :: string,
                opc_tail_names      :: list(string)
            ).

:- type std_eqv_type_status
    --->    std_eqv_type_mer_exported
            % The Mercury definition (i.e. the equivalence) is exported.
    ;       std_eqv_type_abstract_exported
            % Only the type name is exported. The Mercury definition
            % is private.
    ;       std_eqv_type_all_private.
            % Everything about the type is private.

:- type std_du_type_status
    --->    std_du_type_mer_ft_exported
            % Both the Mercury and any foreign type definitions are exported.
            % Any foreign enum definitions are private, as they have to be.
            % This status is not applicable to equivalence types or subtypes,
            % since they may not have foreign type definitions.
    ;       std_du_type_mer_exported
            % The Mercury definition is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_du_type_abstract_exported
            % Only the type name is exported. The Mercury definition and
            % any foreign type definitions and/or foreign enum definitions
            % are private.
    ;       std_du_type_all_private.
            % Everything about the type is private.

    % A version of std_du_type_status for subtypes, which may not have
    % any foreign type definitions, and for which therefore the question of
    % whether any foreign type definitions are exported is moot.
:- type std_subtype_status
    --->    std_sub_type_mer_exported
    ;       std_sub_type_abstract_exported
    ;       std_sub_type_all_private.

:- type std_abs_type_status
    --->    std_abs_type_ft_exported
            % The type has foreign type definitions that are exported.
            % Any foreign enum definitions are private, as they have to be.
    ;       std_abs_type_abstract_exported
            % Only the type name is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_abs_type_all_private.
            % Everything about the type is private.

%---------------------%

:- type c_j_cs_maybe_defn_or_enum ==
    c_java_csharp(maybe(foreign_type_or_enum)).

:- type foreign_type_or_enum
    --->    foreign_type_or_enum_type(item_type_defn_info_foreign)
    ;       foreign_type_or_enum_enum(checked_foreign_enum).

    % Part of checking a foreign enum definition is checking whether
    % the correspondence it describes between the Mercury functors
    % of the type on the one hand and their foreign language counterparts
    % on the other hand is a bijection. If it is, then the second argument
    % of the checked_foreign_enum we construct gives the foreign language
    % counterpart of each Mercury function symbol in the type in the order
    % in which the Mercury function symbols are defined.
    %
    % For example, given
    %
    %   :- type t ---> m1 ; m2 ; m3.
    %
    % and a foreign enum definition that gives the correspondence correctly
    % but in a different order, such as
    %
    %   :- pragma foreign_enum("C", t/0, [m2 - "f2", m3 - "f3", m1 - "f1"]).
    %
    % the second argument will contain the (nonempty) list "f1", "f2", "f3".
    %
    % On the other hand, if the mapping in the foreign enum definition is
    % *not* a bijection, then we will not generate a checked_foreign_enum
    % structure for it.
    %
:- type checked_foreign_enum
    --->    checked_foreign_enum(item_foreign_enum_info, one_or_more(string)).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of inst definitions
% for every inst constructor defined in a module.
%

:- type inst_ctor_checked_map == map(inst_ctor, inst_ctor_checked_defn).

:- type inst_ctor_checked_defn
    --->    checked_defn_inst(std_inst_defn, src_defns_inst).

:- type std_inst_defn
    --->    std_inst_defn(std_inst_status, item_inst_defn_info).

:- type std_inst_status
    --->    std_inst_exported
            % The inst definition is exported.
    ;       std_inst_abstract_exported
            % Only the inst name is exported. Its definition is private.
    ;       std_inst_all_private.
            % Everything about the inst is private.

:- type src_defns_inst
    --->    src_defns_inst(
                % The inst definition (if any) in the interface.
                maybe(item_inst_defn_info),

                % The inst definition (if any) in the implementation.
                maybe(item_inst_defn_info)
            ).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of mode definitions
% for every mode constructor defined in a module.
%

:- type mode_ctor_checked_map == map(mode_ctor, mode_ctor_checked_defn).

:- type mode_ctor_checked_defn
    --->    checked_defn_mode(std_mode_defn, src_defns_mode).

:- type std_mode_defn
    --->    std_mode_defn(std_mode_status, item_mode_defn_info).

:- type std_mode_status
    --->    std_mode_exported
            % The mode definition is exported.
    ;       std_mode_abstract_exported
            % Only the mode name is exported. Its definition is private.
    ;       std_mode_all_private.
            % Everything about the mode is private.

:- type src_defns_mode
    --->    src_defns_mode(
                % The mode definition (if any) in the interface.
                maybe(item_mode_defn_info),

                % The mode definition (if any) in the implementation.
                maybe(item_mode_defn_info)
            ).

%---------------------------------------------------------------------------%
%
% Information about the representations of types defined in other modules.
%

    % This type and type_ctor_checked_defn are closely related.
    % The principal differences are the following.
    %
    % - type_ctor_checked_defn deals with solver types. Since solver types
    %   have no representation information themselves (they are represented
    %   by values of another type), this type does not deal with them.
    %
    % - One of the purposes of type_ctor_checked_defn is to decide
    %   what items to include in interface files, for use by code using
    %   the compiler's ancient approach to deciding type representation,
    %   where each compiler invocation that generated code decided for itself
    %   how every type it had access to was represented, including the types
    %   imported from other modules. This means that it needs to contain
    %   either whole items (of particular kinds), or information from which
    %   whole items can be reconstructed.
    %
    % - The above consideration also requires a type_ctor_checked_defn
    %   to specify the status of the type. On the other hand, values of
    %   this type have no use for status information. Status information
    %   is used only for checking whether an access to a type should be
    %   allowed or not; the only use of values of this type is to help
    %   compute type representations.
    %
    % - Only this type needs to contain representation information.
    %   A value of the type_ctor_checked_defn type needs to contain *part*
    %   of the information from which this representation information is
    %   computed for its type, but not *all* of it; some of that information
    %   comes from information about the representation of *other* types.
    %
    % One sort-of difference is while both contain information that has been
    % checked by a compiler invocation, values of this type that have been
    % read in from an interface file, while checked by another compiler
    % invocation before being written out, may be corrupted in the filesystem.
    % However, while this danger is always present, we need not take any
    % special steps to guard against it, precisely because no perfect defense
    % is possible.
    %
    % XXX TYPE_REPN Consider whether we can split this type into two,
    % one for the tcrepns that can occur in .int3 files, and one for the
    % tcrepns that can occur in .int/.int2 files.
    %
:- type type_ctor_repn_info
    --->    tcrepn_is_word_aligned_ptr
    ;       tcrepn_is_eqv_to(mer_type)
    ;       tcrepn_is_subtype_of(type_ctor)
    ;       tcrepn_du(du_repn)
    ;       tcrepn_foreign(c_j_cs_repn).

    % A type that has a discriminated union definition in Mercury
    % may also have a definition in each of our foreign languages,
    % If it is an direct_dummy or enum type, that definition may be
    % either a foreign type definition or a foreign enum definition;
    % otherwise, it can only be a foreign type definition.
:- type du_repn
    --->    dur_direct_dummy(direct_dummy_repn)
    ;       dur_enum(enum_repn)
    ;       dur_notag(notag_repn)
    ;       dur_gen_only_functor(gen_du_only_functor_repn)
    ;       dur_gen_more_functors(gen_du_more_functors_repn).

    % When targeting C, many argument packing decisions depend on
    % three properties of the target platform, i.e. on the combination
    % of the target hardware and the target grade:
    %
    % - whether the target is 64 or 32 bit;
    % - whether the grade is an spf (single-precision float) grade; and
    % - whether the grade allows the direct arg optimization.
    %
    % These have eight combinations, but the spf grade component has
    % no effect on argument packing on 64 bit targets (a float is one word
    % either way), so only six are meaningful.
    %
    % If the decision represented by the T parameter happens to be the same
    % on all six platforms, that decision can be represented by c_repns_same.
    %
    % If they are different on 64 vs 32 bit platforms, but are consistent
    % for each word size, then they can be represented by c_repns_64_32.
    %
    % If neither is the case, we can record all six decisions using
    % c_repns_all.
    %
    % XXX We should look for other partitions of the set of six platforms
    % which often have identical decision results; one could be da vs noda.
    %
    % The name of this type is c_repns because argument packing applies
    % only to the low level data representation, which is applicable only
    % when targeting C.
:- type c_repns(T)
    --->    c_repns_same(
                c_repn_same             :: T
            )
    ;       c_repns_64_32(
                c_repn_all_64           :: T,
                c_repn_all_32           :: T
            )
    ;       c_repns_all(
                c_repn_64_nospf_noda    :: T,
                c_repn_64_nospf_da      :: T,
                % c_repn_64_spf_noda    :: T,   % not needed; see above
                % c_repn_64_spf_da      :: T,   % not needed; see above
                c_repn_32_nospf_noda    :: T,
                c_repn_32_nospf_da      :: T,
                c_repn_32_spf_noda      :: T,
                c_repn_32_spf_da        :: T
            ).

%---------------------%

:- type direct_dummy_repn
    --->    direct_dummy_repn(
                % The type is a direct dummy type that satisfies the
                % requirements of du_type_is_dummy.

                % The name of the one functor in the type, which must be
                % arity 0. Its representation will be dummy_tag.
                dummy_functor_name      :: string,

                % Any foreign type or foreign enum definitions for the type.
                dummy_foreign           :: c_j_cs_enum_repn
            ).

%---------------------%

:- type enum_repn
    --->    enum_repn(
                % The type is an enum type that satisfies the requirements
                % of non_sub_du_type_is_enum.

                % The list of the functor names (all arity 0). We store
                % the first two separately to enforce the structural invariant
                % that an enum must have at least two functors.
                %
                % The representation of functor #N in Mercury will be
                % int_tag(int_tag_int(N)), with counting starting at 0.
                %
                % We do not care about the 32 vs 64 bit distinction here,
                % because the definition of an enum type with more than 2^32
                % function symbols will cause a compiler to run out of memory
                % for a *very* long time to come.
                enum_functor1           :: string,
                enum_functor2           :: string,
                enum_functors3plus      :: list(string),

                % Any foreign type or foreign enum definitions for the type.
                enum_foreign            :: c_j_cs_enum_repn
            ).

%---------------------%

:- type notag_repn
    --->    notag_repn(
                % The name of the one functor in the type, which must be
                % arity 1. Its representation will be no_tag.
                % The representation of the argument be *recorded*
                % as a full word at offset 0, but this should never be
                % looked up, since the argument will actually be stored
                % wherever the whole term is stored.
                notag_functor_name      :: string,

                % The type of the one functor's one argument.
                % We record this because without this information,
                % we cannot recognize that a notag type whose argument size
                % is less than one word can itself be stored in less than
                % one word.
                notag_functor_arg_type  :: mer_type,

                % The foreign language definitions for this type, if any.
                notag_foreign           :: c_j_cs_repn
            ).

%---------------------%

:- type gen_du_only_functor_repn
    --->    gen_du_only_functor_repn(
                % The name of the data constructor. The arity is given by
                % the length of list of argument types. The lists of argument
                % representations in all of the nonconstant_repns inside
                % the c_repns must also ave this length.
                only_functor            :: string,

                % The types of the constructor's arguments, after
                % the expansion of both equivalence types and notag types.
                only_deref_arg_types    :: list(mer_type),

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                % The nonconstant_repn cannot be ncr_direct_arg.
                % XXX TYPE_REPN could we encode that invariant in the type?
                only_arg_repns          :: c_repns(only_nonconstant_repn),

                % The foreign language definitions for this type, if any.
                only_foreign            :: c_j_cs_repn
            ).

:- type gen_du_more_functors_repn
    --->    gen_du_more_functors_repn(
                % The first, second and any later functors in the type,
                % in declaration order, i.e. ordered on the functors'
                % original ordinal numbers.
                more_functor1           :: gen_du_functor_repn,
                more_functor2           :: gen_du_functor_repn,
                more_functors3plus      :: list(gen_du_functor_repn),

                % The foreign language definitions for this type, if any.
                more_foreign            :: c_j_cs_repn
            ).

%---------------------%

:- type gen_du_functor_repn
    --->    gen_du_constant_functor_repn(
                % The name of the data constructor. The arity is zero.
                gducf_functor           :: string,

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                gducf_functor_repn      :: c_repns(constant_repn)
            )
    ;       gen_du_nonconstant_functor_repn(
                % The name of the data constructor. The arity is given by
                % the length of list of argument types. The lists of argument
                % representations in all of the nonconstant_repns inside
                % the c_repns must also ave this length.
                gduncf_functor          :: string,

                % The types of the constructor's arguments, after
                % the expansion of both equivalence types and notag types.
                %
                % Logically, the type of each argument belongs with
                % the representation of that argument, but we have to store
                % up to six versions of the representation, and we don't want
                % a duplicate copy of the type next to each version.
                gduncf_deref_arg_types  :: list(mer_type),

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                gduncf_functor_repn     :: c_repns(more_nonconstant_repn)
            ).

:- type constant_repn
    --->    constant_repn(
                % The ptag is 0. The next two fields specify the value
                % and the size of the local secondary tag.
                cr_sectag               :: uint,
                cr_sectag_size          :: lsectag_word_or_size
            ).

:- type only_nonconstant_repn
    --->    oncr_local_cell(only_nonconstant_local_cell_repn)
    ;       oncr_remote_cell(only_nonconstant_remote_cell_repn).

:- type more_nonconstant_repn
    --->    mncr_local_cell(more_nonconstant_local_cell_repn)
    ;       mncr_remote_cell(more_nonconstant_remote_cell_repn)
    ;       mncr_direct_arg(ptag).

:- type only_nonconstant_local_cell_repn
    --->    only_nonconstant_local_cell_repn(
                % The ptag and local sectag are both implicitly 0u.
                onclcr_arg_repns        :: one_or_more(local_arg_repn)
            ).

:- type more_nonconstant_local_cell_repn
    --->    more_nonconstant_local_cell_repn(
                % The ptag is implicitly 0u.
                mnclcr_sectag           :: cell_local_sectag,
                mnclcr_arg_repns        :: one_or_more(local_arg_repn)
            ).

:- type only_nonconstant_remote_cell_repn
    --->    only_nonconstant_remote_cell_repn(
                % The ptag is both implicitly 0u, and there is
                % no remote sectag.
                ncrcr_arg_repns         :: one_or_more(remote_arg_repn)
            ).

:- type more_nonconstant_remote_cell_repn
    --->    more_nonconstant_remote_cell_repn(
                ncrcr_ptag              :: ptag,
                ncrcr_sectag            :: cell_remote_sectag,
                ncrcr_arg_repns         :: one_or_more(remote_arg_repn)
            ).

:- type cell_local_sectag
    --->    cell_local_sectag(
                clss_sectag             :: uint,
                clss_sectag_size        :: uint8
            ).

:- type cell_remote_sectag
    --->    cell_remote_no_sectag
    ;       cell_remote_sectag(
                crss_sectag             :: uint,
                crss_sectag_size        :: rsectag_word_or_size
            ).

:- type lsectag_word_or_size
    --->    lsectag_rest_of_word(uint8)
    ;       lsectag_part_of_word(uint8).

:- type rsectag_word_or_size
    --->    rsectag_full_word
    ;       rsectag_part_of_word(uint8).

:- type local_arg_repn
    --->    local_partial(
                lp_shift                :: uint,
                lp_fill                 :: fill_kind_size
            )
    ;       local_none.

:- type remote_arg_repn
    --->    remote_full(
                rf_arg_only_offset      :: arg_only_offset,
                rf_cell_offset          :: cell_offset
            )
    ;       remote_double(
                rd_arg_only_offset      :: arg_only_offset,
                rd_cell_offset          :: cell_offset,
                rd_kind                 :: double_word_kind
            )
    ;       remote_partial_first(
                rpf_arg_only_offset     :: arg_only_offset,
                rpf_cell_offset         :: cell_offset,
                rpf_shift               :: uint8,
                rpf_fill                :: fill_kind_size
            )
    ;       remote_partial_shifted(
                rps_arg_only_offset     :: arg_only_offset,
                rps_cell_offset         :: cell_offset,
                rps_shift               :: uint8,
                rps_fill                :: fill_kind_size
            )
    ;       remote_none_shifted(
                rns_arg_only_offset     :: arg_only_offset,
                rns_cell_offset         :: cell_offset
            )
    ;       remote_none_nowhere.

:- type fill_kind_size
    --->    fk_enum(uint)   % XXX TYPE_REPN should be uint8
    ;       fk_int8
    ;       fk_int16
    ;       fk_int32
    ;       fk_uint8
    ;       fk_uint16
    ;       fk_uint32
    ;       fk_char21.

    % XXX TYPE_REPN should return uint8
:- func fill_kind_size_num_bits(fill_kind_size) = uint.

%---------------------%

:- type foreign_type_lang_repn
    --->    foreign_type_lang_repn(
                ftlr_lang               :: foreign_language,
                ftlr_foreign_type       :: foreign_type_repn
            ).

:- type foreign_type_repn
    --->    foreign_type_repn(
                % The name of the foreign type that represents values
                % of this Mercury type.
                ftr_foreign_type        :: string,

                % The assertions about this foreign type.
                ftr_assertions          :: foreign_type_assertions
            ).

:- type enum_foreign_repn
    --->    enum_foreign_type(foreign_type_repn)
    ;       enum_foreign_enum(one_or_more(string)).

%---------------------------------------------------------------------------%
%
% Pragmas.
%

:- type item_decl_pragma_info
    --->    decl_pragma_obsolete_pred(decl_pragma_obsolete_pred_info)
    ;       decl_pragma_obsolete_proc(decl_pragma_obsolete_proc_info)
    ;       decl_pragma_format_call(decl_pragma_format_call_info)
    ;       decl_pragma_type_spec(decl_pragma_type_spec_info)
    ;       decl_pragma_oisu(decl_pragma_oisu_info)
    ;       decl_pragma_termination(decl_pragma_termination_info)
    ;       decl_pragma_termination2(decl_pragma_termination2_info)
    ;       decl_pragma_struct_sharing(decl_pragma_struct_sharing_info)
    ;       decl_pragma_struct_reuse(decl_pragma_struct_reuse_info).

:- type item_impl_pragma_info
    --->    impl_pragma_foreign_decl(impl_pragma_foreign_decl_info)
    ;       impl_pragma_foreign_code(impl_pragma_foreign_code_info)
    ;       impl_pragma_fproc_export(impl_pragma_fproc_export_info)
    ;       impl_pragma_external_proc(impl_pragma_external_proc_info)
    ;       impl_pragma_fact_table(impl_pragma_fact_table_info)
    ;       impl_pragma_tabled(impl_pragma_tabled_info)
    ;       impl_pragma_req_tail_rec(impl_pragma_req_tail_rec_info)
    ;       impl_pragma_req_feature_set(impl_pragma_req_feature_set_info).

:- type item_generated_pragma_info
    --->    gen_pragma_unused_args(gen_pragma_unused_args_info)
    ;       gen_pragma_exceptions(gen_pragma_exceptions_info)
    ;       gen_pragma_trailing(gen_pragma_trailing_info)
    ;       gen_pragma_mm_tabling(gen_pragma_mm_tabling_info).

%---------------------------------------------------------------------------%
%
% Decl pragmas.
%

:- type decl_pragma_obsolete_pred_info
    --->    decl_pragma_obsolete_pred_info(
                obspred_obsolete_pred   :: pred_pfu_name_arity,
                obspred_in_favour_of    :: list(sym_name_arity),
                obspred_context         :: prog_context,
                obspred_seq_num         :: item_seq_num
            ).

:- type decl_pragma_obsolete_proc_info
    --->    decl_pragma_obsolete_proc_info(
                obsproc_obsolete_proc   :: proc_pf_name_modes,
                obsproc_in_favour_of    :: list(sym_name_arity),
                obsproc_context         :: prog_context,
                obsproc_seq_num         :: item_seq_num
            ).

:- type decl_pragma_format_call_info
    --->    decl_pragma_format_call_info(
                format_pred             :: pred_pf_name_arity,
                format_values           :: one_or_more(format_string_values),
                format_context          :: prog_context,
                format_seq_num          :: item_seq_num
            ).

:- type decl_pragma_type_spec_info
    --->    decl_pragma_type_spec_info(
                tspec_pfumm             :: pred_func_or_unknown_maybe_modes,

                % The existing predicate name.
                tspec_pred_name         :: sym_name,

                % The name of the module from whose (source or interface) file
                % we read the type_spec pragma. This will always name
                % the module that contain the pragma, because we never put
                % a type_spec pragma into any interface file other than
                % an interface file of the module containing the pragma.
                tspec_module_name       :: module_name,

                % The type substitution (using the variable names
                % from the pred declaration).
                tspec_tsubst            :: type_subst,

                % The varset of the term containing the pragma, coerced
                % to being a tvarset (since no part of the pragma except
                % the type substitution may contain variables).
                %
                % All variables in this tvarset have to have explicit names.
                % If the original pragma contains anonymous variables, the
                % code constructing this decl_pragma_type_spec will give
                % those variable names.
                %
                % The reason for this requirement is that the process
                % of writing out an anonymous variable and reading it back in
                % will produce a non-anonymous variable. Since the names
                % (if any) of the variables in tspec_tsubst are an input
                % to the code that constructs the name of the type-specialized
                % predicate, we would get a discrepancy between the predicate
                % name constructed by compiler invocations that know the
                % variable as unnamed (this will be the invocation that
                % compiles the module containing the type_spec pragma,
                % which constructs the code of the type specialized predicate),
                % and compiler invocations that know that variable as named
                % (this will be all the invocations that read the original
                % module's .int file, which will be constructing many of
                % the *calls* to the type specialized predicate). The result
                % will be calls to the type specialized predicate that refer
                % to it by the wrong name, leading to link errors.
                %
                % By giving all anonymous variables in the type_spec pragma
                % in the original source file as soon as we have parsed it,
                % and then always using the resulting names, we avoid this
                % problem.
                tspec_tvarset           :: tvarset,

                % The equivalence types used.
                tspec_items             :: set(recomp_item_id),

                tspec_context           :: prog_context,
                tspec_seq_num           :: item_seq_num
            ).

:- type decl_pragma_oisu_info
    --->    decl_pragma_oisu_info(
                oisu_type_ctor          :: type_ctor,
                oisu_creator_preds      :: list(pred_pf_name_arity),
                oisu_transformer_preds  :: list(pred_pf_name_arity),
                oisu_destroyer_preds    :: list(pred_pf_name_arity),
                oisu_context            :: prog_context,
                oisu_seq_num            :: item_seq_num
            ).

% The termination/termination2 pragmas record information
% about a predicate's or function's termination properties for our
% two different termination analyzers. Even though they are usually
% compiler generated, they are decl pragmas, not gen pragmas, because
% we allow users to include them in Mercury source programs, to tell
% the analyzers some things that they cannot figure out for themselves,
% such as the termination properties of foreign language code in
% foreign_procs.

:- type decl_pragma_termination_info
    --->    decl_pragma_termination_info(
                % The modes represent the declared argmodes of the procedure,
                % unless there are no declared argmodes, in which case
                % we use the inferred argmodes.
                terminfo_pred_id        :: proc_pf_name_modes,
                terminfo_args           :: maybe(pragma_arg_size_info),
                terminfo_term           :: maybe(pragma_termination_info),
                terminfo_context        :: prog_context,
                terminfo_seq_num        :: item_seq_num
            ).

:- type decl_pragma_termination2_info
    --->    decl_pragma_termination2_info(
                terminfo2_pred_id       :: proc_pf_name_modes,
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info),
                terminfo2_context       :: prog_context,
                terminfo2_seq_num       :: item_seq_num
            ).

% The sharing/reuse pragmas record information about a predicate's or
% function's properties that are relevant for compile-time garbage
% collection (ctgx). Even though they are usually compiler generated,
% they are decl pragmas, not gen pragmas, because we allow users
% to include them in Mercury source programs, to tell the compiler some things
% that it cannot figure out for itself, such as the ctgc properties
% of foreign language code in foreign_procs.

:- type decl_pragma_struct_sharing_info
    --->    decl_pragma_struct_sharing_info(
                % After structure sharing analysis, the compiler generates
                % structure sharing pragmas to be stored in and read from
                % optimization interface files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                sharing_pred_id         :: proc_pf_name_modes,
                sharing_headvars        :: list(prog_var),
                sharing_headvar_types   :: list(mer_type),

                % The prog_varset and tvarset are meaningful only when
                % writing out this pragma; add_pragma.m ignores both varsets.
                sharing_varset          :: prog_varset,
                sharing_tvarset         :: tvarset,

                % As of 2019 10 29, and probably long before then,
                % the compiler *always* fills this slot with `yes(...)'.
                % A `no' would mean that the relevant information is not
                % available, but in that case, we simply do not write out
                % this pragma.
                sharing_description     :: maybe(structure_sharing_domain),

                sharing_context         :: prog_context,
                sharing_seq_num         :: item_seq_num
            ).

:- type decl_pragma_struct_reuse_info
    --->    decl_pragma_struct_reuse_info(
                % After reuse analysis, the compiler generates structure reuse
                % pragmas to be stored in and read from optimization interface
                % files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                %
                % The last sym_name (reuse_optimised_name) stores the name
                % of the optimised version of the exported predicate.
                % XXX As of 2019 10 29, the word "reuse_optimised_name"
                % appears nowhere in the compiler apart from this comment.
                reuse_pred_id           :: proc_pf_name_modes,
                reuse_headvars          :: list(prog_var),
                reuse_headvar_types     :: list(mer_type),

                % The prog_varset and tvarset are meaningful only when
                % writing out this pragma; add_pragma.m ignores both varsets.
                reuse_varset            :: prog_varset,
                reuse_tvarset           :: tvarset,

                % As of 2019 10 29, and probably long before then,
                % the compiler *always* fills this slot with `yes(...)'.
                % A `no' would mean that the relevant information is not
                % available, but in that case, we simply do not write out
                % this pragma.
                reuse_description       :: maybe(structure_reuse_domain),

                reuse_context           :: prog_context,
                reuse_seq_num           :: item_seq_num
            ).

:- type item_decl_marker_info
    --->    item_decl_marker_info(
                dm_marker_kind          :: decl_pragma_marker_kind,
                dm_pred_spec            :: pred_pfu_name_arity,
                dm_context              :: prog_context,
                dm_seq_num              :: item_seq_num
            ).

:- type item_decl_marker_info_opt =< item_decl_marker_info
    --->    item_decl_marker_info(
                dm_marker_kind          :: decl_pragma_marker_kind_opt,
                dm_pred_spec            :: pred_pfu_name_arity_pf,
                dm_context              :: prog_context,
                dm_seq_num              :: item_seq_num
            ).

    % XXX The "terminates" and "does_not_terminate" markers are assertions
    % about the behavior of a given predicate that the compiler may be able
    % to exploit when compiling other modules. The "check_termination" marker
    % is not like that: it is a directive that is useful only while
    % the compiler is working on the module in which it occurs. We should
    % therefore consider making this an *impl* marker, which would entail
    % allowing the "check_termination" pragma to occur only in implementation
    % sections, even when the predicate/function they name is exported.
:- type decl_pragma_marker_kind
    --->    dpmk_terminates
    ;       dpmk_does_not_terminate
    ;       dpmk_check_termination.

:- type decl_pragma_marker_kind_opt =< decl_pragma_marker_kind
    --->    dpmk_terminates
    ;       dpmk_does_not_terminate.

%---------------------------------------------------------------------------%
%
% Impl pragmas.
%

:- type impl_pragma_foreign_decl_info
    --->    impl_pragma_foreign_decl_info(
                % A foreign language declaration, such as C header code.
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: foreign_literal_or_include,
                decl_context            :: prog_context,
                decl_seq_num            :: item_seq_num
            ).

:- type impl_pragma_foreign_code_info
    --->    impl_pragma_foreign_code_info(
                code_lang               :: foreign_language,
                code_code               :: foreign_literal_or_include,
                code_context            :: prog_context,
                code_seq_num            :: item_seq_num
            ).

:- type impl_pragma_fproc_export_info
    --->    impl_pragma_fproc_export_info(
                exp_maybe_attrs         :: item_maybe_attrs,

                exp_language            :: foreign_language,
                % Predname, Predicate/function, Modes, foreign function name.
                exp_pred_id             :: proc_pf_name_modes,
                exp_foreign_name        :: string,

                % Specified the names of any variables in the modes above.
                % Used for generating error messages about foreign_export
                % pragmas for undeclared modes.
                exp_varset              :: prog_varset,

                exp_context             :: prog_context,
                exp_seq_num             :: item_seq_num
            ).

:- type impl_pragma_external_proc_info
    --->    impl_pragma_external_proc_info(
                % The specified procedure(s) is/are implemented outside
                % of Mercury code, for the named backend if there is one,
                % or if there isn't a named backend, then for all backends.
                external_name           :: pred_pf_name_arity,
                external_maybe_backend  :: maybe(backend),
                external_context        :: prog_context,
                external_seq_num        :: item_seq_num
            ).

:- type impl_pragma_fact_table_info
    --->    impl_pragma_fact_table_info(
                % Predname and Arity, Fact file name.
                fact_table_pred         :: pred_pfu_name_arity,
                fact_table_filename     :: string,
                fact_table_context      :: prog_context,
                fact_table_seq_num      :: item_seq_num
            ).

:- type impl_pragma_tabled_info
    --->    impl_pragma_tabled_info(
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
                tabled_method           :: tabled_eval_method,
                tabled_name             :: pred_or_proc_pfumm_name,
                tabled_attributes       :: maybe(table_attributes),
                tabled_context          :: prog_context,
                tabled_seq_num          :: item_seq_num
            ).

:- type impl_pragma_req_tail_rec_info
    --->    impl_pragma_req_tail_rec_info(
                rtr_proc_id             :: pred_or_proc_pfumm_name,
                rtr_require_tailrec     :: require_tail_recursion,
                % This parameter only makes sense when options contains
                % either rtro_mutual_rec_only or rtro_all_recursion.
                % TODO, currently unused, may be used later to implement one
                % of Zoltan's suggestions here:
                % http://www.mercurylang.org/list-archives/developers/
                %   2015-November/016482.html
                % rtr_maybe_scc           :: maybe(list(
                %                             pred_or_proc_pfumm_name))
                rtr_context             :: prog_context,
                rtr_seq_num             :: item_seq_num
            ).

:- type impl_pragma_req_feature_set_info
    --->    impl_pragma_req_feature_set_info(
                rfs_feature_set         :: set(required_feature),
                rfs_context             :: prog_context,
                rfs_seq_num             :: item_seq_num
            ).

:- type item_impl_marker_info
    --->    item_impl_marker_info(
                im_marker_kind          :: impl_pragma_marker_kind,
                im_pred_spec            :: pred_pfu_name_arity,
                im_context              :: prog_context,
                im_seq_num              :: item_seq_num
            ).

:- type item_impl_marker_info_opt =< item_impl_marker_info
    --->    item_impl_marker_info(
                im_marker_kind          :: impl_pragma_marker_kind_opt,
                im_pred_spec            :: pred_pfu_name_arity_pf,
                im_context              :: prog_context,
                im_seq_num              :: item_seq_num
            ).

:- type impl_pragma_marker_kind
    --->    ipmk_inline
    ;       ipmk_no_inline
    ;       ipmk_consider_used
    ;       ipmk_mode_check_clauses
    ;       ipmk_no_detism_warning
    ;       ipmk_promise_pure
    ;       ipmk_promise_semipure
    ;       ipmk_promise_eqv_clauses.

    % These are the kinds of impl markers that we put into .opt files.
:- type impl_pragma_marker_kind_opt =< impl_pragma_marker_kind
    --->    ipmk_inline
    ;       ipmk_no_inline
    ;       ipmk_mode_check_clauses
    ;       ipmk_promise_pure
    ;       ipmk_promise_semipure
    ;       ipmk_promise_eqv_clauses.

%---------------------------------------------------------------------------%
%
% Generated pragmas.
%

:- type gen_pragma_unused_args_info
    --->    gen_pragma_unused_args_info(
                % This pragma Should only appear in .opt files.
                unused_proc_id          :: proc_pf_name_arity_mn,

                % The argument positions of the unused arguments.
                % Used for intermodule unused argument removal.
                unused_args             :: list(int),

                unused_context          :: prog_context,
                unused_seq_num          :: item_seq_num
            ).

:- type gen_pragma_exceptions_info
    --->    gen_pragma_exceptions_info(
                % This pragma should only appear in `.opt' and
                % `.trans_opt' files.
                exceptions_proc_id      :: proc_pf_name_arity_mn,
                exceptions_status       :: exception_status,

                exceptions_context      :: prog_context,
                exceptions_seq_num      :: item_seq_num
            ).

:- type gen_pragma_trailing_info
    --->    gen_pragma_trailing_info(
                % This pragma should only appear in `.trans_opt' files.
                trailing_proc_id        :: proc_pf_name_arity_mn,
                trailing_status         :: trailing_status,

                trailing_context        :: prog_context,
                trailing_seq_num        :: item_seq_num
            ).

:- type gen_pragma_mm_tabling_info
    --->    gen_pragma_mm_tabling_info(
                % This pragma should only appear in `.opt' and
                % `.trans_opt' files.
                mm_tabling_proc_id      :: proc_pf_name_arity_mn,
                mm_tabling_status       :: mm_tabling_status,

                mm_tabling_context      :: prog_context,
                mm_tabling_seq_num      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

    % These types identify predicates, functions and/or procedures in pragmas.

:- type pred_pfu_name_arity
    --->    pred_pfu_name_arity(
                ppfuna_pfu              :: pred_func_or_unknown,
                ppfuna_pred_name        :: sym_name,
                ppfuna_arity            :: user_arity
            ).

:- type pred_pfu_name_arity_pf =< pred_pfu_name_arity
    --->    pred_pfu_name_arity(
                ppfuna_pfu              :: pred_func_or_unknown_pf,
                ppfuna_pred_name        :: sym_name,
                ppfuna_arity            :: user_arity
            ).

:- type proc_pf_name_arity_mn
    --->    proc_pf_name_arity_mn(
                ppfnamn_pf              :: pred_or_func,
                ppfnamn_pred_name       :: sym_name,
                ppfnamn_arity           :: user_arity,
                ppfnamn_mode_num        :: mode_num
            ).

:- type proc_pf_name_modes
    --->    proc_pf_name_modes(
                ppfnm_pf                :: pred_or_func,
                ppfnm_pred_name         :: sym_name,
                ppfnm_arity             :: list(mer_mode)
            ).

:- type pred_or_proc_pfumm_name
    --->    pred_or_proc_pfumm_name(
                ppfummn_pfumm           :: pred_func_or_unknown_maybe_modes,
                ppfummn_pred_name       :: sym_name
            ).

:- type pred_func_or_unknown
    --->    pfu_predicate
    ;       pfu_function
    ;       pfu_unknown.

:- type pred_func_or_unknown_pf =< pred_func_or_unknown
    --->    pfu_predicate
    ;       pfu_function.

:- type pred_func_or_unknown_maybe_modes
    --->    pfumm_predicate(modes_or_arity)
    ;       pfumm_function(modes_or_arity)
    ;       pfumm_unknown(user_arity).

:- type modes_or_arity
    --->    moa_modes(list(mer_mode))
    ;       moa_arity(user_arity).

:- func pfu_to_maybe_pred_or_func(pred_func_or_unknown) = maybe(pred_or_func).
:- func maybe_pred_or_func_to_pfu(maybe(pred_or_func)) = pred_func_or_unknown.

:- pred pfumm_to_maybe_pf_arity_maybe_modes(
    pred_func_or_unknown_maybe_modes::in, maybe(pred_or_func)::out,
    user_arity::out, maybe(list(mer_mode))::out) is det.

%---------------------------------------------------------------------------%
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
    % goal_unify               1360701
    % goal_conj                1316066 when we had a conj_expr for each ","
    % goal_call                1263403
    %
    % goal_true                 135352
    % goal_if_then_else         128052
    % goal_disj                 116547 when we had a disj_expr for each ";"
    % goal_not                    7080
    %
    % goal_fail                   5219
    % goal_pro_purity             1492
    % goal_trace                  1356
    % goal_pro_eqv_solns           913
    % goal_some_state_vars         620 now goal_quant/some/state
    % goal_some                    192 now goal_quant/some/ordinary
    % goal_req_compl_switch        172
    % goal_par_conj                132 when we had a par_conj_expr for each "&"
    % goal_implies                 129
    % goal_all                      78 now goal_quant/all/ordinary
    % goal_req_detism               49
    % goal_try                      35
    % goal_equivalent               18
    % goal_event                    17
    % goal_req_arm_detism           14
    % goal_pro_arbitrary            12
    % goal_pro_eqv_soln_sets         8
    % goal_atomic                    2
    % goal_all_state_vars            0 now goal_quant/all/state

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

    ;       conj_expr(prog_context, goal, list(goal))
            % nonempty plain conjunction
            % NOTE: We could replace this with
            %   conj_expr(prog_context, goal, goal, list(goal))
            % to encode the invariant that
            % - a conjunction has at least one conjunction operator, and
            % - that operator has two argument goals.
            % However, no part of the current compiler can exploit
            % this extra information.
            % NOTE: On the other hand, we could also replace this with
            %   conj_expr(prog_context, list(goal))
            % letting a conj_expr with an empty list of goals take over
            % the role of true_expr. However, that would make the parse tree
            % representation of plain conjunctions differ from the
            % representation of parallel conjunctions. And the most
            % frequent goal that does not now have its own primary tag
            % on 64 bit machines, fail_expr, is infrequent enough that
            % giving it its own primary tag would not materially improve
            % performance, and even if it were frequent enough, it could be
            % folded into disj_exprs in a similar way.

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
    ;       disj_expr(prog_context, goal, goal, list(goal))
            % nonempty disjunction; will contain at least two goals.

    ;       not_expr(prog_context, goal)

    % The other kinds of goals.

    ;       fail_expr(prog_context)
            % empty disjunction

    ;       par_conj_expr(prog_context, goal, list(goal))
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

%---------------------------------------------------------------------------%

:- func get_item_context(item) = prog_context.
:- func get_decl_pragma_context(item_decl_pragma_info) = prog_context.
:- func get_impl_pragma_context(item_impl_pragma_info) = prog_context.
:- func get_gen_pragma_context(item_generated_pragma_info) = prog_context.
:- func get_goal_context(goal) = prog_context.

%---------------------------------------------------------------------------%

:- type contains_foreign_code
    --->    foreign_code_langs_known(set(foreign_language))
    ;       foreign_code_langs_unknown.

:- type contains_foreign_export
    --->    contains_foreign_export
    ;       contains_no_foreign_export.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

:- import_module term.
:- import_module varset.

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
%
% Mutable variables.
%

mutable_var_thread_local(Const) = Local :-
    ( if
        Const = mutable_is_not_constant(_AttachToIO, IsLocal),
        % Const = mutable_is_constant would imply mutable_not_thread_local
        IsLocal = mutable_is_thread_local
    then
        Local = mutable_thread_local
    else
        Local = mutable_not_thread_local
    ).

mutable_thread_local_trailed(Local) = Trail :-
    (
        Local = mutable_is_not_thread_local(Trail)
    ;
        Local = mutable_is_thread_local,
        Trail = mutable_untrailed
    ).

%---------------------------------------------------------------------------%

fill_kind_size_num_bits(FillKindSize) = NumBits :-
    (
        FillKindSize = fk_enum(NumBits)
    ;
        ( FillKindSize = fk_int8
        ; FillKindSize = fk_uint8
        ),
        NumBits = 8u
    ;
        ( FillKindSize = fk_int16
        ; FillKindSize = fk_uint16
        ),
        NumBits = 16u
    ;
        ( FillKindSize = fk_int32
        ; FillKindSize = fk_uint32
        ),
        NumBits = 32u
    ;
        FillKindSize = fk_char21,
        NumBits = 21u
    ).

%---------------------------------------------------------------------------%

pfu_to_maybe_pred_or_func(pfu_predicate) = yes(pf_predicate).
pfu_to_maybe_pred_or_func(pfu_function) = yes(pf_function).
pfu_to_maybe_pred_or_func(pfu_unknown) = no.

maybe_pred_or_func_to_pfu(yes(pf_predicate)) = pfu_predicate.
maybe_pred_or_func_to_pfu(yes(pf_function)) = pfu_function.
maybe_pred_or_func_to_pfu(no) = pfu_unknown.

pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes) :-
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        MaybePredOrFunc = yes(PredOrFunc),
        (
            ModesOrArity = moa_modes(Modes),
            list.length(Modes, NumModes),
            PredFormArity = pred_form_arity(NumModes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MaybeModes = yes(Modes)
        ;
            ModesOrArity = moa_arity(UserArity),
            MaybeModes = no
        )
    ;
        PFUMM = pfumm_unknown(UserArity),
        MaybePredOrFunc = no,
        MaybeModes = no
    ).

%---------------------------------------------------------------------------%

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
        Item = item_foreign_proc(ItemForeignProc),
        Context = ItemForeignProc ^ proc_context
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        Context = ItemForeignEnum ^ fe_context
    ;
        Item = item_foreign_export_enum(ItemForeignExportEnum),
        Context = ItemForeignExportEnum ^ fee_context
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        Context = get_decl_pragma_context(ItemDeclPragma)
    ;
        Item = item_decl_marker(ItemDeclMarker),
        Context = ItemDeclMarker ^ dm_context
    ;
        Item = item_impl_pragma(ItemImplPragma),
        Context = get_impl_pragma_context(ItemImplPragma)
    ;
        Item = item_impl_marker(ItemImplMarker),
        Context = ItemImplMarker ^ im_context
    ;
        Item = item_generated_pragma(ItemGenPragma),
        Context = get_gen_pragma_context(ItemGenPragma)
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

get_decl_pragma_context(DeclPragma) = Context :-
    (
        DeclPragma = decl_pragma_obsolete_pred(ObsPred),
        Context = ObsPred ^ obspred_context
    ;
        DeclPragma = decl_pragma_obsolete_proc(ObsProc),
        Context = ObsProc ^ obsproc_context
    ;
        DeclPragma = decl_pragma_format_call(FormatCall),
        Context = FormatCall ^ format_context
    ;
        DeclPragma = decl_pragma_type_spec(TypeSpec),
        Context = TypeSpec ^ tspec_context
    ;
        DeclPragma = decl_pragma_oisu(OISU),
        Context = OISU ^ oisu_context
    ;
        DeclPragma = decl_pragma_termination(Term),
        Context = Term ^ terminfo_context
    ;
        DeclPragma = decl_pragma_termination2(Term2),
        Context = Term2 ^ terminfo2_context
    ;
        DeclPragma = decl_pragma_struct_sharing(Sharing),
        Context = Sharing ^ sharing_context
    ;
        DeclPragma = decl_pragma_struct_reuse(Reuse),
        Context = Reuse ^ reuse_context
    ).

get_impl_pragma_context(ImplPragma) = Context :-
    (
        ImplPragma = impl_pragma_foreign_decl(ForeignDecl),
        Context = ForeignDecl ^ decl_context
    ;
        ImplPragma = impl_pragma_foreign_code(ForeignCode),
        Context = ForeignCode ^ code_context
    ;
        ImplPragma = impl_pragma_fproc_export(Export),
        Context = Export ^ exp_context
    ;
        ImplPragma = impl_pragma_external_proc(ExternalProc),
        Context = ExternalProc ^ external_context
    ;
        ImplPragma = impl_pragma_fact_table(FactTable),
        Context = FactTable ^ fact_table_context
    ;
        ImplPragma = impl_pragma_tabled(Tabled),
        Context = Tabled ^ tabled_context
    ;
        ImplPragma = impl_pragma_req_tail_rec(TailRec),
        Context = TailRec ^ rtr_context
    ;
        ImplPragma = impl_pragma_req_feature_set(FeatureSet),
        Context = FeatureSet ^ rfs_context
    ).

get_gen_pragma_context(GenPragma) = Context :-
    (
        GenPragma = gen_pragma_unused_args(UnusedArgs),
        Context = UnusedArgs ^ unused_context
    ;
        GenPragma = gen_pragma_exceptions(Excps),
        Context = Excps ^ exceptions_context
    ;
        GenPragma = gen_pragma_trailing(Trailing),
        Context = Trailing ^ trailing_context
    ;
        GenPragma = gen_pragma_mm_tabling(MMTabling),
        Context = MMTabling ^ mm_tabling_context
    ).

get_goal_context(Goal) = Context :-
    ( Goal = conj_expr(Context, _, _)
    ; Goal = par_conj_expr(Context, _, _)
    ; Goal = true_expr(Context)
    ; Goal = disj_expr(Context, _, _, _)
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

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_item.
%---------------------------------------------------------------------------%
