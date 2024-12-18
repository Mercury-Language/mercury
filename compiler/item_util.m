%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: item_util.m.
%
% This module contains utility predicates for dealing with items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.item_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.

:- import_module list.
:- import_module map.
:- import_module one_or_more.
:- import_module set.

%---------------------------------------------------------------------------%

    % classify_include_modules(IntIncludes, ImpIncludes, InclMap, !Specs):
    %
    % Record the inclusion of each submodule in the section and at the context
    % where it happens in InclMap.
    %
    % If a submodule is included more than once within the same section,
    % keep only the first inclusion. If a submodule is included in both
    % the interface and the implementation section, keep only the one in the
    % interface section. In both cases, generate an error message for all
    % the other inclusions.
    %
:- pred classify_include_modules(
    list(item_include)::in, list(item_include)::in, include_module_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred include_map_to_int_imp_modules(include_module_map::in,
    set(module_name)::out, set(module_name)::out) is det.

:- pred add_only_int_include(module_name::in, include_module_info::in,
    int_include_module_map::in, int_include_module_map::out) is det.

%---------------------------------------------------------------------------%

:- pred get_imports_uses(module_name::in, section_import_and_or_use::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

:- pred get_uses(module_name::in, section_use::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

    % accumulate_imports_uses_maps(Avails, !ImportMap, !UseMap):
    %
    % Add the imports in Avails to !ImportMap, and
    % add the uses in Avails to !UseMap.
    %
:- pred accumulate_imports_uses_maps(list(item_avail)::in,
    module_names_contexts::in, module_names_contexts::out,
    module_names_contexts::in, module_names_contexts::out) is det.

    % classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
    %   ImportUseMap, !Specs) :-
    %
    % The input is item_avails showing which modules are imported and/or used
    % where in the interface and implementation sections. This input
    % which may contain duplicates of several kinds:
    %
    % - a module may be imported more than once in the same section
    % - a module may be used more than once in the same section
    % - a module may be both imported and used in the same section
    % - a module may be both imported and used in different sections
    %   *other than* the one permitted combination, which is
    %   "used in the interface, imported in the implementation".
    %
    % Report each occurrence of any of these forms of duplication,
    % and we also report self-imports/self-ises, and imports/uses
    % of ancestor modules.
    %
    % We return a structured representation of the non-duplicate entries
    % in ImportUseMap.
    %
:- pred classify_int_imp_import_use_modules(module_name::in,
    list(item_avail)::in, list(item_avail)::in,
    section_import_and_or_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % classify_int_imp_use_modules(ModuleName,
    %   IntUseContextsMap, ImpUseContextsMap, !:UseMap, !Specs) :-
    %
    % Do the same job as classify_int_imp_import_use_modules above,
    % but for .int and .int2 files, not source files or .int0 files.
    % This different use case leads to the following differences:
    %
    % - We process only use_module declarations, since these files
    %   do not allow import_module declarations. We therefore return
    %   a section_use_map instead of a section_import_and_or_use_map.
    %   The former is a subtype of the latter that is restricted to
    %   record information only about uses.
    %
    % - We do not return {int,imp}_{import,use}_maps, since the
    %   parse_trees of .int and .int2 files do not need them.
    %
    % - Since these files are automatically generated, any issues with them
    %   are the fault of either the compiler invocation that generated them,
    %   or of the user for tampering with the output of the compiler.
    %   In both of those cases, generating an error seems preferable to
    %   generating a warning.
    %
:- pred classify_int_imp_use_modules(module_name::in,
    module_names_contexts::in, module_names_contexts::in,
    section_use_map::out, list(error_spec)::in, list(error_spec)::out) is det.

:- pred import_and_or_use_map_section_to_maybe_implicit(
    section_import_and_or_use_map::in, import_and_or_use_map::out) is det.

:- type maybe_include_implicit
    --->    do_include_implicit
    ;       do_not_include_implicit.

:- pred section_use_map_to_item_avails(section_use_map::in,
    list(item_avail)::out, list(item_avail)::out) is det.
:- pred section_import_and_or_use_map_to_item_avails(
    section_import_and_or_use_map::in,
    list(item_avail)::out, list(item_avail)::out) is det.
:- pred import_and_or_use_map_to_item_avails(maybe_include_implicit::in,
    import_and_or_use_map::in,
    list(item_avail)::out, list(item_avail)::out) is det.

:- type module_name_context == map(module_name, prog_context).

    % Maps from module names to the imports or uses in the named section.
    % The code creating these maps will have detected and diagnosed
    % any duplicate entries of the same kind of declaration for
    % the same module in the same section. However, unlike
    % import_and_or_use_maps, which summarize the information in these maps,
    % these maps may contain redundant entries as long as they are all
    % in *different* maps (such as the module name A occurring in both
    % the int_import_context_map and the int_use_context_map of module B).
    %
    % It is an error if a module has an entry in more than one of these maps,
    % with the sole exception being the use_module-in-interface and
    % import_module-in-implementation combination. This is an exception
    % because each grants a permission that the other does not.
:- type int_import_context_map
    --->    int_import_context_map(module_name_context).
:- type int_use_context_map
    --->    int_use_context_map(module_name_context).
:- type imp_import_context_map
    --->    imp_import_context_map(module_name_context).
:- type imp_use_context_map
    --->    imp_use_context_map(module_name_context).

:- pred import_and_or_use_map_to_explicit_int_imp_import_use_maps(
    import_and_or_use_map::in, section_import_and_or_use_map::out,
    int_import_context_map::out, int_use_context_map::out,
    imp_import_context_map::out, imp_use_context_map::out) is det.

    % import_and_or_use_map_to_module_name_contexts(ImportUseMap,
    %   IntImports, IntUses, ImpImports, ImpUses, IntUsesImpImports).
    %
:- pred import_and_or_use_map_to_module_name_contexts(
    import_and_or_use_map::in,
    module_name_context::out, module_name_context::out,
    module_name_context::out, module_name_context::out,
    module_name_context::out) is det.

    % Convert a map from module names to their contexts back
    % to a list of item_avails.
    %
:- func use_map_to_item_avails(module_names_contexts) = list(item_avail).

:- pred acc_avails_with_contexts(import_or_use::in, module_name::in,
    one_or_more(prog_context)::in,
    list(item_avail)::in, list(item_avail)::out) is det.

:- pred avail_imports_uses(list(item_avail)::in,
    list(avail_import_info)::out, list(avail_use_info)::out) is det.

    % Return "import_module" or "use_module", depending on the argument.
    %
:- func import_or_use_decl_name(import_or_use) = string.

:- pred avail_is_import(item_avail::in, avail_import_info::out) is semidet.
:- pred avail_is_use(item_avail::in, avail_use_info::out) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on foreign_import_module (fim) items.
%

:- func fim_item_to_spec(item_fim) = fim_spec.
:- func fim_spec_to_item(fim_spec) = item_fim.
:- func fim_module_lang_to_spec(module_name, foreign_language) = fim_spec.
:- func fim_module_lang_to_item(module_name, foreign_language) = item_fim.

:- pred add_implicit_fim_for_module(module_name::in, foreign_language::in,
    map(fim_spec, prog_context)::in, map(fim_spec, prog_context)::out) is det.

    % For what languages could this item need the import of foreign modules.
    %
:- func item_needs_foreign_imports(item) = list(foreign_language).

:- pred acc_needed_self_fim_langs_for_type_defn(item_type_defn_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.
:- pred acc_needed_self_fim_langs_for_foreign_proc(item_foreign_proc_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.
:- pred acc_needed_self_fim_langs_for_foreign_enum(item_foreign_enum_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.
:- pred acc_needed_self_fim_langs_for_impl_pragma(item_impl_pragma_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

%---------------------------------------------------------------------------%
%
% Describing items for error messages.
%

    % Return a description of one item, ...
    %
:- func item_desc_pieces(item) = list(format_piece).

    % ... and its plural version.
    %
:- func items_desc_pieces(item) = list(format_piece).

:- func decl_pragma_desc_pieces(item_decl_pragma_info) = list(format_piece).
:- func decl_marker_desc_pieces(item_decl_marker_info) = list(format_piece).
:- func impl_pragma_desc_pieces(item_impl_pragma_info) = list(format_piece).
:- func impl_marker_desc_pieces(item_impl_marker_info) = list(format_piece).
:- func gen_pragma_desc_pieces(item_generated_pragma_info)
    = list(format_piece).

%---------------------------------------------------------------------------%
%
% Projection operations.
%

:- func parse_tree_module_src_project_name(parse_tree_module_src)
    = module_name.

:- func item_include_module_name(item_include) = module_name.

:- func get_avail_context(item_avail) = prog_context.
:- func get_import_context(avail_import_info) = prog_context.
:- func get_use_context(avail_use_info) = prog_context.

:- func get_avail_module_name(item_avail) = module_name.
:- func get_import_module_name(avail_import_info) = module_name.
:- func get_use_module_name(avail_use_info) = module_name.

%---------------------------------------------------------------------------%
%
% Given a checked map for types, insts or modes, return the interface items
% and the implementation items they represent. (This will be a consistent
% subset of the set of the relevant kind of items in the module's code.)
% For types, return the set of foreign_enum items as well; these are all
% in the implementation section.
%

:- pred type_ctor_checked_map_get_src_defns(type_ctor_checked_map::in,
    list(item_type_defn_info)::out, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::out) is det.

:- pred inst_ctor_checked_map_get_src_defns(inst_ctor_checked_map::in,
    list(item_inst_defn_info)::out, list(item_inst_defn_info)::out) is det.

:- pred mode_ctor_checked_map_get_src_defns(mode_ctor_checked_map::in,
    list(item_mode_defn_info)::out, list(item_mode_defn_info)::out) is det.

%---------------------------------------------------------------------------%
%
% Wrapping up pieces of information with a dummy context
% and a dummy sequence number.
%

:- func wrap_include(module_name) = item_include.
:- func wrap_import_avail(module_name) = item_avail.
:- func wrap_use_avail(module_name) = item_avail.
:- func wrap_import(module_name) = avail_import_info.
:- func wrap_use(module_name) = avail_use_info.

:- func wrap_avail_import(avail_import_info) = item_avail.
:- func wrap_avail_use(avail_use_info) = item_avail.

:- func wrap_type_defn_item(item_type_defn_info) = item.
:- func wrap_inst_defn_item(item_inst_defn_info) = item.
:- func wrap_mode_defn_item(item_mode_defn_info) = item.
:- func wrap_typeclass_item(item_typeclass_info) = item.
:- func wrap_instance_item(item_instance_info) = item.
:- func wrap_pred_decl_item(item_pred_decl_info) = item.
:- func wrap_mode_decl_item(item_mode_decl_info) = item.
:- func wrap_foreign_enum_item(item_foreign_enum_info) = item.
:- func wrap_foreign_export_enum_item(item_foreign_export_enum_info) = item.
:- func wrap_clause(item_clause_info) = item.
:- func wrap_decl_pragma_item(item_decl_pragma_info) = item.
:- func wrap_impl_pragma_item(item_impl_pragma_info) = item.
:- func wrap_generated_pragma_item(item_generated_pragma_info) = item.
:- func wrap_promise_item(item_promise_info) = item.
:- func wrap_initialise_item(item_initialise_info) = item.
:- func wrap_finalise_item(item_finalise_info) = item.
:- func wrap_mutable_item(item_mutable_info) = item.
:- func wrap_type_repn_item(item_type_repn_info) = item.

%---------------------------------------------------------------------------%
%
% Converting specific forms of type definitions to the generic form.
%

:- func wrap_abstract_type_defn(item_type_defn_info_abstract)
    = item_type_defn_info.
:- func wrap_solver_type_defn(item_type_defn_info_solver)
    = item_type_defn_info.
:- func wrap_eqv_type_defn(item_type_defn_info_eqv)
    = item_type_defn_info.
:- func wrap_du_type_defn(item_type_defn_info_du)
    = item_type_defn_info.
:- func wrap_sub_type_defn(item_type_defn_info_sub)
    = item_type_defn_info.
:- func wrap_foreign_type_defn(item_type_defn_info_foreign)
    = item_type_defn_info.

:- func wrap_abstract_inst_defn(item_inst_defn_info_abstract)
    = item_inst_defn_info.
:- func wrap_eqv_inst_defn(item_inst_defn_info_eqv)
    = item_inst_defn_info.

:- func wrap_abstract_mode_defn(item_mode_defn_info_abstract)
    = item_mode_defn_info.
:- func wrap_eqv_mode_defn(item_mode_defn_info_eqv)
    = item_mode_defn_info.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module maybe.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

classify_include_modules(IntIncludes, ImpIncludes,
        !:InclMap, !Specs) :-
    map.init(!:InclMap),
    list.foldl2(classify_include_module(ms_interface), IntIncludes,
        !InclMap, !Specs),
    list.foldl2(classify_include_module(ms_implementation), ImpIncludes,
        !InclMap, !Specs).

:- pred classify_include_module(module_section::in, item_include::in,
    include_module_map::in, include_module_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

classify_include_module(Section, ItemInclude, !InclMap, !Specs) :-
    ItemInclude = item_include(ModuleName, Context, _SeqNum),
    ( if map.search(!.InclMap, ModuleName, PrevEntry) then
        PrevEntry = include_module_info(_PrevSection, PrevContext),
        report_duplicate_include(ModuleName, PrevContext, Context, !Specs)
    else
        Entry = include_module_info(Section, Context),
        map.det_insert(ModuleName, Entry, !InclMap)
    ).

:- pred report_duplicate_include(module_name::in,
    prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_include(ModuleName, PrevContext, Context, !Specs) :-
    MainPieces = [words("Error:")] ++
        color_as_incorrect([words("duplicate"),
            decl("include"), words("declaration")]) ++
        [words("for submodule")] ++
        color_as_subject([qual_sym_name(ModuleName), suffix(".")]) ++
        [nl],
    MainMsg = msg(Context, MainPieces),
    PrevPieces = [words("The previous"),
        decl("include"), words("declaration was here."), nl],
    PrevMsg = msg(PrevContext, PrevPieces),
    Spec = error_spec($pred, severity_error, phase_pt2h, [MainMsg, PrevMsg]),
    !:Specs = [Spec | !.Specs].

%---------------------%

include_map_to_int_imp_modules(IncludeMap, IntModules, ImpModules) :-
    map.foldl2(include_map_to_int_imp_modules_acc, IncludeMap,
        set.init, IntModules, set.init, ImpModules).

:- pred include_map_to_int_imp_modules_acc(
    module_name::in, include_module_info::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

include_map_to_int_imp_modules_acc(ModuleName, InclInfo,
        !IntModules, !ImpModules) :-
    InclInfo = include_module_info(Section, _Context),
    (
        Section = ms_interface,
        set.insert(ModuleName, !IntModules)
    ;
        Section = ms_implementation,
        set.insert(ModuleName, !ImpModules)
    ).

%---------------------%

add_only_int_include(ModuleName, InclInfo, !IntInclMap) :-
    InclInfo = include_module_info(Section, Context),
    (
        Section = ms_interface,
        IntInclInfo = include_module_info(ms_interface, Context),
        map.det_insert(ModuleName, IntInclInfo, !IntInclMap)
    ;
        Section = ms_implementation
    ).

%---------------------------------------------------------------------------%

get_imports_uses(ModuleName, ImportAndOrUse,
        !IntImports, !ImpImports, !IntUses, !ImpUses) :-
    (
        ImportAndOrUse = int_import(_Context),
        set.insert(ModuleName, !IntImports)
    ;
        ImportAndOrUse = int_use(_Context),
        set.insert(ModuleName, !IntUses)
    ;
        ImportAndOrUse = imp_import(_Context),
        set.insert(ModuleName, !ImpImports)
    ;
        ImportAndOrUse = imp_use(_Context),
        set.insert(ModuleName, !ImpUses)
    ;
        ImportAndOrUse = int_use_imp_import(_IntContext, _ImpContext),
        set.insert(ModuleName, !IntUses),
        set.insert(ModuleName, !ImpImports)
    ).

get_uses(ModuleName, Use, !IntUses, !ImpUses) :-
    (
        Use = int_use(_Context),
        set.insert(ModuleName, !IntUses)
    ;
        Use = imp_use(_Context),
        set.insert(ModuleName, !ImpUses)
    ).

%---------------------%

    % get_imports_uses_maps(Avails, ImportMap, UseMap):
    %
    % Given the avails of a raw compilation unit, return the set of modules
    % imported and used in those sections, mapped to the list of locations
    % of those imports and uses.
    %
:- pred get_imports_uses_maps(list(item_avail)::in,
    module_names_contexts::out, module_names_contexts::out) is det.

get_imports_uses_maps(Avails, ImportMap, UseMap) :-
    accumulate_imports_uses_maps(Avails,
        one_or_more_map.init, ImportMap, one_or_more_map.init, UseMap).

accumulate_imports_uses_maps([], !ImportMap, !UseMap).
accumulate_imports_uses_maps([Avail | Avails], !ImportMap, !UseMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _)),
        one_or_more_map.add(ModuleName, Context, !ImportMap)
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _)),
        one_or_more_map.add(ModuleName, Context, !UseMap)
    ),
    accumulate_imports_uses_maps(Avails, !ImportMap, !UseMap).

%---------------------%

classify_int_imp_import_use_modules(ModuleName, IntAvails, ImpAvails,
        !:ImportUseMap, !Specs) :-
    get_imports_uses_maps(IntAvails, IntImportContextsMap, IntUseContextsMap),
    get_imports_uses_maps(ImpAvails, ImpImportContextsMap, ImpUseContextsMap),
    map.map_foldl(
        report_any_duplicate_avail_contexts("interface", "import_module"),
        IntImportContextsMap, IntImportMap, !Specs),
    map.map_foldl(
        report_any_duplicate_avail_contexts("interface", "use_module"),
        IntUseContextsMap, IntUseMap, !Specs),
    map.map_foldl(
        report_any_duplicate_avail_contexts("implementation", "import_module"),
        ImpImportContextsMap, ImpImportMap, !Specs),
    map.map_foldl(
        report_any_duplicate_avail_contexts("implementation", "use_module"),
        ImpUseContextsMap, ImpUseMap, !Specs),

    map.init(!:ImportUseMap),
    map.foldl(record_int_import,  IntImportMap, !ImportUseMap),
    map.foldl2(record_int_use,    IntUseMap,    !ImportUseMap, !Specs),
    map.foldl2(record_imp_import, ImpImportMap, !ImportUseMap, !Specs),
    map.foldl2(record_imp_use,    ImpUseMap,    !ImportUseMap, !Specs),

    warn_if_avail_for_self(ModuleName, !ImportUseMap, !Specs),
    list.foldl2(warn_if_avail_for_ancestor(ModuleName),
        get_ancestors(ModuleName), !ImportUseMap, !Specs).

classify_int_imp_use_modules(ModuleName, IntUseContextsMap, ImpUseContextsMap,
        !:UseMap, !Specs) :-
    map.map_foldl(
        report_any_duplicate_avail_contexts("interface", "use_module"),
        IntUseContextsMap, IntUseMap, !Specs),
    map.map_foldl(
        report_any_duplicate_avail_contexts("implementation", "use_module"),
        ImpUseContextsMap, ImpUseMap, !Specs),

    map.init(!:UseMap),
    map.foldl2(record_int_use_only, IntUseMap, !UseMap, !Specs),
    map.foldl2(record_imp_use_only, ImpUseMap, !UseMap, !Specs),

    error_if_use_for_self(ModuleName, !UseMap, !Specs),
    list.foldl2(error_if_use_for_ancestor(ModuleName),
        get_ancestors(ModuleName), !UseMap, !Specs).

%---------------------%

:- pred report_any_duplicate_avail_contexts(string::in, string::in,
    module_name::in, one_or_more(prog_context)::in, prog_context::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_duplicate_avail_contexts(Section, DeclName,
        ModuleName, OoMContexts, HeadSortedContext, !Specs) :-
    OoMContexts = one_or_more(HeadContext, TailContexts),
    % The contexts in OoMContexts are not necessarily in sorted order.
    list.sort([HeadContext | TailContexts], SortedContexts),
    (
        SortedContexts = [],
        unexpected($pred, "SortedContexts = []")
    ;
        SortedContexts = [HeadSortedContext | TailSortedContexts],
        (
            TailSortedContexts = []
        ;
            TailSortedContexts = [_ | _],
            list.foldl(
                report_duplicate_avail_context(Section, DeclName,
                    ModuleName, HeadSortedContext),
                TailSortedContexts, !Specs)
        )
    ).

:- pred report_duplicate_avail_context(string::in, string::in,
    module_name::in, prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_avail_context(Section, DeclName, ModuleName, PrevContext,
        DuplicateContext, !Specs) :-
    DupPieces = [words("Warning:")] ++
        color_as_incorrect([words("duplicate"), decl(DeclName),
            words("declaration")]) ++
        [words("for module")] ++
        color_as_subject([qual_sym_name(ModuleName)]) ++
        [words("in the"), words(Section), words("section."), nl],
    PrevPieces = [words("The previous"),
        decl(DeclName), words("declaration was here."), nl],
    DupMsg = msg(DuplicateContext, DupPieces),
    PrevMsg = msg(PrevContext, PrevPieces),
    Spec = error_spec($pred, severity_warning, phase_pt2h, [DupMsg, PrevMsg]),
    !:Specs = [Spec | !.Specs].

:- pred record_int_import(module_name::in, prog_context::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out)
    is det.

record_int_import(ModuleName, Context, !ImportUseMap) :-
    map.det_insert(ModuleName, int_import(Context), !ImportUseMap).

:- pred record_int_use(module_name::in, prog_context::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_int_use(ModuleName, Context, !ImportUseMap, !Specs) :-
    ( if map.search(!.ImportUseMap, ModuleName, OldEntry) then
        (
            OldEntry = int_import(PrevContext),
            DupPieces = [words("Warning: this"), decl("use_module"),
                words("declaration for module")] ++
                color_as_subject([qual_sym_name(ModuleName)]) ++
                [words("in the interface section is")] ++
                color_as_incorrect([words("redundant,")]) ++
                [words("given the"), decl("import_module"),
                    words("declaration for the same module"),
                words("in the same section."), nl],
            PrevPieces = [words("The previous"),
                decl("use_module"), words("declaration was here."), nl],
            DupMsg = msg(Context, DupPieces),
            PrevMsg = msg(PrevContext, PrevPieces),
            Spec = error_spec($pred, severity_warning, phase_pt2h,
                [DupMsg, PrevMsg]),
            !:Specs = [Spec | !.Specs]
        ;
            ( OldEntry = int_use(_)
            ; OldEntry = imp_import(_)
            ; OldEntry = imp_use(_)
            ; OldEntry = int_use_imp_import(_, _)
            ),
            % We haven't yet got around to adding entries of these kinds
            % to !ImportUseMap, except for int_use, which should appear
            % in !.ImportUseMap only for strictly different module names.
            unexpected($pred, "unexpected OldEntry")
        )
    else
        map.det_insert(ModuleName, int_use(Context), !ImportUseMap)
    ).

:- pred record_imp_import(module_name::in, prog_context::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_imp_import(ModuleName, Context, !ImportUseMap, !Specs) :-
    ( if map.search(!.ImportUseMap, ModuleName, OldEntry) then
        (
            OldEntry = int_import(PrevContext),
            DupPieces = [words("Warning: this"), decl("import_module"),
                words("declaration for module")] ++
                color_as_subject([qual_sym_name(ModuleName)]) ++
                [words("in the implementation section is")] ++
                color_as_subject([words("redundant,")]) ++
                [words("given the"), decl("import_module"),
                    words("declaration for the same module"),
                words("in the interface section."), nl],
            PrevPieces = [words("The previous"),
                decl("import_module"), words("declaration was here."), nl],
            DupMsg = msg(Context, DupPieces),
            PrevMsg = msg(PrevContext, PrevPieces),
            Spec = error_spec($pred, severity_warning, phase_pt2h,
                [DupMsg, PrevMsg]),
            !:Specs = [Spec | !.Specs]
        ;
            OldEntry = int_use(IntUseContext),
            map.det_update(ModuleName,
                int_use_imp_import(IntUseContext, Context), !ImportUseMap)
        ;
            ( OldEntry = imp_import(_)
            ; OldEntry = imp_use(_)
            ; OldEntry = int_use_imp_import(_, _)
            ),
            % We haven't yet got around to adding entries of these kinds
            % to !ImportUseMap, except for imp_import, which should appear
            % in !.ImportUseMap only for strictly different module names.
            unexpected($pred, "unexpected OldEntry")
        )
    else
        map.det_insert(ModuleName, imp_import(Context), !ImportUseMap)
    ).

:- pred record_imp_use(module_name::in, prog_context::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_imp_use(ModuleName, Context, !ImportUseMap, !Specs) :-
    ( if map.search(!.ImportUseMap, ModuleName, OldEntry) then
        (
            ( OldEntry = int_import(_)
            ; OldEntry = int_use(_)
            ; OldEntry = imp_import(_)
            ; OldEntry = int_use_imp_import(_, _)
            ),
            % For OldEntry = int_use_imp_import(_, _), we could report
            % the previous entry as being *either* the use in the interface
            % section or the import in the implementation section. We pick
            % the former.
            (
                OldEntry = int_import(PrevContext),
                PrevDeclName = "import_module",
                OldPieces =
                    [decl(PrevDeclName), words("declaration"),
                    words("for the same module in the interface section.")]
            ;
                OldEntry = imp_import(PrevContext),
                PrevDeclName = "import_module",
                OldPieces =
                    [decl(PrevDeclName), words("declaration"),
                    words("for the same module in the same section.")]
            ;
                ( OldEntry = int_use(PrevContext)
                ; OldEntry = int_use_imp_import(PrevContext, _)
                ),
                PrevDeclName = "use_module",
                OldPieces =
                    [decl(PrevDeclName), words("declaration"),
                    words("for the same module in the interface section.")]
            ),
            DupPieces = [words("Warning: this"), decl("use_module"),
                words("declaration for module")] ++
                color_as_subject([qual_sym_name(ModuleName)]) ++
                [words("in the implementation section is")] ++
                color_as_incorrect([words("redundant,")]) ++
                [words("given the")] ++ OldPieces ++ [nl],
            PrevPieces = [words("The previous"),
                decl(PrevDeclName), words("declaration was here."), nl],
            DupMsg = msg(Context, DupPieces),
            PrevMsg = msg(PrevContext, PrevPieces),
            Spec = error_spec($pred, severity_warning, phase_pt2h,
                [DupMsg, PrevMsg]),
            !:Specs = [Spec | !.Specs]
        ;
            OldEntry = imp_use(_),
            % We haven't yet got around to adding entries of these kinds
            % to !ImportUseMap, except for int_use, which should appear
            % in !.ImportUseMap only for strictly different module names.
            unexpected($pred, "unexpected OldEntry")
        )
    else
        map.det_insert(ModuleName, imp_use(Context), !ImportUseMap)
    ).

%---------------------%

:- pred record_int_use_only(module_name::in, prog_context::in,
    section_use_map::in, section_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_int_use_only(ModuleName, Context, !UseMap, !Specs) :-
    ( if map.search(!.UseMap, ModuleName, OldEntry) then
        ( OldEntry = int_use(_)
        ; OldEntry = imp_use(_)
        ),
        % We haven't yet got around to adding entries of these kinds
        % to !UseMap, except for int_use, which should appear
        % in !.UseMap only for strictly different module names.
        unexpected($pred, "unexpected OldEntry")
    else
        map.det_insert(ModuleName, int_use(Context), !UseMap)
    ).

:- pred record_imp_use_only(module_name::in, prog_context::in,
    section_use_map::in, section_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_imp_use_only(ModuleName, Context, !UseMap, !Specs) :-
    ( if map.search(!.UseMap, ModuleName, OldEntry) then
        (
            OldEntry = int_use(PrevContext),
            DupPieces = [words("Warning: this"), decl("use_module"),
                words("declaration for module")] ++
                color_as_subject([qual_sym_name(ModuleName)]) ++
                [words("in the implementation section is")] ++
                color_as_incorrect([words("redundant,")]) ++
                [words("given the"), decl("use_module"), words("declaration"),
                words("for the same module in the interface section."), nl],
            PrevPieces = [words("The previous"), decl("use_module"),
                words("declaration is here."), nl],
            DupMsg = msg(Context, DupPieces),
            PrevMsg = msg(PrevContext, PrevPieces),
            Spec = error_spec($pred, severity_warning, phase_pt2h,
                [DupMsg, PrevMsg]),
            !:Specs = [Spec | !.Specs]
        ;
            OldEntry = imp_use(_),
            % We haven't yet got around to adding entries of these kinds
            % to !UseMap, except for int_use, which should appear
            % in !.UseMap only for strictly different module names.
            unexpected($pred, "unexpected OldEntry")
        )
    else
        map.det_insert(ModuleName, imp_use(Context), !UseMap)
    ).

%---------------------%

    % Generate a report if a module imports itself.
    % NOTE: The main difference between this predicate and
    % error_if_use_for_self lies NOT in the severity or in
    % import vs use, but in the *type* of the map they look things up in.
    % However, the messages they generate should be kept in sync.
    %
:- pred warn_if_avail_for_self(module_name::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_avail_for_self(ModuleName, !SectionImportOrUseMap, !Specs) :-
    ( if map.remove(ModuleName, ImportOrUse, !SectionImportOrUseMap) then
        section_import_or_use_first_context(ImportOrUse, DeclName, Context),
        Pieces = [words("Warning: module")] ++
            color_as_subject([qual_sym_name(ModuleName)]) ++
            [words("has a")] ++
            color_as_incorrect([decl(DeclName),
                words("declaration for itself!")]) ++
            [nl],
        Msg = msg(Context, Pieces),
        Spec = conditional_spec($pred, warn_simple_code, yes,
            severity_warning, phase_pt2h, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

    % Generate a report if a module imports an ancestor.
    % NOTE: The main difference between this predicate and
    % error_if_use_for_ancestor lies NOT in the severity or in
    % import vs use, but in the *type* of the map they look things up in.
    % However, the messages they generate should be kept in sync.
    %
:- pred warn_if_avail_for_ancestor(module_name::in, module_name::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_if_avail_for_ancestor(ModuleName, AncestorName,
        !SectionImportOrUseMap, !Specs) :-
    ( if map.remove(AncestorName, ImportOrUse, !SectionImportOrUseMap) then
        section_import_or_use_first_context(ImportOrUse, DeclName, Context),
        MainPieces = [words("Warning: module")] ++
            color_as_subject([qual_sym_name(ModuleName)]) ++
            [words("has a")] ++
            color_as_incorrect([decl(DeclName),
                words("declaration for its own ancestor module,")]) ++
            [qual_sym_name(AncestorName), suffix("."), nl],
        VerbosePieces = [words("Every submodule"),
            words("implicitly imports its ancestors."),
            words("There is no need to explicitly import them."), nl],
        Msg = simple_msg(Context,
            [always(MainPieces), verbose_only(verbose_once, VerbosePieces)]),
        Spec = conditional_spec($pred, warn_simple_code, yes,
            severity_warning, phase_pt2h, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- pred section_import_or_use_first_context(section_import_and_or_use::in,
    string::out, prog_context::out) is det.

section_import_or_use_first_context(ImportOrUse, DeclName, Context) :-
    (
        ImportOrUse = int_import(Context),
        DeclName = "import_module"
    ;
        ImportOrUse = int_use(Context),
        DeclName = "use_module"
    ;
        ImportOrUse = imp_import(Context),
        DeclName = "import_module"
    ;
        ImportOrUse = imp_use(Context),
        DeclName = "use_module"
    ;
        ImportOrUse = int_use_imp_import(ContextA, ContextB),
        ( if compare((<), ContextB, ContextA) then
            Context = ContextB,
            DeclName = "import_module"
        else
            Context = ContextA,
            DeclName = "use_module"
        )
    ).

%---------------------%

    % Generate an error if a module imports itself.
    % NOTE: The main difference between this predicate and
    % warn_if_avail_for_ancestor lies NOT in the severity or in
    % import vs use, but in the *type* of the map they look things up in.
    % However, the messages they generate should be kept in sync.
    %
:- pred error_if_use_for_self(module_name::in,
    section_use_map::in, section_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

error_if_use_for_self(ModuleName, !UseMap, !Specs) :-
    ( if map.remove(ModuleName, Use, !UseMap) then
        Pieces = [words("Error: module")] ++
            color_as_subject([qual_sym_name(ModuleName)]) ++
            [words("has a")] ++
            color_as_incorrect([decl("use_module"),
                words("declaration for itself!")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_pt2h,
            section_use_first_context(Use), Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

    % Generate an error if a module imports an ancestor.
    % NOTE: The main difference between this predicate and
    % warn_if_avail_for_ancestor lies NOT in the severity or in
    % import vs use, but in the *type* of the map they look things up in.
    % However, the messages they generate should be kept in sync.
    %
:- pred error_if_use_for_ancestor(module_name::in, module_name::in,
    section_use_map::in, section_use_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

error_if_use_for_ancestor(ModuleName, AncestorName, !UseMap, !Specs) :-
    ( if map.remove(ModuleName, Use, !UseMap) then
        Pieces = [words("Error: module")] ++
            color_as_subject([qual_sym_name(ModuleName)]) ++
            [words("has a")] ++
            color_as_incorrect([decl("use_module"),
                words("declaration for its own ancestor module,")]) ++
            [qual_sym_name(AncestorName), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_pt2h,
            section_use_first_context(Use), Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- func section_use_first_context(section_use) = prog_context.

section_use_first_context(Use) = Context :-
    (
        Use = int_use(Context)
    ;
        Use = imp_use(Context)
    ).

%---------------------%

import_and_or_use_map_section_to_maybe_implicit(SectionImportUseMap,
        ImportUseMap) :-
    map.map_values_only(wrap_section_import_and_or_use,
        SectionImportUseMap, ImportUseMap).

:- pred wrap_section_import_and_or_use(section_import_and_or_use::in,
    maybe_implicit_import_and_or_use::out) is det.

wrap_section_import_and_or_use(SectionImportUse, MaybeImplicitUse) :-
    MaybeImplicitUse = explicit_avail(SectionImportUse).

%---------------------%

section_use_map_to_item_avails(UseMap, IntAvails, ImpAvails) :-
    map.foldl2(section_use_map_to_item_avails_acc,
        UseMap, [], RevIntAvails, [], RevImpAvails),
    list.reverse(RevIntAvails, IntAvails),
    list.reverse(RevImpAvails, ImpAvails).

:- pred section_use_map_to_item_avails_acc(module_name::in, section_use::in,
    list(item_avail)::in, list(item_avail)::out,
    list(item_avail)::in, list(item_avail)::out) is det.

section_use_map_to_item_avails_acc(ModuleName, Use,
        !RevIntAvails, !RevImpAvails) :-
    get_explicit_use_avails(ModuleName, Use, IntAvails, ImpAvails),
    !:RevIntAvails = IntAvails ++ !.RevIntAvails,
    !:RevImpAvails = ImpAvails ++ !.RevImpAvails.

%---------------------%

section_import_and_or_use_map_to_item_avails(ImportUseMap,
        IntAvails, ImpAvails) :-
    map.foldl2(section_import_and_or_use_map_to_item_avails_acc,
        ImportUseMap, [], RevIntAvails, [], RevImpAvails),
    list.reverse(RevIntAvails, IntAvails),
    list.reverse(RevImpAvails, ImpAvails).

:- pred section_import_and_or_use_map_to_item_avails_acc(
    module_name::in, section_import_and_or_use::in,
    list(item_avail)::in, list(item_avail)::out,
    list(item_avail)::in, list(item_avail)::out) is det.

section_import_and_or_use_map_to_item_avails_acc(ModuleName, ImportAndOrUse,
        !RevIntAvails, !RevImpAvails) :-
    get_explicit_avails(ModuleName, ImportAndOrUse, IntAvails, ImpAvails),
    !:RevIntAvails = IntAvails ++ !.RevIntAvails,
    !:RevImpAvails = ImpAvails ++ !.RevImpAvails.

%---------------------%

import_and_or_use_map_to_item_avails(IncludeImplicit, ImportUseMap,
        IntAvails, ImpAvails) :-
    map.foldl2(import_and_or_use_map_to_item_avails_acc(IncludeImplicit),
        ImportUseMap, [], RevIntAvails, [], RevImpAvails),
    list.reverse(RevIntAvails, IntAvails),
    list.reverse(RevImpAvails, ImpAvails).

:- pred import_and_or_use_map_to_item_avails_acc(maybe_include_implicit::in,
    module_name::in, maybe_implicit_import_and_or_use::in,
    list(item_avail)::in, list(item_avail)::out,
    list(item_avail)::in, list(item_avail)::out) is det.

import_and_or_use_map_to_item_avails_acc(IncludeImplicit,
        ModuleName, ImportAndOrUse, !RevIntAvails, !RevImpAvails) :-
    % This predicate shares its logic with
    % import_and_or_use_map_to_module_name_contexts_acc.
    %
    % XXX CLEANUP Many invocations of ++ below are redundant,
    % since they append a list that should be empty.
    (
        ImportAndOrUse = explicit_avail(Explicit),
        section_import_and_or_use_map_to_item_avails_acc(ModuleName, Explicit,
            !RevIntAvails, !RevImpAvails)
    ;
        ImportAndOrUse = implicit_avail(Implicit, MaybeExplicit),
        get_implicit_avails(ModuleName, Implicit,
            ImplicitIntAvails, ImplicitImpAvails),
        (
            IncludeImplicit = do_not_include_implicit,
            (
                MaybeExplicit = no
            ;
                MaybeExplicit = yes(Explicit),
                section_import_and_or_use_map_to_item_avails_acc(ModuleName,
                    Explicit, !RevIntAvails, !RevImpAvails)
            )
        ;
            IncludeImplicit = do_include_implicit,
            (
                MaybeExplicit = no,
                !:RevIntAvails = ImplicitIntAvails ++ !.RevIntAvails,
                !:RevImpAvails = ImplicitImpAvails ++ !.RevImpAvails
            ;
                MaybeExplicit = yes(Explicit),
                get_explicit_avails(ModuleName, Explicit,
                    ExplicitIntAvails, ExplicitImpAvails),
                % Include only the avails from Implicit*Avails that grant
                % some permission not granted by Explicit*Avails.

                % Include all avails from Explicit*Avails except those
                % that are implied by strictly stronger avails from
                % Implicit*Avails.
                (
                    Explicit = int_import(_),
                    % Explicit grants all possible permissions for the module,
                    % so any avails from Implicit would be redundant.
                    !:RevIntAvails = ExplicitIntAvails ++ !.RevIntAvails,
                    !:RevImpAvails = ExplicitImpAvails ++ !.RevImpAvails
                ;
                    Explicit = int_use(_),
                    (
                        Implicit = implicit_int_import,
                        % Implicit is strictly stronger.
                        !:RevIntAvails = ImplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ImplicitImpAvails ++ !.RevImpAvails
                    ;
                        ( Implicit = implicit_int_use
                        ; Implicit = implicit_imp_use
                        ),
                        % Explicit grants all the permissions that
                        % Implicit does.
                        !:RevIntAvails = ExplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ExplicitImpAvails ++ !.RevImpAvails
                    )
                ;
                    Explicit = imp_import(_),
                    (
                        Implicit = implicit_int_import,
                        % Implicit is strictly stronger.
                        !:RevIntAvails = ImplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ImplicitImpAvails ++ !.RevImpAvails
                    ;
                        Implicit = implicit_int_use,
                        % In the interface, Implicit grants more permissions
                        % (since it grants some, while Explicit does not).
                        % In the implementation, Explicit grants more
                        % permissions.
                        !:RevIntAvails = ImplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ExplicitImpAvails ++ !.RevImpAvails
                    ;
                        Implicit = implicit_imp_use,
                        % Explicit grants all the permissions that
                        % Implicit does.
                        !:RevIntAvails = ExplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ExplicitImpAvails ++ !.RevImpAvails
                    )
                ;
                    Explicit = imp_use(_),
                    (
                        ( Implicit = implicit_int_import
                        ; Implicit = implicit_int_use
                        ),
                        % Implicit is strictly stronger.
                        !:RevIntAvails = ImplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ImplicitImpAvails ++ !.RevImpAvails
                    ;
                        Implicit = implicit_imp_use,
                        % Implicit and Explicit grants the same permissions,
                        % but Explicit supplies a meaningful context.
                        !:RevIntAvails = ExplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ExplicitImpAvails ++ !.RevImpAvails
                    )
                ;
                    Explicit = int_use_imp_import(_, _),
                    (
                        Implicit = implicit_int_import,
                        % Implicit is strictly stronger.
                        !:RevIntAvails = ImplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ImplicitImpAvails ++ !.RevImpAvails
                    ;
                        ( Implicit = implicit_int_use
                        ; Implicit = implicit_imp_use
                        ),
                        % Explicit is strictly stronger.
                        !:RevIntAvails = ExplicitIntAvails ++ !.RevIntAvails,
                        !:RevImpAvails = ExplicitImpAvails ++ !.RevImpAvails
                    )
                )
            )
        )
    ).

:- pred get_implicit_avails(module_name::in, implicit_import_or_use::in,
    list(item_avail)::out, list(item_avail)::out) is det.

get_implicit_avails(ModuleName, Implicit, IntAvails, ImpAvails) :-
    Context = term_context.context_init("implicit", -1),
    SN = item_no_seq_num,
    (
        Implicit = implicit_int_import,
        Avail = avail_import(avail_import_info(ModuleName, Context, SN)),
        IntAvails = [Avail],
        ImpAvails = []
    ;
        Implicit = implicit_int_use,
        Avail = avail_use(avail_use_info(ModuleName, Context, SN)),
        IntAvails = [Avail],
        ImpAvails = []
    ;
        Implicit = implicit_imp_use,
        Avail = avail_use(avail_use_info(ModuleName, Context, SN)),
        IntAvails = [],
        ImpAvails = [Avail]
    ).

:- pred get_explicit_use_avails(module_name::in, section_use::in,
    list(item_avail)::out, list(item_avail)::out) is det.

get_explicit_use_avails(ModuleName, Explicit, IntAvails, ImpAvails) :-
    SN = item_no_seq_num,
    (
        Explicit = int_use(Context),
        Avail = avail_use(avail_use_info(ModuleName, Context, SN)),
        IntAvails = [Avail],
        ImpAvails = []
    ;
        Explicit = imp_use(Context),
        Avail = avail_use(avail_use_info(ModuleName, Context, SN)),
        IntAvails = [],
        ImpAvails = [Avail]
    ).

:- pred get_explicit_avails(module_name::in, section_import_and_or_use::in,
    list(item_avail)::out, list(item_avail)::out) is det.

get_explicit_avails(ModuleName, Explicit, IntAvails, ImpAvails) :-
    SN = item_no_seq_num,
    (
        Explicit = int_import(Context),
        Avail = avail_import(avail_import_info(ModuleName, Context, SN)),
        IntAvails = [Avail],
        ImpAvails = []
    ;
        Explicit = int_use(Context),
        Avail = avail_use(avail_use_info(ModuleName, Context, SN)),
        IntAvails = [Avail],
        ImpAvails = []
    ;
        Explicit = imp_import(Context),
        Avail = avail_import(avail_import_info(ModuleName, Context, SN)),
        IntAvails = [],
        ImpAvails = [Avail]
    ;
        Explicit = imp_use(Context),
        Avail = avail_use(avail_use_info(ModuleName, Context, SN)),
        IntAvails = [],
        ImpAvails = [Avail]
    ;
        Explicit = int_use_imp_import(IntContext, ImpContext),
        IntAvail = avail_use(avail_use_info(ModuleName, IntContext, SN)),
        ImpAvail = avail_import(avail_import_info(ModuleName, ImpContext, SN)),
        IntAvails = [IntAvail],
        ImpAvails = [ImpAvail]
    ).

%---------------------%

import_and_or_use_map_to_explicit_int_imp_import_use_maps(ImportUseMap,
        SectionImportUseMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap) :-
    map.foldl5(
        import_and_or_use_map_to_explicit_int_imp_import_use_maps_acc,
        ImportUseMap,
        map.init, SectionImportUseMap,
        map.init, IntImportMap0,
        map.init, IntUseMap0,
        map.init, ImpImportMap0,
        map.init, ImpUseMap0),
    IntImportMap = int_import_context_map(IntImportMap0),
    IntUseMap = int_use_context_map(IntUseMap0),
    ImpImportMap = imp_import_context_map(ImpImportMap0),
    ImpUseMap = imp_use_context_map(ImpUseMap0).

:- pred import_and_or_use_map_to_explicit_int_imp_import_use_maps_acc(
    module_name::in,
    maybe_implicit_import_and_or_use::in,
    section_import_and_or_use_map::in, section_import_and_or_use_map::out,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out) is det.

import_and_or_use_map_to_explicit_int_imp_import_use_maps_acc(ModuleName,
        ImportAndOrUse, !SectionImportAndOrUseMap,
        !IntImportMap, !IntUseMap, !ImpImportMap, !ImpUseMap) :-
    ( if
        (
            ImportAndOrUse = explicit_avail(Explicit)
        ;
            ImportAndOrUse = implicit_avail(_Implicit, MaybeExplicit),
            MaybeExplicit = yes(Explicit)
        )
    then
        map.det_insert(ModuleName, Explicit, !SectionImportAndOrUseMap),
        (
            Explicit = int_import(Context),
            map.det_insert(ModuleName, Context, !IntImportMap)
        ;
            Explicit = int_use(Context),
            map.det_insert(ModuleName, Context, !IntUseMap)
        ;
            Explicit = imp_import(Context),
            map.det_insert(ModuleName, Context, !ImpImportMap)
        ;
            Explicit = imp_use(Context),
            map.det_insert(ModuleName, Context, !ImpUseMap)
        ;
            Explicit = int_use_imp_import(IntContext, ImpContext),
            map.det_insert(ModuleName, IntContext, !IntUseMap),
            map.det_insert(ModuleName, ImpContext, !ImpImportMap)
        )
    else
        true
    ).

%---------------------%

import_and_or_use_map_to_module_name_contexts(ImportUseMap,
        IntImports, IntUses, ImpImports, ImpUses, IntUseImpImports) :-
    map.foldl5(import_and_or_use_map_to_module_name_contexts_acc, ImportUseMap,
        map.init, IntImports,
        map.init, IntUses,
        map.init, ImpImports,
        map.init, ImpUses,
        map.init, IntUseImpImports).

:- pred import_and_or_use_map_to_module_name_contexts_acc(module_name::in,
    maybe_implicit_import_and_or_use::in,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out,
    module_name_context::in, module_name_context::out) is det.

import_and_or_use_map_to_module_name_contexts_acc(ModuleName, ImportAndOrUse,
        !IntImports, !IntUses, !ImpImports, !ImpUses, !IntUseImpImports) :-
    % This predicate shares its logic with
    % import_and_or_use_map_to_item_avails_acc.
    (
        ImportAndOrUse = explicit_avail(Explicit),
        (
            Explicit = int_import(Context),
            map.det_insert(ModuleName, Context, !IntImports)
        ;
            Explicit = int_use(Context),
            map.det_insert(ModuleName, Context, !IntUses)
        ;
            Explicit = imp_import(Context),
            map.det_insert(ModuleName, Context, !ImpImports)
        ;
            Explicit = imp_use(Context),
            map.det_insert(ModuleName, Context, !ImpUses)
        ;
            Explicit = int_use_imp_import(Context, _),
            map.det_insert(ModuleName, Context, !IntUseImpImports)
        )
    ;
        ImportAndOrUse = implicit_avail(Implicit, MaybeExplicit),
        ImplicitContext = term_context.context_init("implicit", -1),
        (
            MaybeExplicit = no,
            (
                Implicit = implicit_int_import,
                map.det_insert(ModuleName, ImplicitContext, !IntImports)
            ;
                Implicit = implicit_int_use,
                map.det_insert(ModuleName, ImplicitContext, !IntUses)
            ;
                Implicit = implicit_imp_use,
                map.det_insert(ModuleName, ImplicitContext, !ImpUses)
            )
        ;
            MaybeExplicit = yes(Explicit),
            (
                Explicit = int_import(Context),
                map.det_insert(ModuleName, Context, !IntImports)
            ;
                Explicit = int_use(Context),
                (
                    Implicit = implicit_int_import,
                    % Implicit is strictly stronger.
                    map.det_insert(ModuleName, ImplicitContext, !IntImports)
                ;
                    ( Implicit = implicit_int_use
                    ; Implicit = implicit_imp_use
                    ),
                    % Explicit is at least as strong as Implicit.
                    map.det_insert(ModuleName, Context, !IntUses)
                )
            ;
                Explicit = imp_import(Context),
                (
                    Implicit = implicit_int_import,
                    % Implicit is strictly stronger.
                    map.det_insert(ModuleName, ImplicitContext, !IntImports)
                ;
                    Implicit = implicit_int_use,
                    % In the interface, Implicit grants more permissions
                    % (since it grants some, while Explicit does not).
                    % In the implementation, Explicit grants more permissions.
                    map.det_insert(ModuleName, Context, !IntUseImpImports)
                ;
                    Implicit = implicit_imp_use,
                    % Explicit grants all the permissions that Implicit does.
                    map.det_insert(ModuleName, Context, !ImpImports)
                )
            ;
                Explicit = imp_use(Context),
                (
                    Implicit = implicit_int_import,
                    % Implicit is strictly stronger.
                    map.det_insert(ModuleName, ImplicitContext, !IntImports)
                ;
                    Implicit = implicit_int_use,
                    % Implicit is strictly stronger.
                    map.det_insert(ModuleName, ImplicitContext, !IntUses)
                ;
                    Implicit = implicit_imp_use,
                    % Implicit and Explicit grants the same permissions.
                    map.det_insert(ModuleName, Context, !ImpUses)
                )
            ;
                Explicit = int_use_imp_import(Context, _),
                (
                    Implicit = implicit_int_import,
                    % Implicit is strictly stronger.
                    map.det_insert(ModuleName, ImplicitContext, !IntImports)
                ;
                    ( Implicit = implicit_int_use
                    ; Implicit = implicit_imp_use
                    ),
                    % Explicit is strictly stronger.
                    map.det_insert(ModuleName, Context, !IntUseImpImports)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

use_map_to_item_avails(UseMap) = Avails :-
    map.foldl(use_map_to_item_avails_acc, UseMap, [], RevAvails),
    list.reverse(RevAvails, Avails).

:- pred use_map_to_item_avails_acc(module_name::in,
    one_or_more(prog_context)::in,
    list(item_avail)::in, list(item_avail)::out) is det.

use_map_to_item_avails_acc(ModuleName, Contexts, !RevAvails) :-
    Contexts = one_or_more(Context, _),
    Avail = avail_use(avail_use_info(ModuleName, Context, item_no_seq_num)),
    !:RevAvails = [Avail | !.RevAvails].

acc_avails_with_contexts(ImportOrUse, ModuleName, Contexts, !RevAvails) :-
    one_or_more.foldl(acc_avail_with_context(ImportOrUse, ModuleName),
        Contexts, !RevAvails).

:- pred acc_avail_with_context(import_or_use::in, module_name::in,
    prog_context::in, list(item_avail)::in, list(item_avail)::out) is det.

acc_avail_with_context(ImportOrUse, ModuleName, Context, !RevAvails) :-
    (
        ImportOrUse = import_decl,
        Avail = avail_import(avail_import_info(ModuleName, Context,
            item_no_seq_num))
    ;
        ImportOrUse = use_decl,
        Avail = avail_use(avail_use_info(ModuleName, Context,
            item_no_seq_num))
    ),
    !:RevAvails = [Avail | !.RevAvails].

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

import_or_use_decl_name(import_decl) = "import_module".
import_or_use_decl_name(use_decl) = "use_module".

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

%---------------------------------------------------------------------------%

fim_item_to_spec(FIM) = FIMSpec :-
    FIM = item_fim(Lang, ModuleName, _, _),
    FIMSpec = fim_spec(Lang, ModuleName).

fim_spec_to_item(FIMSpec) = FIM :-
    FIMSpec = fim_spec(Lang, ModuleName),
    FIM = item_fim(Lang, ModuleName, dummy_context, item_no_seq_num).

fim_module_lang_to_spec(ModuleName, Lang) = fim_spec(Lang, ModuleName).

fim_module_lang_to_item(ModuleName, Lang) =
    item_fim(Lang, ModuleName, dummy_context, item_no_seq_num).

add_implicit_fim_for_module(ModuleName, Lang, !Map) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    ( if map.search(!.Map, FIMSpec, _) then
        true
    else
        map.det_insert(FIMSpec, dummy_context, !Map)
    ).

%---------------------------------------------------------------------------%

item_needs_foreign_imports(Item) = Langs :-
    (
        Item = item_mutable(_ItemMutable),
        % We can use all foreign languages.
        Langs = all_foreign_languages
    ;
        Item = item_type_defn(ItemTypeDefn),
        ( if
            ItemTypeDefn ^ td_ctor_defn =
                parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _)
        then
            Langs = [foreign_type_language(ForeignType)]
        else
            Langs = []
        )
    ;
        Item = item_foreign_proc(FPInfo),
        FPInfo = item_foreign_proc_info(Attrs, _, _, _, _, _, _, _, _),
        Langs = [get_foreign_language(Attrs)]
    ;
        Item = item_foreign_enum(FEInfo),
        FEInfo = item_foreign_enum_info(Lang, _, _, _, _),
        Langs = [Lang]
    ;
        Item = item_impl_pragma(ItemImplPragma),
        Langs = impl_pragma_needs_foreign_imports(ItemImplPragma)
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        Langs = []
    ;
        Item = item_type_repn(_),
        % These should not occur in source files.
        unexpected($pred, "item_type_repn")
    ).

acc_needed_self_fim_langs_for_type_defn(ItemTypeDefn, !Langs) :-
    ( if
        ItemTypeDefn ^ td_ctor_defn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, _, _)
    then
        set.insert(foreign_type_language(ForeignType), !Langs)
    else
        true
    ).

acc_needed_self_fim_langs_for_foreign_proc(FPInfo, !Langs) :-
    FPInfo = item_foreign_proc_info(Attrs, _, _, _, _, _, _, _, _),
    set.insert(get_foreign_language(Attrs), !Langs).

acc_needed_self_fim_langs_for_foreign_enum(FEInfo, !Langs) :-
    FEInfo = item_foreign_enum_info(Lang, _, _, _, _),
    set.insert(Lang, !Langs).

acc_needed_self_fim_langs_for_impl_pragma(ItemImplPragma, !Langs) :-
    set.insert_list(impl_pragma_needs_foreign_imports(ItemImplPragma), !Langs).

:- func impl_pragma_needs_foreign_imports(item_impl_pragma_info)
    = list(foreign_language).

impl_pragma_needs_foreign_imports(ImplPragma) = Langs :-
    (
        (
            ImplPragma = impl_pragma_foreign_decl(FDInfo),
            FDInfo = impl_pragma_foreign_decl_info(Lang, _, _, _, _)
        ;
            ImplPragma = impl_pragma_foreign_code(FCInfo),
            FCInfo = impl_pragma_foreign_code_info(Lang, _, _, _)
        ;
            ImplPragma = impl_pragma_fproc_export(FPEInfo),
            FPEInfo = impl_pragma_fproc_export_info(_, Lang, _, _, _, _, _)
        ),
        Langs = [Lang]
    ;
        ( ImplPragma = impl_pragma_external_proc(_)
        ; ImplPragma = impl_pragma_tabled(_)
        ; ImplPragma = impl_pragma_fact_table(_)
        ; ImplPragma = impl_pragma_req_tail_rec(_)
        ; ImplPragma = impl_pragma_req_feature_set(_)
        ),
        Langs = []
    ).

%---------------------------------------------------------------------------%

item_desc_pieces(Item) = Pieces :-
    % If you change this code, see whether items_desc_to_pieces needs
    % updating as well.
    (
        Item = item_clause(_),
        Pieces = [words("clause")]
    ;
        Item = item_type_defn(_),
        Pieces = [words("type definition")]
    ;
        Item = item_inst_defn(_),
        Pieces = [words("inst definition")]
    ;
        Item = item_mode_defn(_),
        Pieces = [words("mode definition")]
    ;
        Item = item_pred_decl(ItemPredDecl),
        PorF = ItemPredDecl ^ pf_p_or_f,
        (
            PorF = pf_predicate,
            Pieces = [words("predicate declaration")]
        ;
            PorF = pf_function,
            Pieces = [words("function declaration")]
        )
    ;
        Item = item_mode_decl(_),
        Pieces = [words("mode declaration")]
    ;
        Item = item_foreign_proc(_),
        Pieces = [pragma_decl("foreign_proc"), words("declaration")]
    ;
        Item = item_foreign_enum(_),
        Pieces = [pragma_decl("foreign_enum"), words("declaration")]
    ;
        Item = item_foreign_export_enum(_),
        Pieces = [pragma_decl("foreign_export_enum"), words("declaration")]
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        Pieces = decl_pragma_desc_pieces(ItemDeclPragma)
    ;
        Item = item_decl_marker(ItemDeclMarker),
        Pieces = decl_marker_desc_pieces(ItemDeclMarker)
    ;
        Item = item_impl_pragma(ItemImplPragma),
        Pieces = impl_pragma_desc_pieces(ItemImplPragma)
    ;
        Item = item_impl_marker(ItemImplMarker),
        Pieces = impl_marker_desc_pieces(ItemImplMarker)
    ;
        Item = item_generated_pragma(ItemGenPragma),
        Pieces = gen_pragma_desc_pieces(ItemGenPragma)
    ;
        Item = item_promise(ItemPromise),
        PromiseType = ItemPromise ^ prom_type,
        (
            PromiseType = promise_type_exclusive,
            Pieces = [words("exclusivity promise")]
        ;
            PromiseType = promise_type_exhaustive,
            Pieces = [words("exhaustivity promise")]
        ;
            PromiseType = promise_type_exclusive_exhaustive,
            Pieces = [words("exclusivity and exhaustivity promise")]
        ;
            PromiseType = promise_type_true,
            Pieces = [words("promise")]
        )
    ;
        Item = item_typeclass(_),
        Pieces = [words("typeclass declaration")]
    ;
        Item = item_instance(_),
        Pieces = [words("instance declaration")]
    ;
        Item = item_initialise(_),
        Pieces = [decl("initialise"), words("declaration")]
    ;
        Item = item_finalise(_),
        Pieces = [decl("finalise"), words("declaration")]
    ;
        Item = item_mutable(_),
        Pieces = [decl("mutable"), words("declaration")]
    ;
        Item = item_type_repn(_),
        Pieces = [decl("type_repn"), words("declaration")]
    ).

items_desc_pieces(Item) = Pieces :-
    % At the moment, all the Pieces0 that item_desc_pieces can generate
    % can be made plural by adding a single final "s".
    Pieces0 = item_desc_pieces(Item),
    Pieces = Pieces0 ++ [suffix("s")].

decl_pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = decl_pragma_obsolete_pred(_),
        Pieces = [pragma_decl("obsolete"), words("declaration")]
    ;
        Pragma = decl_pragma_obsolete_proc(_),
        Pieces = [pragma_decl("obsolete_proc"), words("declaration")]
    ;
        Pragma = decl_pragma_format_call(_),
        Pieces = [pragma_decl("format_call"), words("declaration")]
    ;
        Pragma = decl_pragma_type_spec_constr(_),
        Pieces = [pragma_decl("type_spec_constrained_preds"),
            words("declaration")]
    ;
        Pragma = decl_pragma_type_spec(_),
        Pieces = [pragma_decl("type_spec"), words("declaration")]
    ;
        Pragma = decl_pragma_oisu(_),
        Pieces = [pragma_decl("oisu"), words("declaration")]
    ;
        Pragma = decl_pragma_termination(_),
        Pieces = [pragma_decl("termination_info"), words("declaration")]
    ;
        Pragma = decl_pragma_termination2(_),
        Pieces = [pragma_decl("termination2_info"), words("declaration")]
    ;
        Pragma = decl_pragma_struct_sharing(_),
        Pieces = [pragma_decl("structure_sharing"), words("declaration")]
    ;
        Pragma = decl_pragma_struct_reuse(_),
        Pieces = [pragma_decl("structure_reuse"), words("declaration")]
    ).

decl_marker_desc_pieces(Marker) = Pieces :-
    Marker = item_decl_marker_info(MarkerKind, _, _, _),
    (
        MarkerKind = dpmk_terminates,
        Pieces = [pragma_decl("terminates"), words("declaration")]
    ;
        MarkerKind = dpmk_does_not_terminate,
        Pieces = [pragma_decl("does_not_terminate"), words("declaration")]
    ;
        MarkerKind = dpmk_check_termination,
        Pieces = [pragma_decl("check_termination"), words("declaration")]
    ).

impl_pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = impl_pragma_foreign_code(_),
        Pieces = [pragma_decl("foreign_code"), words("declaration")]
    ;
        Pragma = impl_pragma_foreign_decl(_),
        Pieces = [pragma_decl("foreign_decl"), words("declaration")]
    ;
        Pragma = impl_pragma_fproc_export(_),
        Pieces = [pragma_decl("foreign_export"), words("declaration")]
    ;
        Pragma = impl_pragma_external_proc(External),
        External = impl_pragma_external_proc_info(PFNameArity, _, _, _),
        PFNameArity = pred_pf_name_arity(PorF, _, _),
        (
            PorF = pf_predicate,
            Pieces = [pragma_decl("external_pred"), words("declaration")]
        ;
            PorF = pf_function,
            Pieces = [pragma_decl("external_func"), words("declaration")]
        )
    ;
        Pragma = impl_pragma_req_tail_rec(_),
        Pieces = [pragma_decl("require_tail_recursion"), words("declaration")]
    ;
        Pragma = impl_pragma_fact_table(_),
        Pieces = [pragma_decl("fact_table"), words("declaration")]
    ;
        Pragma = impl_pragma_tabled(Tabled),
        Tabled = impl_pragma_tabled_info(TabledMethod, _, _, _, _),
        (
            TabledMethod = tabled_memo(_),
            Pieces = [pragma_decl("memo"), words("declaration")]
        ;
            TabledMethod = tabled_loop_check,
            Pieces = [pragma_decl("loop_check"), words("declaration")]
        ;
            TabledMethod = tabled_minimal(_),
            Pieces = [pragma_decl("minimal_model"), words("declaration")]
        ;
            TabledMethod = tabled_io(_, _),
            unexpected($pred, "eval_table_io")
        )
    ;
        Pragma = impl_pragma_req_feature_set(_),
        Pieces = [pragma_decl("require_feature_set"), words("declaration")]
    ).

impl_marker_desc_pieces(Marker) = Pieces :-
    Marker = item_impl_marker_info(MarkerKind, _, _, _),
    (
        MarkerKind = ipmk_inline,
        Pieces = [pragma_decl("inline"), words("declaration")]
    ;
        MarkerKind = ipmk_no_inline,
        Pieces = [pragma_decl("no_inline"), words("declaration")]
    ;
        MarkerKind = ipmk_consider_used,
        Pieces = [pragma_decl("consider_used"), words("declaration")]
    ;
        MarkerKind = ipmk_mode_check_clauses,
        Pieces = [pragma_decl("mode_check_clauses"), words("declaration")]
    ;
        MarkerKind = ipmk_no_detism_warning,
        Pieces = [pragma_decl("no_determinism_warning"), words("declaration")]
    ;
        MarkerKind = ipmk_promise_pure,
        Pieces = [pragma_decl("promise_pure"), words("declaration")]
    ;
        MarkerKind = ipmk_promise_semipure,
        Pieces = [pragma_decl("promise_semipure"), words("declaration")]
    ;
        MarkerKind = ipmk_promise_eqv_clauses,
        Pieces = [pragma_decl("promise_equivalent_clauses"),
            words("declaration")]
    ).

gen_pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = gen_pragma_unused_args(_),
        Pieces = [pragma_decl("unused_args"), words("declaration")]
    ;
        Pragma = gen_pragma_exceptions(_),
        Pieces = [pragma_decl("exceptions"), words("declaration")]
    ;
        Pragma = gen_pragma_trailing(_),
        Pieces = [pragma_decl("trailing_info"), words("declaration")]
    ;
        Pragma = gen_pragma_mm_tabling(_),
        Pieces = [pragma_decl("mm_tabling_info"), words("declaration")]
    ).

%---------------------------------------------------------------------------%

parse_tree_module_src_project_name(ParseTreeModuleSrc) =
    ParseTreeModuleSrc ^ ptms_module_name.

item_include_module_name(Incl) = ModuleName :-
    Incl = item_include(ModuleName, _Context, _SeqNum).

get_avail_context(avail_import(avail_import_info(_, Context, _))) = Context.
get_avail_context(avail_use(avail_use_info(_, Context, _))) = Context.

get_import_context(avail_import_info(_, Context, _)) = Context.

get_use_context(avail_use_info(_, Context, _)) = Context.

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

%---------------------------------------------------------------------------%

type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, ImpForeignEnums) :-
    map.values(TypeCtorCheckedMap, TypeCtorCheckedDefns),
    list.map3(type_ctor_checked_defn_get_src_defns, TypeCtorCheckedDefns,
        IntTypeDefnLists, ImpTypeDefnLists, ImpForeignEnumLists),
    list.condense(IntTypeDefnLists, IntTypeDefns),
    list.condense(ImpTypeDefnLists, ImpTypeDefns),
    list.condense(ImpForeignEnumLists, ImpForeignEnums).

inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, ImpInstDefns) :-
    map.values(InstCtorCheckedMap, InstCtorCheckedDefns),
    list.map2(inst_ctor_checked_defn_get_src_defns, InstCtorCheckedDefns,
        IntInstDefnLists, ImpInstDefnLists),
    list.condense(IntInstDefnLists, IntInstDefns),
    list.condense(ImpInstDefnLists, ImpInstDefns).

mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, ImpModeDefns) :-
    map.values(ModeCtorCheckedMap, ModeCtorCheckedDefns),
    list.map2(mode_ctor_checked_defn_get_src_defns, ModeCtorCheckedDefns,
        IntModeDefnLists, ImpModeDefnLists),
    list.condense(IntModeDefnLists, IntModeDefns),
    list.condense(ImpModeDefnLists, ImpModeDefns).

%---------------------%

:- pred type_ctor_checked_defn_get_src_defns(type_ctor_checked_defn::in,
    list(item_type_defn_info)::out, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::out) is det.

type_ctor_checked_defn_get_src_defns(CheckedDefn, IntDefns, ImpDefns,
        ImpForeignEnums) :-
    (
        CheckedDefn = checked_defn_solver(_, SrcDefnsSolver),
        SrcDefnsSolver = src_defns_solver(MaybeIntDefn, MaybeImpDefn),
        IntDefns = maybe_to_list(MaybeIntDefn),
        ImpDefns = maybe_to_list(MaybeImpDefn),
        ImpForeignEnums = []
    ;
        CheckedDefn = checked_defn_std(_, SrcDefnsStd),
        SrcDefnsStd = src_defns_std(IntDefns, ImpDefns, ImpForeignEnums)
    ).

:- pred inst_ctor_checked_defn_get_src_defns(inst_ctor_checked_defn::in,
    list(item_inst_defn_info)::out, list(item_inst_defn_info)::out) is det.

inst_ctor_checked_defn_get_src_defns(CheckedDefn, IntDefns, ImpDefns) :-
    CheckedDefn = checked_defn_inst(_, SrcDefns),
    SrcDefns = src_defns_inst(MaybeIntDefn, MaybeImpDefn),
    IntDefns = maybe_to_list(MaybeIntDefn),
    ImpDefns = maybe_to_list(MaybeImpDefn).

:- pred mode_ctor_checked_defn_get_src_defns(mode_ctor_checked_defn::in,
    list(item_mode_defn_info)::out, list(item_mode_defn_info)::out) is det.

mode_ctor_checked_defn_get_src_defns(CheckedDefn, IntDefns, ImpDefns) :-
    CheckedDefn = checked_defn_mode(_, SrcDefns),
    SrcDefns = src_defns_mode(MaybeIntDefn, MaybeImpDefn),
    IntDefns = maybe_to_list(MaybeIntDefn),
    ImpDefns = maybe_to_list(MaybeImpDefn).

    % XXX Should we move this to library/maybe.m?
:- func maybe_to_list(maybe(T)) = list(T).

maybe_to_list(no) = [].
maybe_to_list(yes(X)) = [X].

%---------------------------------------------------------------------------%

wrap_include(ModuleName) = Include :-
    Include = item_include(ModuleName, dummy_context, item_no_seq_num).

wrap_import_avail(ModuleName) = Avail :-
    ImportInfo = avail_import_info(ModuleName, dummy_context, item_no_seq_num),
    Avail = avail_import(ImportInfo).

wrap_use_avail(ModuleName) = Avail :-
    UseInfo = avail_use_info(ModuleName, dummy_context, item_no_seq_num),
    Avail = avail_use(UseInfo).

wrap_import(ModuleName) = ImportInfo :-
    ImportInfo = avail_import_info(ModuleName, dummy_context, item_no_seq_num).

wrap_use(ModuleName) = UseInfo :-
    UseInfo = avail_use_info(ModuleName, dummy_context, item_no_seq_num).

wrap_avail_import(AvailImportInfo) = avail_import(AvailImportInfo).
wrap_avail_use(AvailUseInfo) = avail_use(AvailUseInfo).

wrap_type_defn_item(X) = item_type_defn(X).
wrap_inst_defn_item(X) = item_inst_defn(X).
wrap_mode_defn_item(X) = item_mode_defn(X).
wrap_typeclass_item(X) = item_typeclass(X).
wrap_instance_item(X) = item_instance(X).
wrap_pred_decl_item(X) = item_pred_decl(X).
wrap_mode_decl_item(X) = item_mode_decl(X).
wrap_foreign_enum_item(X) = item_foreign_enum(X).
wrap_foreign_export_enum_item(X) = item_foreign_export_enum(X).
wrap_clause(X) = item_clause(X).
wrap_decl_pragma_item(X) = item_decl_pragma(X).
wrap_impl_pragma_item(X) = item_impl_pragma(X).
wrap_generated_pragma_item(X) = item_generated_pragma(X).
wrap_promise_item(X) = item_promise(X).
wrap_initialise_item(X) = item_initialise(X).
wrap_finalise_item(X) = item_finalise(X).
wrap_mutable_item(X) = item_mutable(X).
wrap_type_repn_item(X) = item_type_repn(X).

%---------------------------------------------------------------------------%

wrap_abstract_type_defn(AbstractDefnInfo) = TypeDefnInfo :-
    AbstractDefn = AbstractDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = AbstractDefnInfo ^ td_ctor_defn
        := parse_tree_abstract_type(AbstractDefn).

wrap_solver_type_defn(SolverDefnInfo) = TypeDefnInfo :-
    SolverDefn = SolverDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = SolverDefnInfo ^ td_ctor_defn
        := parse_tree_solver_type(SolverDefn).

wrap_eqv_type_defn(EqvDefnInfo) = TypeDefnInfo :-
    EqvDefn = EqvDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = EqvDefnInfo ^ td_ctor_defn
        := parse_tree_eqv_type(EqvDefn).

wrap_du_type_defn(DuDefnInfo) = TypeDefnInfo :-
    DuDefn = DuDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = DuDefnInfo ^ td_ctor_defn
        := parse_tree_du_type(DuDefn).

wrap_sub_type_defn(SubDefnInfo) = TypeDefnInfo :-
    SubDefn = SubDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = SubDefnInfo ^ td_ctor_defn
        := parse_tree_sub_type(SubDefn).

wrap_foreign_type_defn(ForeignDefnInfo) = TypeDefnInfo :-
    ForeignDefn = ForeignDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = ForeignDefnInfo ^ td_ctor_defn
        := parse_tree_foreign_type(ForeignDefn).

%---------------------%

wrap_abstract_inst_defn(AbstractDefnInfo) = InstDefnInfo :-
    InstDefnInfo = AbstractDefnInfo ^ id_inst_defn := abstract_inst_defn.

wrap_eqv_inst_defn(EqvDefnInfo) = InstDefnInfo :-
    EqvDefn = EqvDefnInfo ^ id_inst_defn,
    InstDefnInfo = EqvDefnInfo ^ id_inst_defn
        := nonabstract_inst_defn(EqvDefn).

%---------------------%

wrap_abstract_mode_defn(AbstractDefnInfo) = ModeDefnInfo :-
    ModeDefnInfo = AbstractDefnInfo ^ md_mode_defn := abstract_mode_defn.

wrap_eqv_mode_defn(EqvDefnInfo) = ModeDefnInfo :-
    EqvDefn = EqvDefnInfo ^ md_mode_defn,
    ModeDefnInfo = EqvDefnInfo ^ md_mode_defn
        := nonabstract_mode_defn(EqvDefn).

%---------------------------------------------------------------------------%
:- end_module parse_tree.item_util.
%---------------------------------------------------------------------------%
