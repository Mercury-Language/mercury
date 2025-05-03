%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: item_util.m.
%
% This module contains utility predicates for dealing with items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.convert_import_use.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module one_or_more.
:- import_module set.

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

    % classify_int_imp_import_use_modules(WarnUnsortedAvailBlocks,
    %   ModuleName, IntAvails, ImpAvails, ImportUseMap, !Specs) :-
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
    % If WarnUnsortedAvailBlocks is "yes", then generate warnings
    %
    % - when more than one import_module or use_module declaration occurs
    %   on the same line, and
    %
    % - when a block of import_module and/or use_module declarations
    %   that occur on consecutive lines is not sorted on module name.
    %
:- pred classify_int_imp_import_use_modules(bool::in, module_name::in,
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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.options.

:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more_map.
:- import_module require.
:- import_module string.
:- import_module term_context.

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

classify_int_imp_import_use_modules(WarnUnsortedAvailBlocks, ModuleName,
        IntAvails, ImpAvails, !:ImportUseMap, !Specs) :-
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
        get_ancestors(ModuleName), !ImportUseMap, !Specs),

    (
        WarnUnsortedAvailBlocks = no
    ;
        WarnUnsortedAvailBlocks = yes,
        % Processing IntAvails and ImpAvails together lets us warn
        % about the unlikely but possible case of the same line containing
        % e.g. one import_module declaration in the interface section
        % and another in the implementation section, with a ":- interface"
        % or ":- implementation" marker in between.
        warn_unsorted_avail_blocks(IntAvails ++ ImpAvails, !Specs)
    ).

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
    Spec = conditional_spec($pred, warn_simple_code, yes,
        severity_warning, phase_pt2h, [DupMsg, PrevMsg]),
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
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_pt2h, [DupMsg, PrevMsg]),
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
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_pt2h, [DupMsg, PrevMsg]),
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
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_pt2h, [DupMsg, PrevMsg]),
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
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_pt2h, [DupMsg, PrevMsg]),
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

:- pred warn_unsorted_avail_blocks(list(item_avail)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_unsorted_avail_blocks(Avails, !Specs) :-
    build_import_use_file_map(Avails, multi_map.init, FileMap),
    map.foldl(generate_unsorted_avail_block_warnings_for_file,
        FileMap, !Specs).

    % Values of this type map a file name to the list of import_module
    % and/or use_module declarations in that file.
:- type import_use_file_map == multi_map(string, import_use_line).

:- type import_use_line
    --->    import_use_line(
                % On this line in the file whose entry we are in ...
                int,

                % ... there is an import or use of this module name.
                % The next field specifies which.
                string,

                % The word "import_module" or the word "use_module".
                string

                % We want the line number field first, so that sorting
                % a list of import_use_lines makes it trivial to find
                % blocks of consecutive imports and/or uses.
            ).

:- pred build_import_use_file_map(list(item_avail)::in,
    import_use_file_map::in, import_use_file_map::out) is det.

build_import_use_file_map([], !FileMap).
build_import_use_file_map([Avail | Avails], !FileMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _)),
        AvailDecl = "import_module"
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _)),
        AvailDecl = "use_module"
    ),
    ModuleNameStr = sym_name_to_string(ModuleName),
    Context = context(FileName, LineNumber),
    ImportUseLine = import_use_line(LineNumber, ModuleNameStr, AvailDecl),
    multi_map.add(FileName, ImportUseLine, !FileMap),
    build_import_use_file_map(Avails, !FileMap).

:- pred generate_unsorted_avail_block_warnings_for_file(string::in,
    list(import_use_line)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_unsorted_avail_block_warnings_for_file(FileName, ImportUseLines,
        !Specs) :-
    list.sort(ImportUseLines, SortedImportUseLines),
    (
        SortedImportUseLines = []
        % This should not happen, but there is no point in aborting here.
    ;
        SortedImportUseLines = [FirstLine | LaterLines],
        generate_unsorted_avail_block_warnings(FileName, FirstLine,
            LaterLines, !Specs)
    ).

:- pred generate_unsorted_avail_block_warnings(string::in,
    import_use_line::in, list(import_use_line)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_unsorted_avail_block_warnings(_FileName, _, [], !Specs).
generate_unsorted_avail_block_warnings(FileName, PrevImportUseLine,
        [ImportUseLine | ImportUseLines], !Specs) :-
    PrevImportUseLine = import_use_line(PrevLineNum, PrevModuleNameStr,
        PrevAvailDecl),
    ImportUseLine = import_use_line(CurLineNum, CurModuleNameStr,
        CurAvailDecl),
    ( if CurLineNum = PrevLineNum then
        Pieces = [words("Warning: this")] ++
            color_as_subject([decl(CurAvailDecl),
                words("declaration for module"), quote(CurModuleNameStr)]) ++
            color_as_incorrect([words("is on the same line")]) ++
            [words("as the preceding"),
            decl(PrevAvailDecl), words("declaration for module"),
            quote(PrevModuleNameStr), suffix("."), nl],
        Context = context(FileName, CurLineNum),
        Spec = spec($pred, severity_warning, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else if
        CurLineNum = PrevLineNum + 1,
        module_names_are_in_order(PrevModuleNameStr, CurModuleNameStr) = no
    then
        Pieces = [words("Warning: this")] ++
            color_as_subject([decl(CurAvailDecl),
                words("declaration for module"), quote(CurModuleNameStr)]) ++
            color_as_incorrect([words("is out of order")]) ++
            [words("with respect to the preceding"),
            decl(PrevAvailDecl), words("declaration for module"),
            quote(PrevModuleNameStr), suffix("."), nl],
        Context = context(FileName, CurLineNum),
        Spec = spec($pred, severity_warning, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    generate_unsorted_avail_block_warnings(FileName,
        ImportUseLine, ImportUseLines, !Specs).

:- func module_names_are_in_order(string, string) = bool.

module_names_are_in_order(PrevModuleNameStr, CurModuleNameStr) = InOrder :-
    compare(CmpResult, PrevModuleNameStr, CurModuleNameStr),
    (
        CmpResult = (<),
        % The two module names are in the excepted ascending order.
        InOrder = yes
    ;
        CmpResult = (=),
        % The code that looks for duplicate imports and/or uses, which
        % also handles the case where the duplicate appearances are *not*
        % next to each other, will generate a warning for this case.
        % We don't want to generate two warnings for the same problem.
        InOrder = yes
    ;
        CmpResult = (>),
        string.to_char_list(PrevModuleNameStr, PrevChars),
        string.to_char_list(CurModuleNameStr, CurChars),
        % We want to allow import sequences such as
        %
        %   :- import_module int8.
        %   :- import_module int16.
        %   :- import_module int32.
        %   :- import_module int64.
        %
        % even though lexicographically, int16 < int8. So we consider
        % two module names to be in order if
        %
        % - they consist of nothing but digits starting at the point
        %   of their first difference, and
        %
        % - the numbers that those digits represent are in order.
        InOrder = module_names_are_in_numerical_order(PrevChars, CurChars)
    ).

:- func module_names_are_in_numerical_order(list(char), list(char)) = bool.

module_names_are_in_numerical_order([], []) = _ :-
    unexpected($pred, "CmpResult is >, but char lists are identical").
module_names_are_in_numerical_order([], [_ | _]) = no.
module_names_are_in_numerical_order([_ | _], []) = no.
module_names_are_in_numerical_order([PrevChar | PrevChars],
        [CurChar | CurChars]) = InOrder :-
    ( if PrevChar = CurChar then
        InOrder = module_names_are_in_numerical_order(PrevChars, CurChars)
    else
        string.from_char_list([PrevChar | PrevChars], PrevSuffix),
        string.from_char_list([CurChar | CurChars], CurSuffix),
        ( if
            string.to_int(PrevSuffix, PrevNum),
            string.to_int(CurSuffix, CurNum),
            PrevNum < CurNum
        then
            InOrder = yes
        else
            InOrder = no
        )
    ).

%---------------------------------------------------------------------------%

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
:- end_module parse_tree.convert_import_use.
%---------------------------------------------------------------------------%
