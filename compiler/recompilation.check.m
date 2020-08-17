%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: recompilation_check.m.
% Main author: stayl.
%
% Check whether a module should be recompiled.
%
%-----------------------------------------------------------------------------%

:- module recompilation.check.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.read_modules.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type modules_to_recompile
    --->    all_modules
    ;       some_modules(list(module_name)).

:- type find_target_file_names == pred(module_name, list(file_name), io, io).
:- inst find_target_file_names == (pred(in, out, di, uo) is det).

:- type find_timestamp_file_names ==
    pred(module_name, list(file_name), io, io).
:- inst find_timestamp_file_names ==
   (pred(in, out, di, uo) is det).

    % should_recompile(Globals, ModuleName, FindTargetFiles,
    %   FindTimestampFiles, ModulesToRecompile, HaveReadModuleMaps)
    %
    % Process the `.used'  files for the given module and all its
    % inline submodules to find out which modules need to be recompiled.
    % `FindTargetFiles' takes a module name and returns a list of
    % file names which need to be up-to-date to avoid recompilation.
    % `FindTimestampFiles' takes a module name and returns a list of
    % file names which should be touched if the module does not need
    % to be recompiled.
    % `ReadModules' is the list of interface files read during
    % recompilation checking, returned to avoid rereading them
    % if recompilation is required.
    %
:- pred should_recompile(globals::in, module_name::in,
    find_target_file_names::in(find_target_file_names),
    find_timestamp_file_names::in(find_timestamp_file_names),
    modules_to_recompile::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_cons.    % for type field_access_type
:- import_module hlds.hlds_pred.    % for field_access_function_name,
                                    % type pred_id.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.item_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module recompilation.usage.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module parser.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module univ.

%-----------------------------------------------------------------------------%

should_recompile(Globals, ModuleName, FindTargetFiles, FindTimestampFiles,
        ModulesToRecompile, HaveReadModuleMaps0, HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, find_all_recompilation_reasons,
        FindAll),
    Info0 = recompilation_check_info(ModuleName, no, [], HaveReadModuleMaps0,
        init_item_id_set(map.init, map.init, map.init),
        set.init, some_modules([]), FindAll, []),
    should_recompile_2(Globals, no, FindTargetFiles, FindTimestampFiles,
        ModuleName, Info0, Info, !IO),
    ModulesToRecompile = Info ^ rci_modules_to_recompile,
    HaveReadModuleMaps = Info ^ rci_have_read_module_maps.

:- pred should_recompile_2(globals::in, bool::in,
    find_target_file_names::in(find_target_file_names),
    find_timestamp_file_names::in(find_timestamp_file_names), module_name::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_recompile_2(Globals, IsSubModule, FindTargetFiles, FindTimestampFiles,
        ModuleName, !Info, !IO) :-
    !Info ^ rci_module_name := ModuleName,
    !Info ^ rci_sub_modules := [],
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".used")), ModuleName, UsageFileName, !IO),
    io.open_input(UsageFileName, MaybeVersionStream, !IO),
    (
        MaybeVersionStream = ok(VersionStream),
        promise_equivalent_solutions [Result, !:IO] (
            should_recompile_3_try(VersionStream, Globals, IsSubModule,
                FindTimestampFiles,
            !.Info, Result, !IO)
        ),
        (
            Result = succeeded(!:Info),
            Reasons = !.Info ^ rci_recompilation_reasons
        ;
            Result = failed,
            unexpected($pred, "try failed")
        ;
            Result = exception(Exception),
            ( if univ_to_type(Exception, RecompileException0) then
                RecompileException = RecompileException0
            else
                rethrow(Result)
            ),
            RecompileException = recompile_exception(Reason, !:Info),
            Reasons = [Reason]
        ),
        (
            Reasons = [],
            FindTimestampFiles(ModuleName, TimestampFiles, !IO),
            write_recompilation_message(Globals,
                write_not_recompiling_message(ModuleName), !IO),
            list.foldl(touch_datestamp(Globals), TimestampFiles, !IO)
        ;
            Reasons = [_ | _],
            add_module_to_recompile(ModuleName, !Info),
            write_recompilation_message(Globals,
                write_reasons_message(Globals, ModuleName,
                    list.reverse(Reasons)),
                !IO)
        ),
        io.close_input(VersionStream, !IO),

        ModulesToRecompile = !.Info ^ rci_modules_to_recompile,
        (
            ModulesToRecompile = all_modules
        ;
            ModulesToRecompile = some_modules(_),
            !Info ^ rci_is_inline_sub_module := yes,
            list.foldl2(
                should_recompile_2(Globals, yes, FindTargetFiles,
                    FindTimestampFiles),
                !.Info ^ rci_sub_modules, !Info, !IO)
        )
    ;
        MaybeVersionStream = error(_),
        write_recompilation_message(Globals,
            write_not_found_reasons_message(Globals, UsageFileName,
                ModuleName),
            !IO),
        !Info ^ rci_modules_to_recompile := all_modules
    ).

:- pred write_not_recompiling_message(module_name::in, io::di, io::uo) is det.

write_not_recompiling_message(ModuleName, !IO) :-
    io.write_string("Not recompiling module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

:- pred write_reasons_message(globals::in, module_name::in,
    list(recompile_reason)::in, io::di, io::uo) is det.

write_reasons_message(Globals, ModuleName, Reasons, !IO) :-
    list.foldl(write_recompile_reason(Globals, ModuleName), Reasons, !IO).

:- pred write_not_found_reasons_message(globals::in, string::in,
    module_name::in, io::di, io::uo) is det.

write_not_found_reasons_message(Globals, UsageFileName, ModuleName, !IO) :-
    Reason = recompile_for_file_error(UsageFileName,
        [words("file"), quote(UsageFileName), words("not found."), nl]),
    write_recompile_reason(Globals, ModuleName, Reason, !IO).

:- pred should_recompile_3_try(io.text_input_stream::in, globals::in, bool::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    recompilation_check_info::in,
    exception_result(recompilation_check_info)::out,
    io::di, io::uo) is cc_multi.

should_recompile_3_try(VersionStream, Globals, IsSubModule, FindTargetFiles,
        Info, Result, !IO) :-
    try_io(
        should_recompile_3(VersionStream, Globals, IsSubModule,
            FindTargetFiles, Info),
        Result, !IO).

:- pred should_recompile_3(io.text_input_stream::in, globals::in, bool::in,
    find_target_file_names::in(find_target_file_names),
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_recompile_3(VersionStream, Globals, IsSubModule, FindTargetFiles,
        !Info, !IO) :-
    % WARNING: any exceptions thrown before the sub_modules field is set
    % in the recompilation_check_info must set the modules_to_recompile field
    % to `all', or else the nested submodules will not be checked
    % and necessary recompilations may be missed.

    % Check that the format of the usage file is the current format.
    read_term_check_for_error_or_eof(VersionStream, !.Info,
        "usage file version number", VersionNumberTerm, !IO),
    ( if
        % XXX ITEM_LIST This term should be more self-descriptive.
        % Instead of the current "2,1.", it should be something like
        % "mercury_smart_recomp_usage(usage_format(2), version_format(1))".
        % We could initially accept both formats when reading in,
        % while generating the new format only.
        VersionNumberTerm = term.functor(term.atom(","),
            [UsageFileVersionNumberTerm,
            VersionNumbersVersionNumberTerm], _),
        decimal_term_to_int(UsageFileVersionNumberTerm,
            usage_file_version_number),
        decimal_term_to_int(VersionNumbersVersionNumberTerm,
            version_numbers_version_number)
    then
        true
    else
        io.input_stream_name(UsageFileName, !IO),
        Reason = recompile_for_file_error(UsageFileName,
            [words("invalid usage file version number in file"),
            quote(UsageFileName), suffix("."), nl]),
        throw_syntax_error(Reason, !.Info)
    ),

    % Find the timestamp of the module the last time it was compiled.
    read_term_check_for_error_or_eof(VersionStream, !.Info, "module timestamp",
        TimestampTerm, !IO),
    parse_module_timestamp(!.Info, TimestampTerm, _, ModuleTimestamp),
    ModuleTimestamp = module_timestamp(_, RecordedTimestamp, _),

    (
        IsSubModule = yes
        % For inline submodules we don't need to check the module timestamp
        % because we have already checked the timestamp for the parent module.
    ;
        IsSubModule = no,
        % If the module has changed, recompile.
        ModuleName = !.Info ^ rci_module_name,
        read_module_src(Globals, "Reading module",
            do_not_ignore_errors, do_search, ModuleName, [], FileName,
            dont_read_module_if_match(RecordedTimestamp), MaybeNewTimestamp,
            ParseTree, Specs, Errors, !IO),
        ( if
            MaybeNewTimestamp = yes(NewTimestamp),
            NewTimestamp \= RecordedTimestamp
        then
            record_read_file_src(ModuleName, FileName,
                ModuleTimestamp ^ mts_timestamp := NewTimestamp,
                ParseTree, Specs, Errors, !Info),
            !Info ^ rci_modules_to_recompile := all_modules,
            record_recompilation_reason(recompile_for_module_changed(FileName),
                !Info)
        else if
            ( set.is_non_empty(Errors)
            ; MaybeNewTimestamp = no
            )
        then
            % We are throwing away Specs, even though some of its elements
            % could illuminate the cause of the problem. XXX Is this OK?
            Pieces = [words("error reading file"), quote(FileName),
                suffix("."), nl],
            Reason = recompile_for_file_error(FileName, Pieces),
            % XXX Some of the errors in Errors could be errors other than
            % syntax errors.
            throw_syntax_error(Reason, !.Info)
        else
            % We are throwing away Specs. Since it should be a repeat of the
            % errors we saw when the file was first read in, this should be OK.
            true
        )
    ),

    % Find out whether this module has any inline submodules.
    read_term_check_for_error_or_eof(VersionStream, !.Info,
        "inline sub-modules", SubModulesTerm, !IO),
    ( if
        SubModulesTerm = term.functor(term.atom("sub_modules"),
            SubModuleTerms, _),
        list.map(try_parse_sym_name_and_no_args, SubModuleTerms, SubModules)
    then
        !Info ^ rci_sub_modules := SubModules
    else
        Reason1 = recompile_for_syntax_error(get_term_context(SubModulesTerm),
            "error in sub_modules term"),
        throw_syntax_error(Reason1, !.Info)
    ),

    % Check whether the output files are present and up-to-date.
    FindTargetFiles(!.Info ^ rci_module_name, TargetFiles, !IO),
    list.foldl2(require_recompilation_if_not_up_to_date(RecordedTimestamp),
        TargetFiles, !Info, !IO),

    % Read in the used items, used for checking for ambiguities with new items.
    read_term_check_for_error_or_eof(VersionStream, !.Info, "used items",
        UsedItemsTerm, !IO),
    parse_used_items(!.Info, UsedItemsTerm, UsedItems),
    !Info ^ rci_used_items := UsedItems,

    read_term_check_for_error_or_eof(VersionStream, !.Info, "used classes",
        UsedClassesTerm, !IO),
    ( if
        UsedClassesTerm = term.functor(term.atom("used_classes"),
            UsedClassTerms, _),
        list.map(parse_name_and_arity_to_used, UsedClassTerms, UsedClasses)
    then
        !Info ^ rci_used_typeclasses := set.list_to_set(UsedClasses)
    else
        Reason3 = recompile_for_syntax_error(get_term_context(UsedClassesTerm),
            "error in used_typeclasses term"),
        throw_syntax_error(Reason3, !.Info)
    ),
    check_imported_modules(VersionStream, Globals, !Info, !IO).

:- pred require_recompilation_if_not_up_to_date(timestamp::in, file_name::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

require_recompilation_if_not_up_to_date(RecordedTimestamp, TargetFile,
        !Info, !IO) :-
    io.file_modification_time(TargetFile, TargetModTimeResult, !IO),
    ( if
        TargetModTimeResult = ok(TargetModTime),
        compare(TargetModTimeCompare, time_t_to_timestamp(TargetModTime),
            RecordedTimestamp),
        TargetModTimeCompare = (>)
    then
        true
    else
        Reason = recompile_for_output_file_not_up_to_date(TargetFile),
        record_recompilation_reason(Reason, !Info)
    ).

:- pred parse_name_and_arity_to_used(term::in, item_name::out) is semidet.

parse_name_and_arity_to_used(Term, UsedClass) :-
    parse_unqualified_name_and_arity(Term, ClassName, ClassArity),
    UsedClass = item_name(ClassName, ClassArity).

%-----------------------------------------------------------------------------%

:- pred parse_module_timestamp(recompilation_check_info::in, term::in,
    module_name::out, module_timestamp::out) is det.

parse_module_timestamp(Info, Term, ModuleName, ModuleTimestamp) :-
    conjunction_to_list(Term, Args),
    ( if
        Args = [ModuleNameTerm, SuffixTerm, TimestampTerm | MaybeOtherTerms],
        try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleNamePrime),
        SuffixTerm = term.functor(term.string(SuffixStr), [], _),
        extension_to_file_kind(SuffixStr, FileKind),
        Timestamp = term_to_timestamp(TimestampTerm),
        % This must be kept in sync with write_module_name_and_used_items
        % in recompilation.usage.m.
        (
            MaybeOtherTerms = [],
            RecompAvail = recomp_avail_int_import
        ;
            MaybeOtherTerms = [term.functor(term.atom(Other), [], _)],
            (
                Other = "src",
                RecompAvail = recomp_avail_src
            ;
                ( Other = "used"
                ; Other = "int_used"
                ),
                RecompAvail = recomp_avail_int_use
            ;
                Other = "imp_used",
                RecompAvail = recomp_avail_imp_use
            ;
                Other = "int_imported",
                RecompAvail = recomp_avail_int_import
            ;
                Other = "imp_imported",
                RecompAvail = recomp_avail_imp_import
            ;
                Other = "int_used_imp_imported",
                RecompAvail = recomp_avail_int_use_imp_import
            )
        )
    then
        ModuleName = ModuleNamePrime,
        ModuleTimestamp = module_timestamp(FileKind, Timestamp, RecompAvail)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in module timestamp"),
        throw_syntax_error(Reason, Info)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_used_items(recompilation_check_info::in,
    term::in, resolved_used_items::out) is det.

parse_used_items(Info, Term, UsedItems) :-
    ( if Term = term.functor(term.atom("used_items"), UsedItemTerms, _) then
        list.foldl(parse_used_item_set(Info), UsedItemTerms,
            init_item_id_set(map.init, map.init, map.init), UsedItems)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in used items"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_used_item_set(recompilation_check_info::in, term::in,
    resolved_used_items::in, resolved_used_items::out) is det.

parse_used_item_set(Info, Term, UsedItems0, UsedItems) :-
    ( if
        Term = term.functor(term.atom(ItemTypeStr), ItemTerms, _),
        string_to_item_type(ItemTypeStr, ItemType)
    then
        ( if is_simple_item_type(ItemType) then
            list.foldl(parse_simple_item(Info), ItemTerms,
                map.init, SimpleItems),
            update_simple_item_set(ItemType, SimpleItems,
                UsedItems0, UsedItems)
        else if is_pred_or_func_item_type(ItemType) then
            list.foldl(parse_pred_or_func_item(Info),
                ItemTerms, map.init, PredOrFuncItems),
            update_pred_or_func_set(ItemType, PredOrFuncItems,
                UsedItems0, UsedItems)
        else if ItemType = functor_item then
            list.foldl(parse_functor_item(Info),
                ItemTerms, map.init, CtorItems),
            UsedItems = UsedItems0 ^ functors := CtorItems
        else
            Reason = recompile_for_syntax_error(get_term_context(Term),
                "error in used items: unknown item type: " ++ ItemTypeStr),
            throw_syntax_error(Reason, Info)
        )
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in used items"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_simple_item(recompilation_check_info::in, term::in,
    simple_item_set::in, simple_item_set::out) is det.

parse_simple_item(Info, Term, !Set) :-
    ( if
        Term = term.functor(term.atom("-"), [NameArityTerm, MatchesTerm], _),
        parse_unqualified_name_and_arity(NameArityTerm, SymName, Arity)
    then
        Name = unqualify_name(SymName),
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.foldl(parse_simple_item_match(Info), MatchTermList,
            map.init, Matches),
        map.det_insert(Name - Arity, Matches, !Set)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in simple items"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_simple_item_match(recompilation_check_info::in, term::in,
    map(module_qualifier, module_name)::in,
    map(module_qualifier, module_name)::out) is det.

parse_simple_item_match(Info, Term, !Items) :-
    ( if
        ( if
            Term = term.functor(term.atom("=>"),
                [QualifierTerm, ModuleNameTerm], _)
        then
            try_parse_sym_name_and_no_args(QualifierTerm, Qualifier),
            try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName)
        else
            try_parse_sym_name_and_no_args(Term, ModuleName),
            Qualifier = ModuleName
        )
    then
        map.det_insert(Qualifier, ModuleName, !Items)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in simple item match"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_pred_or_func_item(recompilation_check_info::in, term::in,
    resolved_pred_or_func_set::in, resolved_pred_or_func_set::out) is det.

parse_pred_or_func_item(Info, Term, !Set) :-
    parse_resolved_item_set(Info, parse_pred_or_func_item_match, Term, !Set).

:- pred parse_pred_or_func_item_match(recompilation_check_info::in, term::in,
    resolved_pred_or_func_map::in, resolved_pred_or_func_map::out) is det.

parse_pred_or_func_item_match(Info, Term, !Items) :-
    PredId = invalid_pred_id,
    ( if
        ( if
            Term = term.functor(term.atom("=>"),
                [QualifierTerm, MatchesTerm], _)
        then
            try_parse_sym_name_and_no_args(QualifierTerm, Qualifier),
            conjunction_to_list(MatchesTerm, MatchesList),
            list.map(
                ( pred(MatchTerm::in, Match::out) is semidet :-
                    try_parse_sym_name_and_no_args(MatchTerm, MatchName),
                    Match = PredId - MatchName
                ),
                MatchesList, Matches)
        else
            try_parse_sym_name_and_no_args(Term, Qualifier),
            Matches = [PredId - Qualifier]
        )
    then
        map.det_insert(Qualifier, set.list_to_set(Matches), !Items)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in pred or func match"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_functor_item(recompilation_check_info::in, term::in,
    resolved_functor_set::in, resolved_functor_set::out) is det.

parse_functor_item(Info, Term, !Set) :-
    parse_resolved_item_set(Info, parse_functor_matches, Term, !Set).

:- pred parse_functor_matches(recompilation_check_info::in, term::in,
    resolved_functor_map::in, resolved_functor_map::out) is det.

parse_functor_matches(Info, Term, !Map) :-
    ( if
        Term = term.functor(term.atom("=>"),
            [QualifierTerm, MatchesTerm], _),
        try_parse_sym_name_and_no_args(QualifierTerm, Qualifier)
    then
        conjunction_to_list(MatchesTerm, MatchesList),
        list.map(parse_resolved_functor(Info), MatchesList, Matches),
        map.det_insert(Qualifier, set.list_to_set(Matches), !Map)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in functor match"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_resolved_functor(recompilation_check_info::in, term::in,
    resolved_functor::out) is det.

parse_resolved_functor(Info, Term, Ctor) :-
    ( if
        Term = term.functor(term.atom(PredOrFuncStr),
            [ModuleTerm, ArityTerm], _),
        ( PredOrFuncStr = "predicate", PredOrFunc = pf_predicate
        ; PredOrFuncStr = "function", PredOrFunc = pf_function
        ),
        try_parse_sym_name_and_no_args(ModuleTerm, ModuleName),
        decimal_term_to_int(ArityTerm, Arity)
    then
        PredId = invalid_pred_id,
        Ctor = resolved_functor_pred_or_func(PredId, ModuleName, PredOrFunc,
            Arity)
    else if
        Term = term.functor(term.atom("ctor"), [NameArityTerm], _),
        parse_unqualified_name_and_arity(NameArityTerm, TypeName, TypeArity)
    then
        Ctor = resolved_functor_constructor(item_name(TypeName, TypeArity))
    else if
        Term = term.functor(term.atom("field"),
            [TypeNameArityTerm, ConsNameArityTerm], _),
        parse_unqualified_name_and_arity(TypeNameArityTerm,
            TypeName, TypeArity),
        parse_unqualified_name_and_arity(ConsNameArityTerm,
            ConsName, ConsArity)
    then
        Ctor = resolved_functor_field(item_name(TypeName, TypeArity),
            item_name(ConsName, ConsArity))
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in functor match"),
        throw_syntax_error(Reason, Info)
    ).

:- type parse_resolved_item_matches(T) ==
    pred(recompilation_check_info, term,
        resolved_item_map(T), resolved_item_map(T)).
:- inst parse_resolved_item_matches == (pred(in, in, in, out) is det).

:- pred parse_resolved_item_set(recompilation_check_info::in,
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
    term::in, resolved_item_set(T)::in, resolved_item_set(T)::out) is det.

parse_resolved_item_set(Info, ParseMatches, Term, !Set) :-
    ( if
        Term = term.functor(term.atom("-"), [NameTerm, MatchesTerm], _),
        NameTerm = term.functor(term.atom(Name), [], _)
    then
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.map(parse_resolved_item_arity_matches(Info, ParseMatches),
            MatchTermList, Matches),
        map.det_insert(Name, Matches, !Set)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in resolved item matches"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_resolved_item_arity_matches(recompilation_check_info::in,
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
    term::in, pair(arity, resolved_item_map(T))::out) is det.

parse_resolved_item_arity_matches(Info, ParseMatches, Term,
        Arity - MatchMap) :-
    ( if
        Term = term.functor(term.atom("-"), [ArityTerm, MatchesTerm], _),
        decimal_term_to_int(ArityTerm, Arity0),
        conjunction_to_list(MatchesTerm, MatchTermList)
    then
        Arity = Arity0,
        list.foldl(
            ( pred(MatchTerm::in, Map0::in, Map::out) is det :-
                ParseMatches(Info, MatchTerm, Map0, Map)
            ),
            MatchTermList, map.init, MatchMap)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in resolved item matches"),
        throw_syntax_error(Reason, Info)
    ).

%-----------------------------------------------------------------------------%

    % Check whether the interface file read for a module in the last
    % compilation has changed, and if so whether the items have changed
    % in a way which should cause a recompilation.
    %
:- pred check_imported_modules(io.text_input_stream::in, globals::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_modules(VersionStream, Globals, !Info, !IO) :-
    parser.read_term(VersionStream, TermResult, !IO),
    (
        TermResult = term(_, Term),
        ( if Term = term.functor(term.atom("done"), [], _) then
            true
        else
            check_imported_module(Globals, Term, !Info, !IO),
            check_imported_modules(VersionStream, Globals, !Info, !IO)
        )
    ;
        TermResult = error(Message, Line),
        io.input_stream_name(VersionStream, FileName, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            Message),
        throw_syntax_error(Reason, !.Info)
    ;
        TermResult = eof,
        % There should always be an item `done.' at the end of the list
        % of modules to check. This is used to make sure that the writing
        % of the `.used' file was not interrupted.
        io.input_stream_name(VersionStream, FileName, !IO),
        io.get_line_number(VersionStream, Line, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            "unexpected end of file"),
        throw_syntax_error(Reason, !.Info)
    ).

:- pred check_imported_module(globals::in, term::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_module(Globals, Term, !Info, !IO) :-
    ( if
        Term = term.functor(term.atom("=>"),
            [TimestampTerm0, UsedItemsTerm0], _)
    then
        TimestampTerm = TimestampTerm0,
        MaybeUsedItemsTerm = yes(UsedItemsTerm0)
    else
        TimestampTerm = Term,
        MaybeUsedItemsTerm = no
    ),
    parse_module_timestamp(!.Info, TimestampTerm,
        ImportedModuleName, ModuleTimestamp),

    ModuleTimestamp =
        module_timestamp(FileKind, RecordedTimestamp, RecompAvail),
    (
        FileKind = fk_int(IntFileKind)
    ;
        FileKind = fk_src,
        unexpected($pred, "fk_src")
    ;
        FileKind = fk_opt(_),
        unexpected($pred, "fk_opt")
    ),
    HaveReadModuleMaps = !.Info ^ rci_have_read_module_maps,
    HaveReadModuleMapInt = HaveReadModuleMaps ^ hrmm_int,
    ( if
        % If we are checking a submodule, don't re-read interface files
        % read for other modules checked during this compilation.
        !.Info ^ rci_is_inline_sub_module = yes,
        IntKey = have_read_module_key(ImportedModuleName, IntFileKind),
        find_read_module_int(HaveReadModuleMapInt, IntKey,
            do_return_timestamp, FileNamePrime, MaybeNewTimestampPrime,
            ParseTreeIntPrime, SpecsPrime, ErrorsPrime)
    then
        FileName = FileNamePrime,
        MaybeNewTimestamp = MaybeNewTimestampPrime,
        ParseTreeInt = ParseTreeIntPrime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime,
        Recorded = bool.yes
    else
        Recorded = bool.no,
        read_module_int(Globals, "Reading interface file for module",
            do_not_ignore_errors, do_search,
            ImportedModuleName, IntFileKind, FileName,
            dont_read_module_if_match(RecordedTimestamp), MaybeNewTimestamp,
            ParseTreeInt, Specs, Errors, !IO)
    ),
    ( if set.is_empty(Errors) then
        ( if
            MaybeNewTimestamp = yes(NewTimestamp),
            NewTimestamp \= RecordedTimestamp
        then
            (
                Recorded = no,
                record_read_file_int(ImportedModuleName, IntFileKind, FileName,
                    ModuleTimestamp ^ mts_timestamp := NewTimestamp,
                    ParseTreeInt, Specs, Errors, !Info)
            ;
                Recorded = yes
            ),
            ( if
                MaybeUsedItemsTerm = yes(UsedItemsTerm),
                ParseTreeInt = parse_tree_int(ParseTreeModuleName, _, _,
                    MaybeVersionNumbers, IntIncls, ImpIncls,
                    IntAvails, ImpAvails, IntFIMs, ImpFIMs,
                    IntItems, ImplItems),
                MaybeVersionNumbers = version_numbers(VersionNumbers)
            then
                int_imp_items_to_item_blocks(ParseTreeModuleName,
                    ms_interface, ms_implementation,
                    IntIncls, ImpIncls, IntAvails, ImpAvails,
                    IntFIMs, ImpFIMs, IntItems, ImplItems, RawItemBlocks),
                check_module_used_items(ImportedModuleName, RecompAvail,
                    RecordedTimestamp, UsedItemsTerm, VersionNumbers,
                    RawItemBlocks, !Info)
            else
                record_recompilation_reason(
                    recompile_for_module_changed(FileName), !Info)
            )
        else
            % We are throwing away Specs. Since it should be a repeat of the
            % errors we saw when the file was first read in, this should be OK.
            true
        )
    else
        % We are throwing away Specs, even though some of its elements
        % could illuminate the cause of the problem. XXX Is this OK?
        Pieces = [words("error reading file"), quote(FileName), suffix("."),
            nl],
        throw_syntax_error(recompile_for_file_error(FileName, Pieces), !.Info)
    ).

:- pred check_module_used_items(module_name::in, recomp_avail::in,
    timestamp::in, term::in, version_numbers::in, list(raw_item_block)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_module_used_items(ModuleName, RecompAvail, OldTimestamp,
        UsedItemsTerm, NewVersionNumbers, RawItemBlocks, !Info) :-
    parse_version_numbers(UsedItemsTerm, UsedItemsResult),
    (
        UsedItemsResult = ok1(UsedVersionNumbers)
    ;
        UsedItemsResult = error1(Specs),
        (
            Specs = [],
            unexpected($pred, "error1([])")
        ;
            Specs = [_ | _],
            Reason = recompile_for_unreadable_used_items(Specs),
            throw_syntax_error(Reason, !.Info)
        )
    ),

    UsedVersionNumbers = version_numbers(UsedItemVersionNumbers,
        UsedInstanceVersionNumbers),
    NewVersionNumbers = version_numbers(NewItemVersionNumbers,
        NewInstanceVersionNumbers),

    % Check whether any of the items which were used have changed.
    list.foldl(
        check_item_version_numbers(ModuleName, UsedItemVersionNumbers,
            NewItemVersionNumbers),
        [type_abstract_item, type_body_item, inst_item, mode_item,
            typeclass_item, predicate_item, function_item], !Info),

    % Check whether added or modified items could cause name resolution
    % ambiguities with items which were used.
    list.foldl(
        check_raw_item_block_for_ambiguities(RecompAvail,
            OldTimestamp, UsedItemVersionNumbers),
        RawItemBlocks, !Info),

    % Check whether any instances of used typeclasses have been added,
    % removed or changed.
    check_instance_version_numbers(ModuleName, UsedInstanceVersionNumbers,
        NewInstanceVersionNumbers, !Info),

    % Check for new instances for used typeclasses.
    ModuleInstances = set.sorted_list_to_set(
        map.sorted_keys(NewInstanceVersionNumbers)),
    UsedInstances = set.sorted_list_to_set(
        map.sorted_keys(UsedInstanceVersionNumbers)),

    UsedClasses = !.Info ^ rci_used_typeclasses,
    set.difference(set.intersect(UsedClasses, ModuleInstances),
        UsedInstances, AddedInstances),
    AddedInstancesList = set.to_sorted_list(AddedInstances),
    (
        AddedInstancesList = []
    ;
        AddedInstancesList = [FirstAddedInstance | _],
        Reason1 = recompile_for_changed_or_added_instance(ModuleName,
            FirstAddedInstance),
        record_recompilation_reason(Reason1, !Info)
    ).

:- func make_item_id(module_name, item_type, pair(string, arity)) = item_id.

make_item_id(Module, ItemType, Name - Arity) =
    item_id(ItemType, item_name(qualified(Module, Name), Arity)).

%-----------------------------------------------------------------------------%

:- pred check_item_version_numbers(module_name::in, item_version_numbers::in,
    item_version_numbers::in, item_type::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_version_numbers(ModuleName, UsedVersionNumbers, NewVersionNumbers,
        ItemType, !Info) :-
    NewItemTypeVersionNumbers = extract_ids(NewVersionNumbers, ItemType),
    map.foldl(
        check_item_version_number(ModuleName,
            NewItemTypeVersionNumbers, ItemType),
        extract_ids(UsedVersionNumbers, ItemType), !Info).

:- pred check_item_version_number(module_name::in, version_number_map::in,
    item_type::in, pair(string, arity)::in, version_number::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_version_number(ModuleName, NewItemTypeVersionNumbers, ItemType,
        NameArity, UsedVersionNumber, !Info) :-
    ( if
        map.search(NewItemTypeVersionNumbers, NameArity, NewVersionNumber)
    then
        ( if NewVersionNumber = UsedVersionNumber then
            true
        else
            ItemId = make_item_id(ModuleName, ItemType, NameArity),
            Reason = recompile_for_changed_item(ItemId),
            record_recompilation_reason(Reason, !Info)
        )
    else
        ItemId = make_item_id(ModuleName, ItemType, NameArity),
        Reason = recompile_for_removed_item(ItemId),
        record_recompilation_reason(Reason, !Info)
    ).

:- pred check_instance_version_numbers(module_name::in,
    instance_version_numbers::in, instance_version_numbers::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_instance_version_numbers(ModuleName, UsedInstanceVersionNumbers,
        NewInstanceVersionNumbers, !Info) :-
    map.foldl(check_instance_version_number(ModuleName,
        NewInstanceVersionNumbers), UsedInstanceVersionNumbers, !Info).

:- pred check_instance_version_number(module_name::in,
    instance_version_numbers::in, item_name::in, version_number::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_instance_version_number(ModuleName, NewInstanceVersionNumbers,
        ClassId, UsedVersionNumber, !Info) :-
    ( if map.search(NewInstanceVersionNumbers, ClassId, NewVersionNumber) then
        ( if UsedVersionNumber = NewVersionNumber then
            true
        else
            Reason = recompile_for_changed_or_added_instance(ModuleName,
                ClassId),
            record_recompilation_reason(Reason, !Info)
        )
    else
        Reason = recompile_for_removed_instance(ModuleName, ClassId),
        record_recompilation_reason(Reason, !Info)
    ).

%-----------------------------------------------------------------------------%

    % For each item which has changed since the last time we read the interface
    % file, check whether it introduces ambiguities with items which were used
    % when the current module was last compiled.
    %
:- pred check_raw_item_block_for_ambiguities(recomp_avail::in,
    timestamp::in, item_version_numbers::in, raw_item_block::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_raw_item_block_for_ambiguities(RecompAvail, OldTimestamp,
        VersionNumbers, RawItemBlock, !Info) :-
    RawItemBlock = item_block(_, _, _Incls, _Avails, _FIMs, Items),
    list.foldl(
        check_item_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers),
        Items, !Info).

:- pred check_item_for_ambiguities(recomp_avail::in, timestamp::in,
    item_version_numbers::in, item::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers, Item,
        !Info) :-
    (
        Item = item_clause(_),
        unexpected($pred, "clause")
    ;
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams, TypeBody,
            _, _, _),
        list.length(TypeParams, TypeArity),
        check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
            VersionNumbers, type_abstract_item, TypeSymName, TypeArity,
            NeedsCheck, !Info),
        (
            NeedsCheck = yes,
            check_type_defn_ambiguity_with_functor(RecompAvail,
                type_ctor(TypeSymName, TypeArity), TypeBody, !Info)
        ;
            NeedsCheck = no
        )
    ;
        Item = item_inst_defn(ItemInstDefn),
        % XXX IFTC Do we need to check _MaybeForTypeCtor?
        ItemInstDefn = item_inst_defn_info(InstSymName, InstParams,
            _MaybeForTypeCtor, _, _, _, _),
        list.length(InstParams, InstArity),
        check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
            VersionNumbers, inst_item, InstSymName, InstArity, _, !Info)
    ;
        Item = item_mode_defn(ItemModeDefn),
        ItemModeDefn = item_mode_defn_info(ModeSymName, ModeParams,
            _, _, _, _),
        list.length(ModeParams, ModeArity),
        check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
            VersionNumbers, mode_item, ModeSymName, ModeArity, _, !Info)
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(TypeClassSymName, TypeClassParams,
            _, _, Interface, _, _, _),
        list.length(TypeClassParams, TypeClassArity),
        check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
            VersionNumbers, typeclass_item, TypeClassSymName, TypeClassArity,
            NeedsCheck, !Info),
        ( if
            NeedsCheck = yes,
            Interface = class_interface_concrete(ClassDecls)
        then
            list.foldl(
                check_class_decl_for_ambiguities(RecompAvail,
                    OldTimestamp, VersionNumbers),
                ClassDecls, !Info)
        else
            true
        )
    ;
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc, Args,
            WithType, _, _, _, _, _, _, _, _, _, _),
        check_for_pred_or_func_item_ambiguity(no, RecompAvail, OldTimestamp,
            VersionNumbers, PredOrFunc, PredSymName, Args, WithType, !Info)
    ;
        ( Item = item_mode_decl(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_impl_pragma(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_type_repn(_)
        )
    ).

:- pred check_class_decl_for_ambiguities(recomp_avail::in,
    timestamp::in, item_version_numbers::in, class_decl::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_class_decl_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        Decl, !Info) :-
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(MethodName, PredOrFunc,
            MethodArgs, MethodWithType, _, _, _, _, _, _, _, _),
        check_for_pred_or_func_item_ambiguity(yes, RecompAvail,
            OldTimestamp, VersionNumbers, PredOrFunc, MethodName,
            MethodArgs, MethodWithType, !Info)
    ;
        Decl = class_decl_mode(_)
    ).

:- pred item_is_new_or_changed(timestamp::in, item_version_numbers::in,
    item_type::in, sym_name::in, arity::in) is semidet.

item_is_new_or_changed(UsedFileTimestamp, UsedVersionNumbers,
        ItemType, SymName, Arity) :-
    Name = unqualify_name(SymName),
    ( if
        map.search(extract_ids(UsedVersionNumbers, ItemType), Name - Arity,
            UsedVersionNumber)
    then
        % XXX This assumes that version numbers are timestamps.
        compare((>), UsedVersionNumber, UsedFileTimestamp)
    else
        true
    ).

:- pred check_for_simple_item_ambiguity(recomp_avail::in,
    timestamp::in, item_version_numbers::in, item_type::in(simple_item),
    sym_name::in, arity::in, bool::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity(RecompAvail, UsedFileTimestamp,
        VersionNumbers, ItemType, SymName, Arity, NeedsCheck, !Info) :-
    ( if
        item_is_new_or_changed(UsedFileTimestamp, VersionNumbers,
            ItemType, SymName, Arity)
    then
        NeedsCheck = yes,
        UsedItems = !.Info ^ rci_used_items,
        UsedItemMap = extract_simple_item_set(UsedItems, ItemType),
        Name = unqualify_name(SymName),
        ( if map.search(UsedItemMap, Name - Arity, MatchingQualifiers) then
            map.foldl(
                check_for_simple_item_ambiguity_2(ItemType,
                    RecompAvail, SymName, Arity),
                MatchingQualifiers, !Info)
        else
            true
        )
    else
        NeedsCheck = no
    ).

:- pred check_for_simple_item_ambiguity_2(item_type::in,
    recomp_avail::in, sym_name::in, arity::in,
    module_qualifier::in, module_name::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity_2(ItemType, RecompAvail, SymName, Arity,
        OldModuleQualifier, OldMatchingModuleName, !Info) :-
    Name = unqualify_name(SymName),
    ( if
        % XXX RECOMP401 This logic is ancient, and may do the wrong thing
        % with most values of RecompAvail, since they those values did not
        % exist when the original version of this was written.
        ( RecompAvail = recomp_avail_int_use
        ; RecompAvail = recomp_avail_imp_use
        ),
        % XXX This is a bit conservative in the case of partially qualified
        % names but that hopefully won't come up too often.
        OldModuleQualifier = unqualified("")
    then
        true
    else if
        QualifiedName = module_qualify_name(OldModuleQualifier, Name),
        partial_sym_name_matches_full(QualifiedName, SymName),
        not SymName = qualified(OldMatchingModuleName, _)
    then
        OldMatchingName = qualified(OldMatchingModuleName, Name),
        Reason = recompile_for_item_ambiguity(
            item_id(ItemType, item_name(SymName, Arity)),
            [item_id(ItemType, item_name(OldMatchingName, Arity))]),
        record_recompilation_reason(Reason, !Info)
    else
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity(bool::in,
    recomp_avail::in, timestamp::in, item_version_numbers::in,
    pred_or_func::in, sym_name::in,
    list(type_and_mode)::in, maybe(mer_type)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity(NeedsCheck, RecompAvail, OldTimestamp,
        VersionNumbers, PredOrFunc, SymName, Args, WithType, !Info) :-
    (
        WithType = no,
        adjust_func_arity(PredOrFunc, Arity, list.length(Args))
    ;
        WithType = yes(_),
        Arity = list.length(Args)
    ),
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    ( if
        (
            NeedsCheck = yes
        ;
            item_is_new_or_changed(OldTimestamp, VersionNumbers,
                ItemType, SymName, Arity)
        )
    then
        UsedItems = !.Info ^ rci_used_items,
        UsedItemMap = extract_pred_or_func_set(UsedItems, ItemType),
        Name = unqualify_name(SymName),
        ( if map.search(UsedItemMap, Name, MatchingArityList) then
            list.foldl(
                check_for_pred_or_func_item_ambiguity_1(WithType,
                    ItemType, RecompAvail, SymName, Arity),
                MatchingArityList, !Info)
        else
            true
        ),

        PredId = invalid_pred_id,
        (
            SymName = qualified(ModuleName, _),
            (
                WithType = yes(_),
                % We don't know the actual arity.
                AritiesToMatch = match_arity_any
            ;
                WithType = no,
                AritiesToMatch = match_arity_less_than_or_equal(Arity)
            ),
            ResolvedFunctor = resolved_functor_pred_or_func(PredId, ModuleName,
                PredOrFunc, Arity),
            check_functor_ambiguities_by_name(RecompAvail, SymName,
                AritiesToMatch, ResolvedFunctor, !Info)
        ;
            SymName = unqualified(_),
            unexpected($pred, "unqualified predicate name")
        )
    else
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity_1(maybe(mer_type)::in,
    item_type::in, recomp_avail::in, sym_name::in, arity::in,
    pair(arity, map(sym_name, set(pair(pred_id, module_name))))::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_1(WithType, ItemType, RecompAvail,
        SymName, Arity, MatchArity - MatchingQualifiers, !Info) :-
    ( if
        (
            WithType = yes(_),
            MatchArity >= Arity
        ;
            WithType = no,
            MatchArity = Arity
        )
    then
        map.foldl(
            check_for_pred_or_func_item_ambiguity_2(ItemType, RecompAvail,
                SymName, MatchArity),
            MatchingQualifiers, !Info)
    else
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity_2(item_type::in,
    recomp_avail::in, sym_name::in, arity::in, module_qualifier::in,
    set(pair(pred_id, module_name))::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_2(ItemType, RecompAvail,
        SymName, Arity, OldModuleQualifier, OldMatchingModuleNames, !Info) :-
    Name = unqualify_name(SymName),
    ( if
        % XXX RECOMP401 This logic is ancient, and may do the wrong thing
        % with most values of RecompAvail, since they those values did not
        % exist when the original version of this was written.
        ( RecompAvail = recomp_avail_int_use
        ; RecompAvail = recomp_avail_imp_use
        ),
        % XXX This is a bit conservative in the case of partially qualified
        % names but that hopefully won't come up too often.
        OldModuleQualifier = unqualified("")
    then
        true
    else if
        QualifiedName = module_qualify_name(OldModuleQualifier, Name),
        partial_sym_name_matches_full(QualifiedName, SymName),
        not (
            SymName = qualified(PredModuleName, _),
            set.member(_ - PredModuleName, OldMatchingModuleNames)
        )
    then
        AmbiguousDecls = list.map(
            ( func(_ - OldMatchingModule) = Item :-
                OldMatchingName = qualified(OldMatchingModule, Name),
                Item = item_id(ItemType, item_name(OldMatchingName, Arity))
            ),
            set.to_sorted_list(OldMatchingModuleNames)),
        Reason = recompile_for_item_ambiguity(item_id(ItemType,
            item_name(SymName, Arity)), AmbiguousDecls),
        record_recompilation_reason(Reason, !Info)
    else
        true
    ).

    % Go over the constructors for a type which has changed and check whether
    % any of them could create an ambiguity with functors used during the
    % last compilation.
    %
:- pred check_type_defn_ambiguity_with_functor(recomp_avail::in,
    type_ctor::in, type_defn::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_type_defn_ambiguity_with_functor(RecompAvail, TypeCtor, TypeDefn,
        !Info) :-
    (
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        )
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(Ctors, _, _),
        list.foldl(check_functor_ambiguities(RecompAvail, TypeCtor),
            one_or_more_to_list(Ctors), !Info)
    ).

:- pred check_functor_ambiguities(recomp_avail::in, type_ctor::in,
    constructor::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities(RecompAvail, TypeCtor, Ctor, !Info) :-
    Ctor = ctor(_, _, Name, Args, Arity, _),
    TypeCtorItem = type_ctor_to_item_name(TypeCtor),
    ResolvedCtor = resolved_functor_constructor(TypeCtorItem),
    check_functor_ambiguities_by_name(RecompAvail, Name,
        match_arity_exact(Arity), ResolvedCtor, !Info),
    list.foldl(
        check_field_ambiguities(RecompAvail,
            resolved_functor_field(TypeCtorItem, item_name(Name, Arity))),
        Args, !Info).

:- pred check_field_ambiguities(recomp_avail::in,
    resolved_functor::in, constructor_arg::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_field_ambiguities(RecompAvail, ResolvedCtor, CtorArg, !Info) :-
    CtorArg = ctor_arg(MaybeCtorFieldName, _, _),
    (
        MaybeCtorFieldName = no
    ;
        MaybeCtorFieldName = yes(CtorFieldName),
        CtorFieldName = ctor_field_name(FieldName, _Ctxt),
        % XXX The arities to match below will need to change if we ever
        % allow taking the address of field access functions.
        field_access_function_name(get, FieldName, ExtractFuncName),
        field_access_function_name(set, FieldName, UpdateFuncName),
        check_functor_ambiguities_by_name(RecompAvail, ExtractFuncName,
            match_arity_exact(1), ResolvedCtor, !Info),
        check_functor_ambiguities_by_name(RecompAvail, UpdateFuncName,
            match_arity_exact(2), ResolvedCtor, !Info)
    ).

    % Predicates and functions used as functors can match any arity
    % less than or equal to the predicate or function's arity.
:- type functor_match_arity
    --->    match_arity_exact(arity)
    ;       match_arity_less_than_or_equal(arity)
    ;       match_arity_any.

:- pred check_functor_ambiguities_by_name(recomp_avail::in,
    sym_name::in, functor_match_arity::in, resolved_functor::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_by_name(RecompAvail, Name, MatchArity,
        ResolvedCtor, !Info) :-
    UsedItems = !.Info ^ rci_used_items,
    UnqualName = unqualify_name(Name),
    UsedCtors = UsedItems ^ functors,
    ( if map.search(UsedCtors, UnqualName, UsedCtorAL) then
        check_functor_ambiguities_2(RecompAvail, Name, MatchArity,
            ResolvedCtor, UsedCtorAL, !Info)
    else
        true
    ).

:- pred check_functor_ambiguities_2(recomp_avail::in, sym_name::in,
    functor_match_arity::in, resolved_functor::in,
    assoc_list(arity, resolved_functor_map)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_2(_, _, _, _, [], !Info).
check_functor_ambiguities_2(RecompAvail, Name, MatchArity,
        ResolvedCtor, [Arity - UsedCtorMap | UsedCtorAL], !Info) :-
    (
        MatchArity = match_arity_exact(ArityToMatch),
        ( if ArityToMatch = Arity then
            Check = yes,
            Continue = no
        else
            Check = no,
            ( if Arity < ArityToMatch then
                Continue = yes
            else
                Continue = no
            )
        )
    ;
        MatchArity = match_arity_less_than_or_equal(ArityToMatch),
        ( if Arity =< ArityToMatch then
            Check = yes,
            Continue = yes
        else
            Check = no,
            Continue = no
        )
    ;
        MatchArity = match_arity_any,
        Check = yes,
        Continue = yes
    ),
    (
        Check = yes,
        map.foldl(check_functor_ambiguity(RecompAvail, Name, Arity,
            ResolvedCtor), UsedCtorMap, !Info)
    ;
        Check = no
    ),
    (
        Continue = yes,
        check_functor_ambiguities_2(RecompAvail, Name, MatchArity,
            ResolvedCtor, UsedCtorAL, !Info)
    ;
        Continue = no
    ).

:- pred check_functor_ambiguity(recomp_avail::in,
    sym_name::in, arity::in, resolved_functor::in,
    module_qualifier::in, set(resolved_functor)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguity(RecompAvail, SymName, Arity, ResolvedCtor,
        OldModuleQualifier, OldResolvedCtors, !Info) :-
    ( if
        % XXX RECOMP401 This logic is ancient, and may do the wrong thing
        % with most values of RecompAvail, since they those values did not
        % exist when the original version of this was written.
        ( RecompAvail = recomp_avail_int_use
        ; RecompAvail = recomp_avail_imp_use
        ),
        % XXX This is a bit conservative in the case of partially qualified
        % names but that hopefully won't come up too often.
        OldModuleQualifier = unqualified("")
    then
        true
    else if
        Name = unqualify_name(SymName),
        OldName = module_qualify_name(OldModuleQualifier, Name),
        partial_sym_name_matches_full(OldName, SymName),
        not set.member(ResolvedCtor, OldResolvedCtors)
    then
        OldModuleQualName = module_qualify_name(OldModuleQualifier, Name),
        Reason = recompile_for_functor_ambiguity(OldModuleQualName, Arity,
            ResolvedCtor, set.to_sorted_list(OldResolvedCtors)),
        record_recompilation_reason(Reason, !Info)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- type recompilation_check_info
    --->    recompilation_check_info(
                rci_module_name             :: module_name,
                rci_is_inline_sub_module    :: bool,
                rci_sub_modules             :: list(module_name),
                rci_have_read_module_maps   :: have_read_module_maps,
                rci_used_items              :: resolved_used_items,
                rci_used_typeclasses        :: set(item_name),
                rci_modules_to_recompile    :: modules_to_recompile,
                rci_collect_all_reasons     :: bool,
                rci_recompilation_reasons   :: list(recompile_reason)
            ).

:- type recompile_exception
    --->    recompile_exception(
                recompile_reason,
                recompilation_check_info
            ).

:- type recompile_reason
    --->    recompile_for_file_error(
                file_name,
                list(format_component)
            )
    ;       recompile_for_output_file_not_up_to_date(
                file_name
            )
    ;       recompile_for_syntax_error(
                term.context,
                string
            )
    ;       recompile_for_unreadable_used_items(
                list(error_spec)
            )
    ;       recompile_for_module_changed(
                file_name
            )
    ;       recompile_for_item_ambiguity(
                item_id,                % new item.
                list(item_id)           % ambiguous declarations.
            )
    ;       recompile_for_functor_ambiguity(
                sym_name,
                arity,
                resolved_functor,       % new item.
                list(resolved_functor)  % ambiguous declarations.
            )
    ;       recompile_for_changed_item(
                item_id
            )
    ;       recompile_for_removed_item(
                item_id
            )
    ;       recompile_for_changed_or_added_instance(
                module_name,
                item_name               % class name
            )
    ;       recompile_for_removed_instance(
                module_name,
                item_name               % class name
            ).

:- pred add_module_to_recompile(module_name::in, recompilation_check_info::in,
    recompilation_check_info::out) is det.

add_module_to_recompile(Module, !Info) :-
    ModulesToRecompile0 = !.Info ^ rci_modules_to_recompile,
    (
        ModulesToRecompile0 = all_modules
    ;
        ModulesToRecompile0 = some_modules(Modules0),
        !Info ^ rci_modules_to_recompile := some_modules([Module | Modules0])
    ).

:- pred record_read_file_src(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_src::in, list(error_spec)::in,
    read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_src(ModuleName, FileName, ModuleTimestamp,
        ParseTree, Specs, Errors, !Info) :-
    HaveReadModuleMaps0 = !.Info ^ rci_have_read_module_maps,
    HaveReadModuleMapSrc0 = HaveReadModuleMaps0 ^ hrmm_src,
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    map.set(ModuleName,
        have_successfully_read_module(FileName, yes(Timestamp),
            ParseTree, Specs, Errors),
        HaveReadModuleMapSrc0, HaveReadModuleMapSrc),
    HaveReadModuleMaps =
        HaveReadModuleMaps0 ^ hrmm_src := HaveReadModuleMapSrc,
    !Info ^ rci_have_read_module_maps := HaveReadModuleMaps.

:- pred record_read_file_int(module_name::in, int_file_kind::in, file_name::in,
    module_timestamp::in, parse_tree_int::in, list(error_spec)::in,
    read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_int(ModuleName, IntFileKind, FileName, ModuleTimestamp,
        ParseTree, Specs, Errors, !Info) :-
    HaveReadModuleMaps0 = !.Info ^ rci_have_read_module_maps,
    HaveReadModuleMapInt0 = HaveReadModuleMaps0 ^ hrmm_int,
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    map.set(have_read_module_key(ModuleName, IntFileKind),
        have_successfully_read_module(FileName, yes(Timestamp),
            ParseTree, Specs, Errors),
        HaveReadModuleMapInt0, HaveReadModuleMapInt),
    HaveReadModuleMaps =
        HaveReadModuleMaps0 ^ hrmm_int := HaveReadModuleMapInt,
    !Info ^ rci_have_read_module_maps := HaveReadModuleMaps.

%-----------------------------------------------------------------------------%

:- pred write_recompilation_message(globals::in,
    pred(io, io)::in(pred(di, uo) is det), io::di, io::uo) is det.

write_recompilation_message(Globals, P, !IO) :-
    globals.lookup_bool_option(Globals, verbose_recompilation, Verbose),
    (
        Verbose = yes,
        P(!IO)
    ;
        Verbose = no
    ).

:- pred write_recompile_reason(globals::in, module_name::in,
    recompile_reason::in, io::di, io::uo) is det.

write_recompile_reason(Globals, ModuleName, Reason, !IO) :-
    PrefixPieces = [words("Recompiling module"), qual_sym_name(ModuleName),
        suffix(":"), nl],
    recompile_reason_message(Globals, PrefixPieces, Reason, Spec),
    % Since these messages are informational, there should be no warnings
    % or errors.
    write_error_spec_ignore(Globals, Spec, !IO).

:- pred recompile_reason_message(globals::in, list(format_component)::in,
    recompile_reason::in, error_spec::out) is det.

recompile_reason_message(Globals, PrefixPieces, Reason, Spec) :-
    (
        (
            Reason = recompile_for_file_error(_FileName, Pieces)
            % Pieces should mention FileName.
        ;
            Reason = recompile_for_output_file_not_up_to_date(FileName),
            Pieces = [words("output file"), quote(FileName),
                words("is not up to date.")]
        ;
            Reason = recompile_for_module_changed(FileName),
            Pieces = [words("file"), quote(FileName), words("has changed.")]
        ;
            Reason = recompile_for_item_ambiguity(Item, AmbiguousItems),
            ItemPieces = describe_item(Item),
            AmbiguousItemPieces = component_lists_to_pieces("and",
                list.map(describe_item, AmbiguousItems)),
            Pieces = [words("addition of") | ItemPieces]
                ++ [words("could cause an ambiguity with")]
                ++ AmbiguousItemPieces ++ [suffix(".")]
        ;
            Reason = recompile_for_functor_ambiguity(SymName, Arity,
                Functor, AmbiguousFunctors),
            FunctorPieces = describe_resolved_functor(SymName, Arity, Functor),
            AmbiguousFunctorPieces = component_lists_to_pieces("and",
                list.map(describe_resolved_functor(SymName, Arity),
                    AmbiguousFunctors)),
            Pieces = [words("addition of") | FunctorPieces]
                ++ [words("could cause an ambiguity with")]
                ++ AmbiguousFunctorPieces ++ [suffix(".")]
        ;
            Reason = recompile_for_changed_item(Item),
            Pieces = describe_item(Item) ++ [words("was modified.")]
        ;
            Reason = recompile_for_removed_item(Item),
            Pieces = describe_item(Item) ++ [words("was removed.")]
        ;
            Reason = recompile_for_changed_or_added_instance(ModuleName,
                item_name(ClassName, ClassArity)),
            Pieces = [words("an instance for class"),
                qual_sym_name_arity(sym_name_arity(ClassName, ClassArity)),
                words("in module"), qual_sym_name(ModuleName),
                words("was added or modified.")]
        ;
            Reason = recompile_for_removed_instance(ModuleName,
                item_name(ClassName, ClassArity)),
            Pieces = [words("an instance for class "),
                qual_sym_name_arity(sym_name_arity(ClassName, ClassArity)),
                words("in module"), qual_sym_name(ModuleName),
                words("was removed.")]
        ),
        MaybeContext = no,
        AllPieces = PrefixPieces ++ Pieces,
        Spec = error_spec($pred, severity_informational, phase_read_files,
            [error_msg(MaybeContext, treat_as_first, 0, [always(AllPieces)])])
    ;
        Reason = recompile_for_syntax_error(Context, Msg),
        MaybeContext = yes(Context),
        AllPieces = PrefixPieces ++ [words(Msg), suffix("."), nl],
        Spec = error_spec($pred, severity_informational, phase_read_files,
            [error_msg(MaybeContext, treat_as_first, 0, [always(AllPieces)])])
    ;
        Reason = recompile_for_unreadable_used_items(Specs),
        list.map(extract_spec_msgs(Globals), Specs, MsgsList),
        list.condense(MsgsList, Msgs),
        % MaybeContext = find_first_context_in_msgs(Msgs),
        Spec = error_spec($pred, severity_informational, phase_read_files,
            Msgs)
    ).

:- func describe_item(item_id) = list(format_component).

describe_item(item_id(ItemType0, item_name(SymName, Arity))) = Pieces :-
    ( if body_item(ItemType0, ItemType1) then
        string_to_item_type(ItemTypeStr, ItemType1),
        ItemPieces = [words("body of"), words(ItemTypeStr)]
    else
        string_to_item_type(ItemTypeStr, ItemType0),
        ItemPieces = [words(ItemTypeStr)]
    ),
    Pieces = ItemPieces ++
        [qual_sym_name_arity(sym_name_arity(SymName, Arity))].

:- pred body_item(item_type::in, item_type::out) is semidet.

body_item(type_body_item, type_abstract_item).

:- func describe_resolved_functor(sym_name, arity, resolved_functor) =
    list(format_component).

describe_resolved_functor(SymName, _Arity, ResolvedFunctor) = Pieces :-
    ResolvedFunctor = resolved_functor_pred_or_func(_, ModuleName, PredOrFunc,
        PredArity),
    string_to_item_type(ItemTypeStr, pred_or_func_to_item_type(PredOrFunc)),
    UnqualName = unqualify_name(SymName),
    SymNameAndArity =
        sym_name_arity(qualified(ModuleName, UnqualName), PredArity),
    SymNameAndArityPiece = qual_sym_name_arity(SymNameAndArity),
    Pieces = [words(ItemTypeStr), SymNameAndArityPiece].
describe_resolved_functor(SymName, Arity, ResolvedFunctor) = Pieces :-
    ResolvedFunctor = resolved_functor_constructor(
        item_name(TypeName, TypeArity)),
    Pieces = [words("constructor"),
        unqual_sym_name_arity(sym_name_arity(SymName, Arity)),
        words("of type"),
        qual_sym_name_arity(sym_name_arity(TypeName, TypeArity))].
describe_resolved_functor(SymName, Arity, ResolvedFunctor) = Pieces :-
    ResolvedFunctor = resolved_functor_field(item_name(TypeName, TypeArity),
        item_name(ConsName, ConsArity)),
    Pieces = [words("field access function"),
        unqual_sym_name_arity(sym_name_arity(SymName, Arity)),
        words("for constructor"),
        unqual_sym_name_arity(sym_name_arity(ConsName, ConsArity)),
        words("of type"),
        qual_sym_name_arity(sym_name_arity(TypeName, TypeArity))].

%-----------------------------------------------------------------------------%

:- pred read_term_check_for_error_or_eof(io.text_input_stream::in,
    recompilation_check_info::in, string::in, term::out,
    io::di, io::uo) is det.

read_term_check_for_error_or_eof(VersionStream, Info, Item, Term, !IO) :-
    parser.read_term(VersionStream, TermResult, !IO),
    (
        TermResult = term(_, Term)
    ;
        TermResult = error(Message, Line),
        io.input_stream_name(VersionStream, FileName, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            Message),
        throw_syntax_error(Reason, Info)
    ;
        TermResult = eof,
        io.input_stream_name(VersionStream, FileName, !IO),
        io.get_line_number(VersionStream, Line, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            "unexpected end of file, expected " ++ Item ++ "."),
        throw_syntax_error(Reason, Info)
    ).

:- pred record_recompilation_reason(recompile_reason::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_recompilation_reason(Reason, !Info) :-
    CollectAllReasons = !.Info ^ rci_collect_all_reasons,
    (
        CollectAllReasons = yes,
        !Info ^ rci_recompilation_reasons :=
            [Reason | !.Info ^ rci_recompilation_reasons]
    ;
        CollectAllReasons = no,
        throw(recompile_exception(Reason, !.Info))
    ).

:- pred throw_syntax_error(recompile_reason::in, recompilation_check_info::in)
    is erroneous.

throw_syntax_error(Reason, Info) :-
    % If there were syntax errors in a `.used' file written during
    % a compilation, all outputs of that compilation are slightly
    % suspect, so it's worth entirely redoing the compilation.
    RecompileInfo = Info ^ rci_modules_to_recompile := all_modules,
    throw(recompile_exception(Reason, RecompileInfo)).

%-----------------------------------------------------------------------------%
:- end_module recompilation.check.
%-----------------------------------------------------------------------------%
