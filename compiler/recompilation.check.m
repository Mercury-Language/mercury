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

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
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
    %   FindTimestampFiles, ModulesToRecompile, ReadModules)
    %
    % Process the `.used'  files for the given module and all its
    % inline sub-modules to find out which modules need to be recompiled.
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
    modules_to_recompile::out, have_read_module_map::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.    % for type field_access_type
:- import_module hlds.hlds_pred.    % for field_access_function_name,
                                    % type pred_id.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_io_error.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_util.
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
:- import_module parser.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module univ.

%-----------------------------------------------------------------------------%

should_recompile(Globals, ModuleName, FindTargetFiles, FindTimestampFiles,
        Info ^ rci_modules_to_recompile, Info ^ rci_have_read_module_map,
        !IO) :-
    globals.lookup_bool_option(Globals, find_all_recompilation_reasons,
        FindAll),
    Info0 = recompilation_check_info(ModuleName, no, [], map.init,
        init_item_id_set(map.init, map.init, map.init),
        set.init, some_modules([]), FindAll, []),
    should_recompile_2(Globals, no, FindTargetFiles, FindTimestampFiles,
        ModuleName, Info0, Info, !IO).

:- pred should_recompile_2(globals::in, bool::in,
    find_target_file_names::in(find_target_file_names),
    find_timestamp_file_names::in(find_timestamp_file_names), module_name::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_recompile_2(Globals, IsSubModule, FindTargetFiles, FindTimestampFiles,
        ModuleName, !Info, !IO) :-
    !Info ^ rci_module_name := ModuleName,
    !Info ^ rci_sub_modules := [],
    module_name_to_file_name(Globals, ModuleName, ".used",
        do_not_create_dirs, UsageFileName, !IO),
    io.open_input(UsageFileName, MaybeVersionStream, !IO),
    (
        MaybeVersionStream = ok(VersionStream0),
        io.set_input_stream(VersionStream0, OldInputStream, !IO),

        promise_equivalent_solutions [Result, !:IO] (
            should_recompile_3_try(Globals, IsSubModule, FindTimestampFiles,
            !.Info, Result, !IO)
        ),
        (
            Result = succeeded(!:Info),
            Reasons = !.Info ^ rci_recompilation_reasons
        ;
            Result = failed,
            unexpected($module, $pred, "try failed")
        ;
            Result = exception(Exception),
            ( univ_to_type(Exception, RecompileException0) ->
                RecompileException = RecompileException0
            ;
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
        io.set_input_stream(OldInputStream, VersionStream, !IO),
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

:- pred should_recompile_3_try(globals::in, bool::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    recompilation_check_info::in,
    exception_result(recompilation_check_info)::out,
    io::di, io::uo) is cc_multi.

should_recompile_3_try(Globals, IsSubModule, FindTargetFiles, Info,
        Result, !IO) :-
    try_io(should_recompile_3(Globals, IsSubModule, FindTargetFiles, Info),
        Result, !IO).

:- pred should_recompile_3(globals::in, bool::in,
    find_target_file_names::in(find_target_file_names),
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_recompile_3(Globals, IsSubModule, FindTargetFiles, !Info, !IO) :-
    % WARNING: any exceptions thrown before the sub_modules field is set
    % in the recompilation_check_info must set the modules_to_recompile field
    % to `all', or else the nested sub-modules will not be checked
    % and necessary recompilations may be missed.

    % Check that the format of the usage file is the current format.
    read_term_check_for_error_or_eof(!.Info, "usage file version number",
        VersionNumberTerm, !IO),
    ( if
        VersionNumberTerm = term.functor(term.atom(","),
            [UsageFileVersionNumberTerm,
            VersionNumbersVersionNumberTerm], _),
        UsageFileVersionNumberTerm =
            term.functor( term.integer(usage_file_version_number), _, _),
        VersionNumbersVersionNumberTerm =
            term.functor( term.integer(version_numbers_version_number), _, _)
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
    read_term_check_for_error_or_eof(!.Info, "module timestamp",
        TimestampTerm, !IO),
    parse_module_timestamp(!.Info, TimestampTerm, _, ModuleTimestamp),
    ModuleTimestamp = module_timestamp(_, RecordedTimestamp, _),

    (
        IsSubModule = yes
        % For inline sub-modules we don't need to check the module timestamp
        % because we've already checked the timestamp for the parent module.
    ;
        IsSubModule = no,
        % If the module has changed, recompile.
        ModuleName = !.Info ^ rci_module_name,
        read_module_if_changed(Globals, ModuleName, ".m", "Reading module",
            do_search, RecordedTimestamp, Items, Specs, Errors, FileName,
            MaybeNewTimestamp, !IO),
        ( if
            MaybeNewTimestamp = yes(NewTimestamp),
            NewTimestamp \= RecordedTimestamp
        then
            record_read_file(ModuleName,
                ModuleTimestamp ^ timestamp := NewTimestamp,
                Items, Specs, Errors, FileName, !Info),
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

    % Find out whether this module has any inline sub-modules.
    read_term_check_for_error_or_eof(!.Info, "inline sub-modules",
        SubModulesTerm, !IO),
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
    read_term_check_for_error_or_eof(!.Info, "used items", UsedItemsTerm, !IO),
    parse_used_items(!.Info, UsedItemsTerm, UsedItems),
    !Info ^ rci_used_items := UsedItems,

    read_term_check_for_error_or_eof(!.Info, "used classes",
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
    check_imported_modules(Globals, !Info, !IO).

:- pred require_recompilation_if_not_up_to_date(timestamp::in, file_name::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

require_recompilation_if_not_up_to_date(RecordedTimestamp, TargetFile,
        !Info, !IO) :-
    io.file_modification_time(TargetFile, TargetModTimeResult, !IO),
    (
        TargetModTimeResult = ok(TargetModTime),
        compare(TargetModTimeCompare, time_t_to_timestamp(TargetModTime),
            RecordedTimestamp),
        TargetModTimeCompare = (>)
    ->
        true
    ;
        Reason = recompile_for_output_file_not_up_to_date(TargetFile),
        record_recompilation_reason(Reason, !Info)
    ).

:- pred parse_name_and_arity_to_used(term::in, item_name::out) is semidet.

parse_name_and_arity_to_used(Term, UsedClass) :-
    parse_name_and_arity_unqualified(Term, ClassName, ClassArity),
    UsedClass = item_name(ClassName, ClassArity).

%-----------------------------------------------------------------------------%

:- pred parse_module_timestamp(recompilation_check_info::in, term::in,
    module_name::out, module_timestamp::out) is det.

parse_module_timestamp(Info, Term, ModuleName, ModuleTimestamp) :-
    conjunction_to_list(Term, Args),
    (
        Args = [ModuleNameTerm, SuffixTerm, TimestampTerm | MaybeOtherTerms],
        try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName0),
        SuffixTerm = term.functor(term.string(Suffix), [], _),
        Timestamp = term_to_timestamp(TimestampTerm),
        (
            MaybeOtherTerms = [term.functor(term.atom("used"), [], _)],
            NeedQualifier = must_be_qualified
        ;
            MaybeOtherTerms = [],
            NeedQualifier = may_be_unqualified
        )
    ->
        ModuleName = ModuleName0,
        ModuleTimestamp = module_timestamp(Suffix, Timestamp, NeedQualifier)
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in module timestamp"),
        throw_syntax_error(Reason, Info)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_used_items(recompilation_check_info::in,
    term::in, resolved_used_items::out) is det.

parse_used_items(Info, Term, UsedItems) :-
    ( Term = term.functor(term.atom("used_items"), UsedItemTerms, _) ->
        list.foldl(parse_used_item_set(Info), UsedItemTerms,
            init_item_id_set(map.init, map.init, map.init), UsedItems)
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in used items"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_used_item_set(recompilation_check_info::in, term::in,
    resolved_used_items::in, resolved_used_items::out) is det.

parse_used_item_set(Info, Term, UsedItems0, UsedItems) :-
    (
        Term = term.functor(term.atom(ItemTypeStr), ItemTerms, _),
        string_to_item_type(ItemTypeStr, ItemType)
    ->
        ( is_simple_item_type(ItemType) ->
            list.foldl(parse_simple_item(Info), ItemTerms,
                map.init, SimpleItems),
            UsedItems = update_simple_item_set(UsedItems0,
                ItemType, SimpleItems)
        ; is_pred_or_func_item_type(ItemType) ->
            list.foldl(parse_pred_or_func_item(Info),
                ItemTerms, map.init, PredOrFuncItems),
            UsedItems = update_pred_or_func_set(UsedItems0,
                ItemType, PredOrFuncItems)
        ; ItemType = functor_item ->
            list.foldl(parse_functor_item(Info),
                ItemTerms, map.init, CtorItems),
            UsedItems = UsedItems0 ^ functors := CtorItems
        ;
            Reason = recompile_for_syntax_error(get_term_context(Term),
                "error in used items: unknown item type: " ++ ItemTypeStr),
            throw_syntax_error(Reason, Info)
        )
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in used items"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_simple_item(recompilation_check_info::in, term::in,
    simple_item_set::in, simple_item_set::out) is det.

parse_simple_item(Info, Term, !Set) :-
    (
        Term = term.functor(term.atom("-"), [NameArityTerm, MatchesTerm], _),
        parse_name_and_arity_unqualified(NameArityTerm, SymName, Arity)
    ->
        Name = unqualify_name(SymName),
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.foldl(parse_simple_item_match(Info), MatchTermList,
            map.init, Matches),
        map.det_insert(Name - Arity, Matches, !Set)
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in simple items"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_simple_item_match(recompilation_check_info::in, term::in,
    map(module_qualifier, module_name)::in,
    map(module_qualifier, module_name)::out) is det.

parse_simple_item_match(Info, Term, !Items) :-
    (
        (
            Term = term.functor(term.atom("=>"),
                [QualifierTerm, ModuleNameTerm], _)
        ->
            try_parse_sym_name_and_no_args(QualifierTerm, Qualifier),
            try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName)
        ;
            try_parse_sym_name_and_no_args(Term, ModuleName),
            Qualifier = ModuleName
        )
    ->
        map.det_insert(Qualifier, ModuleName, !Items)
    ;
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
    (
        (
            Term = term.functor(term.atom("=>"),
                [QualifierTerm, MatchesTerm], _)
        ->
            try_parse_sym_name_and_no_args(QualifierTerm, Qualifier),
            conjunction_to_list(MatchesTerm, MatchesList),
            list.map(
                (pred(MatchTerm::in, Match::out) is semidet :-
                    try_parse_sym_name_and_no_args(MatchTerm, MatchName),
                    Match = PredId - MatchName
                ),
                MatchesList, Matches)
        ;
            try_parse_sym_name_and_no_args(Term, Qualifier),
            Matches = [PredId - Qualifier]
        )
    ->
        map.det_insert(Qualifier, set.list_to_set(Matches), !Items)
    ;
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
    (
        Term = term.functor(term.atom("=>"),
            [QualifierTerm, MatchesTerm], _),
        try_parse_sym_name_and_no_args(QualifierTerm, Qualifier)
    ->
        conjunction_to_list(MatchesTerm, MatchesList),
        list.map(parse_resolved_functor(Info), MatchesList, Matches),
        map.det_insert(Qualifier, set.list_to_set(Matches), !Map)
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in functor match"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_resolved_functor(recompilation_check_info::in, term::in,
    resolved_functor::out) is det.

parse_resolved_functor(Info, Term, Ctor) :-
    (
        Term = term.functor(term.atom(PredOrFuncStr),
            [ModuleTerm, ArityTerm], _),
        ( PredOrFuncStr = "predicate", PredOrFunc = pf_predicate
        ; PredOrFuncStr = "function", PredOrFunc = pf_function
        ),
        try_parse_sym_name_and_no_args(ModuleTerm, ModuleName),
        ArityTerm = term.functor(term.integer(Arity), [], _)
    ->
        PredId = invalid_pred_id,
        Ctor = resolved_functor_pred_or_func(PredId, ModuleName, PredOrFunc,
            Arity)
    ;
        Term = term.functor(term.atom("ctor"), [NameArityTerm], _),
        parse_name_and_arity_unqualified(NameArityTerm, TypeName, TypeArity)
    ->
        Ctor = resolved_functor_constructor(item_name(TypeName, TypeArity))
    ;
        Term = term.functor(term.atom("field"),
            [TypeNameArityTerm, ConsNameArityTerm], _),
        parse_name_and_arity_unqualified(TypeNameArityTerm,
            TypeName, TypeArity),
        parse_name_and_arity_unqualified(ConsNameArityTerm,
            ConsName, ConsArity)
    ->
        Ctor = resolved_functor_field(item_name(TypeName, TypeArity),
            item_name(ConsName, ConsArity))
    ;
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
    (
        Term = term.functor(term.atom("-"), [NameTerm, MatchesTerm], _),
        NameTerm = term.functor(term.atom(Name), [], _)
    ->
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.map(parse_resolved_item_arity_matches(Info, ParseMatches),
            MatchTermList, Matches),
        map.det_insert(Name, Matches, !Set)
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in resolved item matches"),
        throw_syntax_error(Reason, Info)
    ).

:- pred parse_resolved_item_arity_matches(recompilation_check_info::in,
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
    term::in, pair(arity, resolved_item_map(T))::out) is det.

parse_resolved_item_arity_matches(Info, ParseMatches, Term,
        Arity - MatchMap) :-
    (
        Term = term.functor(term.atom("-"), [ArityTerm, MatchesTerm], _),
        ArityTerm = term.functor(term.integer(Arity0), [], _),
        conjunction_to_list(MatchesTerm, MatchTermList)
    ->
        Arity = Arity0,
        list.foldl(
            (pred(MatchTerm::in, Map0::in, Map::out) is det :-
                ParseMatches(Info, MatchTerm, Map0, Map)
            ),
            MatchTermList, map.init, MatchMap)
    ;
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in resolved item matches"),
        throw_syntax_error(Reason, Info)
    ).

%-----------------------------------------------------------------------------%

    % Check whether the interface file read for a module in the last
    % compilation has changed, and if so whether the items have changed
    % in a way which should cause a recompilation.
    %
:- pred check_imported_modules(globals::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_modules(Globals, !Info, !IO) :-
    parser.read_term(TermResult, !IO),
    (
        TermResult = term(_, Term),
        ( Term = term.functor(term.atom("done"), [], _) ->
            true
        ;
            check_imported_module(Globals, Term, !Info, !IO),
            check_imported_modules(Globals, !Info, !IO)
        )
    ;
        TermResult = error(Message, Line),
        io.input_stream_name(FileName, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            Message),
        throw_syntax_error(Reason, !.Info)
    ;
        TermResult = eof,
        % There should always be an item `done.' at the end of the list
        % of modules to check. This is used to make sure that the writing
        % of the `.used' file was not interrupted.
        io.input_stream_name(FileName, !IO),
        io.get_line_number(Line, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            "unexpected end of file"),
        throw_syntax_error(Reason, !.Info)
    ).

:- pred check_imported_module(globals::in, term::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_module(Globals, Term, !Info, !IO) :-
    (
        Term = term.functor(term.atom("=>"),
            [TimestampTerm0, UsedItemsTerm0], _)
    ->
        TimestampTerm = TimestampTerm0,
        MaybeUsedItemsTerm = yes(UsedItemsTerm0)
    ;
        TimestampTerm = Term,
        MaybeUsedItemsTerm = no
    ),
    parse_module_timestamp(!.Info, TimestampTerm,
        ImportedModuleName, ModuleTimestamp),

    ModuleTimestamp = module_timestamp(Suffix,
        RecordedTimestamp, NeedQualifier),
    (
        % If we're checking a sub-module, don't re-read interface files
        % read for other modules checked during this compilation.
        !.Info ^ rci_is_inline_sub_module = yes,
        find_read_module(!.Info ^ rci_have_read_module_map, ImportedModuleName,
            Suffix, do_return_timestamp, ItemsPrime, SpecsPrime, ErrorsPrime,
            FileNamePrime, MaybeNewTimestampPrime)
    ->
        Items = ItemsPrime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime,
        FileName = FileNamePrime,
        MaybeNewTimestamp = MaybeNewTimestampPrime,
        Recorded = bool.yes
    ;
        Recorded = bool.no,
        read_module_if_changed(Globals, ImportedModuleName, Suffix,
            "Reading interface file for module", do_search, RecordedTimestamp,
            Items, Specs, Errors, FileName, MaybeNewTimestamp, !IO)
    ),
    ( if set.is_empty(Errors) then
        ( if
            MaybeNewTimestamp = yes(NewTimestamp),
            NewTimestamp \= RecordedTimestamp
        then
            (
                Recorded = no,
                record_read_file(ImportedModuleName,
                    ModuleTimestamp ^ timestamp := NewTimestamp,
                    Items, Specs, Errors, FileName, !Info)
            ;
                Recorded = yes
            ),
            ( if
                MaybeUsedItemsTerm = yes(UsedItemsTerm),
                Items = [InterfaceItem, VersionNumberItem | OtherItems],
                InterfaceItem = item_module_defn(InterfaceItemModuleDefn),
                InterfaceItemModuleDefn =
                    item_module_defn_info(md_interface, _, _),
                VersionNumberItem =
                    item_module_defn(VersionNumberItemModuleDefn),
                VersionNumberItemModuleDefn =
                    item_module_defn_info(
                        md_version_numbers(_, VersionNumbers),
                        _, _)
            then
                check_module_used_items(ImportedModuleName, NeedQualifier,
                    RecordedTimestamp, UsedItemsTerm, VersionNumbers,
                    OtherItems, !Info)
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
        throw_syntax_error(
            recompile_for_file_error(FileName,
                [words("error reading file"), quote(FileName), suffix("."),
                nl]),
            !.Info)
    ).

:- pred check_module_used_items(module_name::in, need_qualifier::in,
    timestamp::in, term::in, version_numbers::in, list(item)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_module_used_items(ModuleName, NeedQualifier, OldTimestamp,
        UsedItemsTerm, NewVersionNumbers, Items, !Info) :-
    parse_version_numbers(UsedItemsTerm, UsedItemsResult),
    (
        UsedItemsResult = ok1(UsedVersionNumbers)
    ;
        UsedItemsResult = error1(Specs),
        (
            Specs = [],
            unexpected($module, $pred, "error1([])")
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
    list.foldl(check_for_ambiguities(NeedQualifier, OldTimestamp,
        UsedItemVersionNumbers), Items, !Info),

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
    ( [AddedInstance | _] = set.to_sorted_list(AddedInstances) ->
        Reason1 = recompile_for_changed_or_added_instance(ModuleName,
            AddedInstance),
        record_recompilation_reason(Reason1, !Info)
    ;
        true
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
    map.foldl(check_item_version_number(ModuleName,
        NewItemTypeVersionNumbers, ItemType),
        extract_ids(UsedVersionNumbers, ItemType), !Info).

:- pred check_item_version_number(module_name::in, version_number_map::in,
    item_type::in, pair(string, arity)::in, version_number::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_version_number(ModuleName, NewItemTypeVersionNumbers, ItemType,
        NameArity, UsedVersionNumber, !Info) :-
    ( map.search(NewItemTypeVersionNumbers, NameArity, NewVersionNumber) ->
        ( NewVersionNumber = UsedVersionNumber ->
            true
        ;
            Reason = recompile_for_changed_item(make_item_id(ModuleName,
                ItemType, NameArity)),
            record_recompilation_reason(Reason, !Info)
        )
    ;
        Reason = recompile_for_removed_item(make_item_id(ModuleName, ItemType,
            NameArity)),
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
    ( map.search(NewInstanceVersionNumbers, ClassId, NewVersionNumber) ->
        ( UsedVersionNumber = NewVersionNumber ->
            true
        ;
            Reason = recompile_for_changed_or_added_instance(ModuleName,
                ClassId),
            record_recompilation_reason(Reason, !Info)
        )
    ;
        Reason = recompile_for_removed_instance(ModuleName, ClassId),
        record_recompilation_reason(Reason, !Info)
    ).

%-----------------------------------------------------------------------------%

    % For each item which has changed since the last time we read the interface
    % file, check whether it introduces ambiguities with items which were used
    % when the current module was last compiled.
    %
:- pred check_for_ambiguities(need_qualifier::in, timestamp::in,
    item_version_numbers::in, item::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers, Item,
        !Info) :-
    (
        Item = item_clause(_),
        unexpected($module, $pred, "clause")
    ;
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(_, Name, Params, Body, _, _, _),
        Arity = list.length(Params),
        check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
            VersionNumbers, type_abstract_item, Name, Arity, NeedsCheck,
            !Info),
        (
            NeedsCheck = yes,
            check_type_defn_ambiguity_with_functor(NeedQualifier,
                type_ctor(Name, Arity), Body, !Info)
        ;
            NeedsCheck = no
        )
    ;
        Item = item_inst_defn(ItemInstDefn),
        ItemInstDefn = item_inst_defn_info(_, Name, Params, _, _, _, _),
        check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
            VersionNumbers, inst_item, Name, list.length(Params), _, !Info)
    ;
        Item = item_mode_defn(ItemModeDefn),
        ItemModeDefn = item_mode_defn_info(_, Name, Params, _, _, _, _),
        check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
            VersionNumbers, mode_item, Name, list.length(Params), _, !Info)
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(_, _, Name, Params, Interface,
            _, _, _),
        check_for_simple_item_ambiguity(NeedQualifier, OldTimestamp,
            VersionNumbers, typeclass_item, Name, list.length(Params),
            NeedsCheck, !Info),
        (
            NeedsCheck = yes,
            Interface = class_interface_concrete(Methods)
        ->
            list.foldl(check_class_method_for_ambiguities(NeedQualifier,
                OldTimestamp, VersionNumbers), Methods, !Info)
        ;
            true
        )
    ;
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info(_, _, _, _, PredOrFunc, Name, Args,
            WithType, _, _, _, _, _, _, _),
        check_for_pred_or_func_item_ambiguity(no, NeedQualifier, OldTimestamp,
            VersionNumbers, PredOrFunc, Name, Args, WithType, !Info)
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_module_defn(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        )
    ).

:- pred check_class_method_for_ambiguities(need_qualifier::in, timestamp::in,
    item_version_numbers::in, class_method::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_class_method_for_ambiguities(NeedQualifier, OldTimestamp, VersionNumbers,
        ClassMethod, !Info) :-
    (
        ClassMethod = method_pred_or_func(_, _, _, PredOrFunc, MethodName,
            MethodArgs, MethodWithType, _, _, _, _, _, _),
        check_for_pred_or_func_item_ambiguity(yes, NeedQualifier, OldTimestamp,
            VersionNumbers, PredOrFunc, MethodName, MethodArgs, MethodWithType,
            !Info)
    ;
        ClassMethod = method_pred_or_func_mode(_, _, _, _, _, _, _, _)
    ).

:- pred item_is_new_or_changed(timestamp::in, item_version_numbers::in,
    item_type::in, sym_name::in, arity::in) is semidet.

item_is_new_or_changed(UsedFileTimestamp, UsedVersionNumbers,
        ItemType, SymName, Arity) :-
    Name = unqualify_name(SymName),
    (
        map.search(extract_ids(UsedVersionNumbers, ItemType), Name - Arity,
            UsedVersionNumber)
    ->
        % XXX This assumes that version numbers are timestamps.
        compare((>), UsedVersionNumber, UsedFileTimestamp)
    ;
        true
    ).

:- pred check_for_simple_item_ambiguity(need_qualifier::in, timestamp::in,
    item_version_numbers::in, item_type::in(simple_item), sym_name::in,
    arity::in, bool::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity(NeedQualifier, UsedFileTimestamp,
        VersionNumbers, ItemType, SymName, Arity, NeedsCheck, !Info) :-
    (
        item_is_new_or_changed(UsedFileTimestamp, VersionNumbers,
            ItemType, SymName, Arity)
    ->
        NeedsCheck = yes,
        UsedItems = !.Info ^ rci_used_items,
        UsedItemMap = extract_simple_item_set(UsedItems, ItemType),
        Name = unqualify_name(SymName),
        (
            map.search(UsedItemMap, Name - Arity,
                MatchingQualifiers)
        ->
            map.foldl(
                check_for_simple_item_ambiguity_2(ItemType,
                    NeedQualifier, SymName, Arity),
                MatchingQualifiers, !Info)
        ;
            true
        )
    ;
        NeedsCheck = no
    ).

:- pred check_for_simple_item_ambiguity_2(item_type::in, need_qualifier::in,
    sym_name::in, arity::in, module_qualifier::in, module_name::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity_2(ItemType, NeedQualifier, SymName, Arity,
        OldModuleQualifier, OldMatchingModuleName, !Info) :-
    Name = unqualify_name(SymName),
    (
        % XXX This is a bit conservative in the case of partially qualified
        % names but that hopefully won't come up too often.
        NeedQualifier = must_be_qualified,
        OldModuleQualifier = unqualified("")
    ->
        true
    ;
        QualifiedName = module_qualify_name(OldModuleQualifier, Name),
        match_sym_name(QualifiedName, SymName),
        \+ SymName = qualified(OldMatchingModuleName, _)
    ->
        OldMatchingName = qualified(OldMatchingModuleName, Name),
        Reason = recompile_for_item_ambiguity(
            item_id(ItemType, item_name(SymName, Arity)),
            [item_id(ItemType, item_name(OldMatchingName, Arity))]),
        record_recompilation_reason(Reason, !Info)
    ;
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity(bool::in, need_qualifier::in,
    timestamp::in, item_version_numbers::in, pred_or_func::in,
    sym_name::in, list(type_and_mode)::in, maybe(mer_type)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity(NeedsCheck, NeedQualifier, OldTimestamp,
        VersionNumbers, PredOrFunc, SymName, Args, WithType, !Info) :-
    (
        WithType = no,
        adjust_func_arity(PredOrFunc, Arity, list.length(Args))
    ;
        WithType = yes(_),
        Arity = list.length(Args)
    ),
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    (
        (
            NeedsCheck = yes
        ;
            item_is_new_or_changed(OldTimestamp, VersionNumbers,
                ItemType, SymName, Arity)
        )
    ->
        UsedItems = !.Info ^ rci_used_items,
        UsedItemMap = extract_pred_or_func_set(UsedItems, ItemType),
        Name = unqualify_name(SymName),
        ( map.search(UsedItemMap, Name, MatchingArityList) ->
            list.foldl(check_for_pred_or_func_item_ambiguity_1(WithType,
                ItemType, NeedQualifier, SymName, Arity), MatchingArityList,
                !Info)
        ;
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
            check_functor_ambiguities_by_name(NeedQualifier, SymName,
                AritiesToMatch, ResolvedFunctor, !Info)
        ;
            SymName = unqualified(_),
            unexpected($module, $pred, "unqualified predicate name")
        )
    ;
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity_1(maybe(mer_type)::in,
    item_type::in, need_qualifier::in, sym_name::in, arity::in,
    pair(arity, map(sym_name, set(pair(pred_id, module_name))))::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_1(WithType, ItemType, NeedQualifier,
        SymName, Arity, MatchArity - MatchingQualifiers, !Info) :-
    (
        (
            WithType = yes(_),
            MatchArity >= Arity
        ;
            WithType = no,
            MatchArity = Arity
        )
    ->
        map.foldl(
            check_for_pred_or_func_item_ambiguity_2(ItemType, NeedQualifier,
                SymName, MatchArity),
            MatchingQualifiers, !Info)
    ;
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity_2(item_type::in,
    need_qualifier::in, sym_name::in, arity::in, module_qualifier::in,
    set(pair(pred_id, module_name))::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_2(ItemType, NeedQualifier,
        SymName, Arity, OldModuleQualifier, OldMatchingModuleNames, !Info) :-
    Name = unqualify_name(SymName),
    (
        % XXX This is a bit conservative in the case of partially qualified
        % names but that hopefully won't come up too often.
        NeedQualifier = must_be_qualified,
        OldModuleQualifier = unqualified("")
    ->
        true
    ;
        QualifiedName = module_qualify_name(OldModuleQualifier, Name),
        match_sym_name(QualifiedName, SymName),
        \+ (
            SymName = qualified(PredModuleName, _),
            set.member(_ - PredModuleName, OldMatchingModuleNames)
        )
    ->
        AmbiguousDecls = list.map(
            (func(_ - OldMatchingModule) = Item :-
                OldMatchingName = qualified(OldMatchingModule, Name),
                Item = item_id(ItemType, item_name(OldMatchingName, Arity))
            ),
            set.to_sorted_list(OldMatchingModuleNames)),
        Reason = recompile_for_item_ambiguity(item_id(ItemType,
            item_name(SymName, Arity)), AmbiguousDecls),
        record_recompilation_reason(Reason, !Info)
    ;
        true
    ).

    % Go over the constructors for a type which has changed and check whether
    % any of them could create an ambiguity with functors used during the
    % last compilation.
    %
:- pred check_type_defn_ambiguity_with_functor(need_qualifier::in,
    type_ctor::in, type_defn::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_type_defn_ambiguity_with_functor(NeedQualifier, TypeCtor, TypeDefn,
        !Info) :-
    (
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_foreign_type(_, _, _)
        ; TypeDefn = parse_tree_solver_type(_, _)
        )
    ;
        TypeDefn = parse_tree_du_type(Ctors, _, _),
        list.foldl(check_functor_ambiguities(NeedQualifier, TypeCtor), Ctors,
            !Info)
    ).

:- pred check_functor_ambiguities(need_qualifier::in, type_ctor::in,
    constructor::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities(NeedQualifier, TypeCtor, Ctor, !Info) :-
    Ctor = ctor(_, _, Name, Args, _),
    TypeCtorItem = type_ctor_to_item_name(TypeCtor),
    ResolvedCtor = resolved_functor_constructor(TypeCtorItem),
    Arity = list.length(Args),
    check_functor_ambiguities_by_name(NeedQualifier, Name,
        match_arity_exact(Arity), ResolvedCtor, !Info),
    list.foldl(
        check_field_ambiguities(NeedQualifier,
            resolved_functor_field(TypeCtorItem, item_name(Name, Arity))),
        Args, !Info).

:- pred check_field_ambiguities(need_qualifier::in, resolved_functor::in,
    constructor_arg::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_field_ambiguities(_, _, ctor_arg(no, _, _, _), !Info).
check_field_ambiguities(NeedQualifier, ResolvedCtor,
        ctor_arg(yes(ctor_field_name(FieldName, _Ctxt)), _, _, _), !Info) :-
    % XXX The arities to match below will need to change if we ever
    % allow taking the address of field access functions.
    field_access_function_name(get, FieldName, ExtractFuncName),
    check_functor_ambiguities_by_name(NeedQualifier, ExtractFuncName,
        match_arity_exact(1), ResolvedCtor, !Info),
    field_access_function_name(set, FieldName, UpdateFuncName),
    check_functor_ambiguities_by_name(NeedQualifier, UpdateFuncName,
        match_arity_exact(2), ResolvedCtor, !Info).

    % Predicates and functions used as functors can match any arity
    % less than or equal to the predicate or function's arity.
:- type functor_match_arity
    --->    match_arity_exact(arity)
    ;       match_arity_less_than_or_equal(arity)
    ;       match_arity_any.

:- pred check_functor_ambiguities_by_name(need_qualifier::in, sym_name::in,
    functor_match_arity::in, resolved_functor::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_by_name(NeedQualifier, Name, MatchArity,
        ResolvedCtor, !Info) :-
    UsedItems = !.Info ^ rci_used_items,
    UnqualName = unqualify_name(Name),
    UsedCtors = UsedItems ^ functors,
    ( map.search(UsedCtors, UnqualName, UsedCtorAL) ->
        check_functor_ambiguities_2(NeedQualifier, Name, MatchArity,
            ResolvedCtor, UsedCtorAL, !Info)
    ;
        true
    ).

:- pred check_functor_ambiguities_2(need_qualifier::in, sym_name::in,
    functor_match_arity::in, resolved_functor::in,
    assoc_list(arity, resolved_functor_map)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_2(_, _, _, _, [], !Info).
check_functor_ambiguities_2(NeedQualifier, Name, MatchArity,
        ResolvedCtor, [Arity - UsedCtorMap | UsedCtorAL], !Info) :-
    (
        MatchArity = match_arity_exact(ArityToMatch),
        ( ArityToMatch = Arity ->
            Check = yes,
            Continue = no
        ;
            Check = no,
            ( Arity < ArityToMatch ->
                Continue = yes
            ;
                Continue = no
            )
        )
    ;
        MatchArity = match_arity_less_than_or_equal(ArityToMatch),
        ( Arity =< ArityToMatch ->
            Check = yes,
            Continue = yes
        ;
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
        map.foldl(check_functor_ambiguity(NeedQualifier, Name, Arity,
            ResolvedCtor), UsedCtorMap, !Info)
    ;
        Check = no
    ),
    (
        Continue = yes,
        check_functor_ambiguities_2(NeedQualifier, Name, MatchArity,
            ResolvedCtor, UsedCtorAL, !Info)
    ;
        Continue = no
    ).

:- pred check_functor_ambiguity(need_qualifier::in,
    sym_name::in, arity::in, resolved_functor::in,
    module_qualifier::in, set(resolved_functor)::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguity(NeedQualifier, SymName, Arity, ResolvedCtor,
        OldModuleQualifier, OldResolvedCtors, !Info) :-
    (
        % XXX This is a bit conservative in the case of partially qualified
        % names but that hopefully won't come up too often.
        NeedQualifier = must_be_qualified,
        OldModuleQualifier = unqualified("")
    ->
        true
    ;
        Name = unqualify_name(SymName),
        OldName = module_qualify_name(OldModuleQualifier, Name),
        match_sym_name(OldName, SymName),
        \+ set.member(ResolvedCtor, OldResolvedCtors)
    ->
        Reason = recompile_for_functor_ambiguity(
            module_qualify_name(OldModuleQualifier, Name),
            Arity, ResolvedCtor, set.to_sorted_list(OldResolvedCtors)
        ),
        record_recompilation_reason(Reason, !Info)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- type recompilation_check_info
    --->    recompilation_check_info(
                rci_module_name             :: module_name,
                rci_is_inline_sub_module    :: bool,
                rci_sub_modules             :: list(module_name),
                rci_have_read_module_map    :: have_read_module_map,
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

:- pred record_read_file(module_name::in, module_timestamp::in,
    list(item)::in, list(error_spec)::in,
    read_module_errors::in, file_name::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file(ModuleName, ModuleTimestamp, Items, Specs, Errors, FileName,
        !Info) :-
    Imports0 = !.Info ^ rci_have_read_module_map,
    map.set(ModuleName - ModuleTimestamp ^ suffix,
        have_read_module(ModuleTimestamp, Items, Specs, Errors, FileName),
        Imports0, Imports),
    !Info ^ rci_have_read_module_map := Imports.

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
    PrefixPieces = [words("Recompiling module"), sym_name(ModuleName),
        suffix(":"), nl],
    recompile_reason_message(PrefixPieces, Reason, Spec),
    % Since these messages are informational, there should be no warnings
    % or errors.
    write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors, !IO).

:- pred recompile_reason_message(list(format_component)::in,
    recompile_reason::in, error_spec::out) is det.

recompile_reason_message(PrefixPieces, Reason, Spec) :-
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
            AmbiguousItemPieces = component_lists_to_pieces(
                list.map(describe_item, AmbiguousItems)),
            Pieces = [words("addition of") | ItemPieces]
                ++ [words("could cause an ambiguity with")]
                ++ AmbiguousItemPieces ++ [suffix(".")]
        ;
            Reason = recompile_for_functor_ambiguity(SymName, Arity,
                Functor, AmbiguousFunctors),
            FunctorPieces = describe_resolved_functor(SymName, Arity, Functor),
            AmbiguousFunctorPieces = component_lists_to_pieces(
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
                sym_name_and_arity(ClassName / ClassArity),
                words("in module"), sym_name(ModuleName),
                words("was added or modified.")]
        ;
            Reason = recompile_for_removed_instance(ModuleName,
                item_name(ClassName, ClassArity)),
            Pieces = [words("an instance for class "),
                sym_name_and_arity(ClassName / ClassArity),
                words("in module"), sym_name(ModuleName),
                words("was removed.")]
        ),
        MaybeContext = no,
        AllPieces = PrefixPieces ++ Pieces,
        Spec = error_spec(severity_informational, phase_read_files,
            [error_msg(MaybeContext, treat_as_first, 0, [always(AllPieces)])])
    ;
        Reason = recompile_for_syntax_error(Context, Msg),
        MaybeContext = yes(Context),
        AllPieces = PrefixPieces ++ [words(Msg), suffix("."), nl],
        Spec = error_spec(severity_informational, phase_read_files,
            [error_msg(MaybeContext, treat_as_first, 0, [always(AllPieces)])])
    ;
        Reason = recompile_for_unreadable_used_items(Specs),
        MsgsList = list.map(project_spec_to_msgs, Specs),
        list.condense(MsgsList, Msgs),
        % MaybeContext = find_first_context_in_msgs(Msgs),
        Spec = error_spec(severity_informational, phase_read_files, Msgs)
    ).

:- func project_spec_to_msgs(error_spec) = list(error_msg).

project_spec_to_msgs(Spec) = Msgs :-
    Spec = error_spec(_Severity, _Phase, Msgs).

:- func describe_item(item_id) = list(format_component).

describe_item(item_id(ItemType0, item_name(SymName, Arity))) = Pieces :-
    ( body_item(ItemType0, ItemType1) ->
        string_to_item_type(ItemTypeStr, ItemType1),
        ItemPieces = [words("body of"), words(ItemTypeStr)]
    ;
        string_to_item_type(ItemTypeStr, ItemType0),
        ItemPieces = [words(ItemTypeStr)]
    ),
    Pieces = ItemPieces ++ [sym_name_and_arity(SymName / Arity)].

:- pred body_item(item_type::in, item_type::out) is semidet.

body_item(type_body_item, type_abstract_item).

:- func describe_resolved_functor(sym_name, arity, resolved_functor) =
    list(format_component).

describe_resolved_functor(SymName, _Arity, ResolvedFunctor) = Pieces :-
    ResolvedFunctor = resolved_functor_pred_or_func(_, ModuleName, PredOrFunc,
        PredArity),
    string_to_item_type(ItemTypeStr, pred_or_func_to_item_type(PredOrFunc)),
    UnqualName = unqualify_name(SymName),
    SymNameAndArityPiece =
        sym_name_and_arity(qualified(ModuleName, UnqualName) / PredArity),
    Pieces = [words(ItemTypeStr), SymNameAndArityPiece].
describe_resolved_functor(SymName, Arity, ResolvedFunctor) = Pieces :-
    ResolvedFunctor = resolved_functor_constructor(
        item_name(TypeName, TypeArity)),
    Pieces = [words("constructor"), sym_name_and_arity(SymName / Arity),
        words("of type"), sym_name_and_arity(TypeName / TypeArity)].
describe_resolved_functor(SymName, Arity, ResolvedFunctor) = Pieces :-
    ResolvedFunctor = resolved_functor_field(item_name(TypeName, TypeArity),
        item_name(ConsName, ConsArity)),
    Pieces = [words("field access function"),
        sym_name_and_arity(SymName / Arity),
        words("for constructor"), sym_name_and_arity(ConsName / ConsArity),
        words("of type"), sym_name_and_arity(TypeName / TypeArity)].

%-----------------------------------------------------------------------------%

:- pred read_term_check_for_error_or_eof(recompilation_check_info::in,
    string::in, term::out, io::di, io::uo) is det.

read_term_check_for_error_or_eof(Info, Item, Term, !IO) :-
    parser.read_term(TermResult, !IO),
    (
        TermResult = term(_, Term)
    ;
        TermResult = error(Message, Line),
        io.input_stream_name(FileName, !IO),
        Reason = recompile_for_syntax_error(term.context(FileName, Line),
            Message),
        throw_syntax_error(Reason, Info)
    ;
        TermResult = eof,
        io.input_stream_name(FileName, !IO),
        io.get_line_number(Line, !IO),
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
