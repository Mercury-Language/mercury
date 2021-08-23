%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recompilation_check.m.
% Main author: stayl.
%
% Check whether a module should be recompiled.
%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_cons.    % for type field_access_type
:- import_module hlds.hlds_pred.    % for field_access_function_name,
                                    % type pred_id.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module parse_tree.convert_parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
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
:- import_module cord.
:- import_module int.
:- import_module lexer.             % for line_context and line_posn
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module parser.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module unit.

%---------------------------------------------------------------------------%

should_recompile(Globals, ModuleName, FindTargetFiles, FindTimestampFiles,
        ModulesToRecompile, HaveReadModuleMaps0, HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, find_all_recompilation_reasons,
        FindAll),
    Info0 = recompilation_check_info(ModuleName, no, [], HaveReadModuleMaps0,
        init_item_id_set(map.init, map.init, map.init),
        set.init, some_modules([]), FindAll, []),
    % XXX How do we know ModuleName is not an inline submodule?
    should_recompile_2(Globals, is_not_inline_submodule, FindTargetFiles,
        FindTimestampFiles, ModuleName, Info0, Info, !IO),
    ModulesToRecompile = Info ^ rci_modules_to_recompile,
    HaveReadModuleMaps = Info ^ rci_have_read_module_maps.

:- type maybe_is_inline_submodule
    --->    is_not_inline_submodule
    ;       is_inline_submodule.

:- pred should_recompile_2(globals::in, maybe_is_inline_submodule::in,
    find_target_file_names::in(find_target_file_names),
    find_timestamp_file_names::in(find_timestamp_file_names), module_name::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_recompile_2(Globals, IsSubModule, FindTargetFiles, FindTimestampFiles,
        ModuleName, !Info, !IO) :-
    !Info ^ rci_module_name := ModuleName,
    !Info ^ rci_sub_modules := [],
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".used")), ModuleName, UsedFileName, !IO),
    io.read_named_file_as_string(UsedFileName, MaybeUsedFileString, !IO),
    (
        MaybeUsedFileString = ok(UsedFileString),
        string.length(UsedFileString, MaxOffset),
        LineContext0 = line_context(1, 0),
        LinePosn0 = line_posn(0),
        should_recompile_3(UsedFileName, UsedFileString, MaxOffset,
            LineContext0, LinePosn0, Globals, IsSubModule, FindTargetFiles,
            MaybeStoppingReason, !Info, !IO),
        (
            MaybeStoppingReason = no,
            Reasons = !.Info ^ rci_recompilation_reasons
        ;
            MaybeStoppingReason = yes(StoppingReason),
            % Ignoring the old contents of the rci_recompilation_reasons field
            % in favor of the stopping reason preserves old behavior.
            % (We used to throw an exception containing StoppingReason,
            % and we used to ignore the contents of rci_collect_all_reasons
            % when catching that exception.)
            Reasons = [StoppingReason]
        ),
        (
            Reasons = [],
            FindTimestampFiles(ModuleName, TimestampFiles, !IO),
            write_recompilation_message(Globals,
                write_not_recompiling_message(ModuleName), !IO),
            get_progress_output_stream(Globals, ModuleName,
                ProgressStream, !IO),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            list.map_foldl(
                touch_datestamp(Globals, ProgressStream, ErrorStream),
                TimestampFiles, _Succeededs, !IO)
        ;
            Reasons = [_ | _],
            add_module_to_recompile(ModuleName, !Info),
            write_recompilation_message(Globals,
                write_reasons_message(Globals, ModuleName,
                    list.reverse(Reasons)),
                !IO)
        ),

        ModulesToRecompile = !.Info ^ rci_modules_to_recompile,
        (
            ModulesToRecompile = all_modules
        ;
            ModulesToRecompile = some_modules(_),
            % XXX How does this piece of code justify the jump from
            % "not all modules should be recompiled" to "must recompile
            % !.Info ^ rci_sub_modules"?
            !Info ^ rci_is_inline_sub_module := yes,
            list.foldl2(
                should_recompile_2(Globals, is_inline_submodule,
                    FindTargetFiles, FindTimestampFiles),
                !.Info ^ rci_sub_modules, !Info, !IO)
        )
    ;
        MaybeUsedFileString = error(_),
        write_recompilation_message(Globals,
            write_not_found_reasons_message(Globals, UsedFileName, ModuleName),
            !IO),
        !Info ^ rci_modules_to_recompile := all_modules
    ).

:- pred write_not_recompiling_message(module_name::in, io::di, io::uo) is det.

write_not_recompiling_message(ModuleName, !IO) :-
    io.format("Not recompiling module %s.\n",
        [s(sym_name_to_escaped_string(ModuleName))], !IO).

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

:- pred should_recompile_3(string::in, string::in, int::in,
    line_context::in, line_posn::in, globals::in,
    maybe_is_inline_submodule::in,
    find_target_file_names::in(find_target_file_names),
    maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_recompile_3(UsedFileName, UsedFileString, MaxOffset,
        !.LineContext, !.LinePosn, Globals, IsSubModule, FindTargetFiles,
        MaybeStoppingReason, !Info, !IO) :-
    % WARNING: any exceptions thrown before the sub_modules field is set
    % in the recompilation_check_info must set the modules_to_recompile field
    % to `all', or else the nested submodules will not be checked
    % and necessary recompilations may be missed.
    read_and_parse_used_file(UsedFileName, UsedFileString, MaxOffset,
        !.LineContext, !.LinePosn, ParseUsedFile),
    (
        ParseUsedFile = rpt_error(Reason),
        MaybeStoppingReason = yes(Reason)
    ;
        ParseUsedFile = rpt_ok(UsedFile),
        UsedFile = used_file(ModuleTimestamp, InlineSubModules,
            UsedItems, UsedClasses, UsedModules),

        ModuleTimestamp = module_timestamp(_, RecordedTimestamp, _),
        !Info ^ rci_sub_modules := InlineSubModules,
        !Info ^ rci_used_items := UsedItems,
        !Info ^ rci_used_typeclasses := set.list_to_set(UsedClasses),

        (
            IsSubModule = is_inline_submodule,
            % For inline submodules, we don't need to check the module
            % timestamp, because we have already checked the timestamp
            % for the parent module.
            MaybeStoppingReason0 = no
        ;
            IsSubModule = is_not_inline_submodule,
            % If the module has changed, recompile.
            ModuleName = !.Info ^ rci_module_name,
            read_module_src(Globals, "Reading module",
                do_not_ignore_errors, do_search, ModuleName, [], FileName,
                dont_read_module_if_match(RecordedTimestamp),
                MaybeNewTimestamp, ParseTree, Specs, Errors, !IO),
            ( if
                MaybeNewTimestamp = yes(NewTimestamp),
                NewTimestamp \= RecordedTimestamp
            then
                record_read_file_src(ModuleName, FileName,
                    ModuleTimestamp ^ mts_timestamp := NewTimestamp,
                    ParseTree, Specs, Errors, !Info),
                !Info ^ rci_modules_to_recompile := all_modules,
                ChangedReason = recompile_for_module_changed(FileName),
                record_recompilation_reason(ChangedReason,
                    MaybeStoppingReason0, !Info)
            else if
                ( set.is_non_empty(Errors)
                ; MaybeNewTimestamp = no
                )
            then
                % We are throwing away Specs, even though some of its elements
                % could illuminate the cause of the problem. XXX Is this OK?
                Pieces = [words("error reading file"), quote(FileName),
                    suffix("."), nl],
                FileReason = recompile_for_file_error(FileName, Pieces),
                % XXX Some of the errors in Errors could be errors other than
                % syntax errors.
                MaybeStoppingReason0 = yes(FileReason)
            else
                % We are throwing away Specs. Since it should be a repeat
                % of the errors we saw when the file was first read in,
                % this should be OK.
                MaybeStoppingReason0 = no
            )
        ),

        (
            MaybeStoppingReason0 = yes(_),
            MaybeStoppingReason = MaybeStoppingReason0
        ;
            MaybeStoppingReason0 = no,
            % Check whether the output files are present and up-to-date.
            FindTargetFiles(!.Info ^ rci_module_name, TargetFiles, !IO),
            list.foldl3(
                require_recompilation_if_not_up_to_date(RecordedTimestamp),
                TargetFiles,
                MaybeStoppingReason0, MaybeStoppingReason1, !Info, !IO),

            check_imported_modules(Globals, UsedModules,
                MaybeStoppingReason1, MaybeStoppingReason, !Info, !IO)
        )
    ).

:- pred require_recompilation_if_not_up_to_date(timestamp::in, file_name::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

require_recompilation_if_not_up_to_date(RecordedTimestamp, TargetFile,
        !MaybeStoppingReason, !Info, !IO) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        io.file_modification_time(TargetFile, TargetModTimeResult, !IO),
        ( if
            TargetModTimeResult = ok(TargetModTime),
            compare(TargetModTimeCompare, time_t_to_timestamp(TargetModTime),
                RecordedTimestamp),
            TargetModTimeCompare = (>)
        then
            !:MaybeStoppingReason = no
        else
            Reason = recompile_for_output_file_not_up_to_date(TargetFile),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        )
    ).

%---------------------------------------------------------------------------%

:- type used_file
    --->    used_file(
                % XXX document the meanings of these fields.
                module_timestamp,
                list(module_name),
                resolved_used_items,
                list(item_name),
                list(recomp_used_module)
            ).

:- pred read_and_parse_used_file(string::in, string::in, int::in,
    line_context::in, line_posn::in, recomp_parse_term(used_file)::out) is det.

read_and_parse_used_file(UsedFileName, UsedFileString, MaxOffset,
        !.LineContext, !.LinePosn, ParseUsedFile) :-
    % XXX
    % The contents of the *entire .used file* should be a simple large term
    % of a type that is designed to represent its entire contents, since
    % that would allow a single call to io.read to read it all in, allowing us
    % to dispense with pretty much all of the code handling syntax errors
    % in this module. (Since the contents of .used files are machine generated,
    % we do not need to strive to generate user-friendly error messages;
    % a simple indication of the error's presence and location will do.)
    %
    % That type, used_file, should depend *only* on public type definitions,
    % unlike e.g. the representation of sets, which is hidden behind
    % an abstraction barrier. This is because we don't want changes hidden by
    % those abstraction barriers to affect the file format.
    %
    % We could then handle transitions between file format versions either
    %
    % - by trying the parse the contents of the .used file first as a member
    %   of the new type, and if that failed, as a member of the old type, or
    %
    % - by having both formats represented by two different function symbols
    %   in the same type.
    %
    % Alternatively, the .used file could contain two terms, the version
    % number info, and everything else. We could then select the predicate
    % we use to read in everything else based on the version number.

    % Check that the format of the usage file is the current format.
    read_and_parse_used_file_version_number(UsedFileName, UsedFileString,
        MaxOffset, !LineContext, !LinePosn, ParseVersionNumber),
    (
        ParseVersionNumber = rpt_error(Reason),
        ParseUsedFile = rpt_error(Reason)
    ;
        ParseVersionNumber = rpt_ok(_Unit),

        % Find the timestamp of the module the last time it was compiled.
        read_and_parse_module_timestamp(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseTimestamp),
        % Find out whether this module has any inline submodules.
        read_and_parse_inline_submodules(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseInlineSubModules),
        % Parse the used items, which are used for checking for ambiguities
        % with new items.
        read_and_parse_used_items(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseUsedItems),
        read_and_parse_used_classes(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseUsedClasses),
        read_and_parse_used_modules(UsedFileName, UsedFileString,
            MaxOffset, !.LineContext, _, !.LinePosn, _,
            cord.init, ParseUsedModules),
        ( if
            ParseTimestamp = rpt_ok({_ModuleName, ModuleTimestamp}),
            ParseInlineSubModules = rpt_ok(InlineSubModules),
            ParseUsedItems = rpt_ok(UsedItems),
            ParseUsedClasses = rpt_ok(UsedClasses),
            ParseUsedModules = rpt_ok(UsedModules)
        then
            UsedFile = used_file(ModuleTimestamp, InlineSubModules,
                UsedItems, set.to_sorted_list(UsedClasses), UsedModules),
            ParseUsedFile = rpt_ok(UsedFile)
        else
            Reasons1 = project_error_reason(ParseTimestamp),
            Reasons2 = project_error_reason(ParseInlineSubModules),
            Reasons3 = project_error_reason(ParseUsedItems),
            Reasons4 = project_error_reason(ParseUsedClasses),
            Reasons5 = project_error_reason(ParseUsedModules),
            % Since at least one rpt_ok test in the condition has failed,
            % there must be at least one reason.
            % XXX We could report all the syntax errors we found,
            % not just the first.
            FirstReason = list.det_head(Reasons1 ++ Reasons2 ++
                Reasons3 ++ Reasons4 ++ Reasons5),
            ParseUsedFile = rpt_error(FirstReason)
        )
    ).

:- func project_error_reason(recomp_parse_term(T)) = list(recompile_reason).

project_error_reason(rpt_ok(_)) = [].
project_error_reason(rpt_error(Reason)) = [Reason].

:- pred read_and_parse_used_file_version_number(string::in,
    string::in, int::in, line_context::in, line_context::out,
    line_posn::in, line_posn::out, recomp_parse_term(unit)::out) is det.

read_and_parse_used_file_version_number(UsedFileName, UsedFileString,
        MaxOffset, !LineContext, !LinePosn, ParseTerm) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "usage file version number", ReadTerm),
    (
        ReadTerm = rpt_error(Reason),
        ParseTerm = rpt_error(Reason)
    ;
        ReadTerm = rpt_ok(Term),
        ( if
            % XXX ITEM_LIST This term should be more self-descriptive.
            % Instead of the current "2,1.", it should be something like
            % "mercury_smart_recomp_usage(usage_format(2), version_format(1))".
            % We could initially accept both formats when reading in,
            % while generating the new format only.
            Term = term.functor(term.atom(","), [SubTerm1, SubTerm2], _),
            decimal_term_to_int(SubTerm1, used_file_version_number),
            decimal_term_to_int(SubTerm2, version_numbers_version_number)
        then
            ParseTerm = rpt_ok(unit)
        else
            Reason = recompile_for_file_error(UsedFileName,
                [words("invalid version number(s) in file"),
                quote(UsedFileName), suffix("."), nl]),
            ParseTerm = rpt_error(Reason)
        )
    ).

%---------------------%

:- pred read_and_parse_module_timestamp(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    recomp_parse_term({module_name, module_timestamp})::out) is det.

read_and_parse_module_timestamp(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseTerm) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "module timestamp", ReadTerm),
    (
        ReadTerm = rpt_error(Reason),
        ParseTerm = rpt_error(Reason)
    ;
        ReadTerm = rpt_ok(Term),
        parse_module_timestamp(Term, ParseTerm)
    ).

:- pred parse_module_timestamp(term::in,
    recomp_parse_term({module_name, module_timestamp})::out) is det.

parse_module_timestamp(Term, ParseTerm) :-
    conjunction_to_list(Term, Args),
    ( if
        Args = [ModuleNameTerm, SuffixTerm, TimestampTerm | MaybeOtherTerms],
        try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName),
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
                % XXX Does write_module_name_and_used_items
                % still generate "used"?
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
        ModuleTimestamp = module_timestamp(FileKind, Timestamp, RecompAvail),
        ParseTerm = rpt_ok({ModuleName, ModuleTimestamp})
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in module timestamp"),
        ParseTerm = rpt_error(Reason)
    ).

%---------------------%

:- pred read_and_parse_inline_submodules(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    recomp_parse_term(list(module_name))::out) is det.

read_and_parse_inline_submodules(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseSubModules) :-
    % Find out whether this module has any inline submodules.
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "inline submodules", ReadTerm),
    (
        ReadTerm = rpt_error(Reason),
        ParseSubModules = rpt_error(Reason)
    ;
        ReadTerm = rpt_ok(Term),
        ( if
            Term = term.functor(term.atom("sub_modules"), SubModuleTerms, _),
            list.map(try_parse_sym_name_and_no_args,
                SubModuleTerms, SubModules)
        then
            ParseSubModules = rpt_ok(SubModules)
        else
            Reason = recompile_for_syntax_error(get_term_context(Term),
                "error in sub_modules term"),
            ParseSubModules = rpt_error(Reason)
        )
    ).

%---------------------%

:- pred read_and_parse_used_items(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    recomp_parse_term(resolved_used_items)::out) is det.

read_and_parse_used_items(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseUsedItems) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "used items", ReadTerm),
    (
        ReadTerm = rpt_error(Reason),
        ParseUsedItems = rpt_error(Reason)
    ;
        ReadTerm = rpt_ok(Term),
        ( if
            Term = term.functor(term.atom("used_items"), UsedItemTerms, _)
        then
            UsedItems0 = init_item_id_set(map.init, map.init, map.init),
            ReasonsCord0 = cord.init,
            list.foldl2(parse_used_item_set, UsedItemTerms,
                UsedItems0, UsedItems, ReasonsCord0, ReasonsCord),
            Reasons = cord.list(ReasonsCord),
            (
                Reasons = [],
                ParseUsedItems = rpt_ok(UsedItems)
            ;
                Reasons = [HeadReason | _],
                ParseUsedItems = rpt_error(HeadReason)
            )
        else
            Reason = recompile_for_syntax_error(get_term_context(Term),
                "error in used items"),
            ParseUsedItems = rpt_error(Reason)
        )
    ).

:- pred parse_used_item_set(term::in,
    resolved_used_items::in, resolved_used_items::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_used_item_set(Term, !UsedItems, !Reasons) :-
    ( if
        Term = term.functor(term.atom(ItemTypeStr), ItemTerms, _),
        string_to_item_type(ItemTypeStr, ItemType)
    then
        (
            ( ItemType = type_abstract_item
            ; ItemType = type_body_item
            ; ItemType = inst_item
            ; ItemType = mode_item
            ; ItemType = typeclass_item
            ),
            list.foldl2(parse_simple_item, ItemTerms,
                map.init, SimpleItems, cord.init, ItemReasons),
            ( if cord.is_empty(ItemReasons) then
                update_simple_item_set(ItemType, SimpleItems, !UsedItems)
            else
                !:Reasons = !.Reasons ++ ItemReasons
            )
        ;
            ( ItemType = predicate_item
            ; ItemType = function_item
            ),
            list.foldl2(parse_pred_or_func_item, ItemTerms,
                map.init, PredOrFuncItems, cord.init, ItemReasons),
            ( if cord.is_empty(ItemReasons) then
                update_pred_or_func_set(ItemType, PredOrFuncItems, !UsedItems)
            else
                !:Reasons = !.Reasons ++ ItemReasons
            )
        ;
            ItemType = functor_item,
            list.foldl2(parse_functor_item, ItemTerms,
                map.init, CtorItems, cord.init, ItemReasons),
            ( if cord.is_empty(ItemReasons) then
                !UsedItems ^ functors := CtorItems
            else
                !:Reasons = !.Reasons ++ ItemReasons
            )
        ;
            ( ItemType = mutable_item
            ; ItemType = foreign_proc_item
            ),
            Reason = recompile_for_syntax_error(get_term_context(Term),
                "error in used items: unknown item type: " ++ ItemTypeStr),
            !:Reasons = cord.snoc(!.Reasons, Reason)
        )
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in used items"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

:- pred parse_simple_item(term::in, simple_item_set::in, simple_item_set::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_simple_item(Term, !Set, !Reasons) :-
    ( if
        Term = term.functor(term.atom("-"), [NameArityTerm, MatchesTerm], _),
        parse_unqualified_name_and_arity(NameArityTerm, SymName, Arity)
    then
        Name = unqualify_name(SymName),
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.foldl2(parse_simple_item_match, MatchTermList,
            map.init, Matches, cord.init, TermReasons),
        ( if cord.is_empty(TermReasons) then
            map.det_insert(Name - Arity, Matches, !Set)
        else
            !:Reasons = !.Reasons ++ TermReasons
        )
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in simple items"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

:- pred parse_simple_item_match(term::in,
    map(module_qualifier, module_name)::in,
    map(module_qualifier, module_name)::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_simple_item_match(Term, !ItemMap, !Reasons) :-
    ( if
        % XXX This defaulty representation (the absence of an arrow
        % meaning there is no explicit qualifier term) is bad design.
        % There should *always* be an arrow (or some other fixed functor),
        % with an explicit representation of the qualifier being the same
        % as the module name.
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
        map.det_insert(Qualifier, ModuleName, !ItemMap)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in simple item match"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

:- pred parse_pred_or_func_item(term::in,
    resolved_pred_or_func_set::in, resolved_pred_or_func_set::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_pred_or_func_item(Term, !Set, !Reasons) :-
    parse_resolved_item_set(parse_pred_or_func_item_match, Term,
        !Set, !Reasons).

:- pred parse_pred_or_func_item_match(term::in,
    resolved_pred_or_func_map::in, resolved_pred_or_func_map::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_pred_or_func_item_match(Term, !Items, !Reasons) :-
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
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

:- pred parse_functor_item(term::in,
    resolved_functor_set::in, resolved_functor_set::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_functor_item(Term, !Set, !Reasons) :-
    parse_resolved_item_set(parse_functor_matches, Term, !Set, !Reasons).

:- pred parse_functor_matches(term::in,
    resolved_functor_map::in, resolved_functor_map::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_functor_matches(Term, !Map, !Reasons) :-
    ( if
        Term = term.functor(term.atom("=>"), [QualifierTerm, MatchesTerm], _),
        try_parse_sym_name_and_no_args(QualifierTerm, Qualifier)
    then
        conjunction_to_list(MatchesTerm, MatchesTerms),
        list.foldl2(parse_resolved_functor, MatchesTerms,
            [], RevMatches, cord.init, TermReasons),
        ( if cord.is_empty(TermReasons) then
            list.reverse(RevMatches, Matches),
            map.det_insert(Qualifier, set.list_to_set(Matches), !Map)
        else
            !:Reasons = !.Reasons ++ TermReasons
        )
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in functor match"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

:- pred parse_resolved_functor(term::in,
    list(resolved_functor)::in, list(resolved_functor)::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_resolved_functor(Term, !RevCtors, !Reasons) :-
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
            Arity),
        !:RevCtors = [Ctor | !.RevCtors]
    else if
        Term = term.functor(term.atom("ctor"), [NameArityTerm], _),
        parse_unqualified_name_and_arity(NameArityTerm, TypeName, TypeArity)
    then
        Ctor = resolved_functor_constructor(item_name(TypeName, TypeArity)),
        !:RevCtors = [Ctor | !.RevCtors]
    else if
        Term = term.functor(term.atom("field"),
            [TypeNameArityTerm, ConsNameArityTerm], _),
        parse_unqualified_name_and_arity(TypeNameArityTerm,
            TypeName, TypeArity),
        parse_unqualified_name_and_arity(ConsNameArityTerm,
            ConsName, ConsArity)
    then
        Ctor = resolved_functor_field(item_name(TypeName, TypeArity),
            item_name(ConsName, ConsArity)),
        !:RevCtors = [Ctor | !.RevCtors]
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in functor match"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

%---------------------%

:- type parse_resolved_item_matches(T) ==
    pred(term, resolved_item_map(T), resolved_item_map(T),
        cord(recompile_reason), cord(recompile_reason)).
:- inst parse_resolved_item_matches == (pred(in, in, out, in, out) is det).

:- pred parse_resolved_item_set(
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
    term::in, resolved_item_set(T)::in, resolved_item_set(T)::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_resolved_item_set(ParseMatches, Term, !Set, !Reasons) :-
    ( if
        Term = term.functor(term.atom("-"), [NameTerm, MatchesTerm], _),
        NameTerm = term.functor(term.atom(Name), [], _)
    then
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.foldl2(parse_resolved_item_arity_matches(ParseMatches),
            MatchTermList, [], RevMatches, !Reasons),
        list.reverse(RevMatches, Matches),
        map.det_insert(Name, Matches, !Set)
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in resolved item matches"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

:- pred parse_resolved_item_arity_matches(
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches), term::in,
    list(pair(arity, resolved_item_map(T)))::in,
    list(pair(arity, resolved_item_map(T)))::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_resolved_item_arity_matches(ParseMatches, Term,
        !RevArityMatchMaps, !Reasons) :-
    ( if
        Term = term.functor(term.atom("-"), [ArityTerm, MatchesTerm], _),
        decimal_term_to_int(ArityTerm, Arity0),
        conjunction_to_list(MatchesTerm, MatchTermList)
    then
        Arity = Arity0,
        list.foldl2(
            ( pred(MatchTerm::in, Map0::in, Map::out,
                    Reasons0::in, Reasons::out) is det :-
                ParseMatches(MatchTerm, Map0, Map, Reasons0, Reasons)
            ),
            MatchTermList, map.init, MatchMap, cord.init, TermReasons),
        ( if cord.is_empty(TermReasons) then
            !:RevArityMatchMaps = [Arity - MatchMap | !.RevArityMatchMaps]
        else
            !:Reasons = !.Reasons ++ TermReasons
        )
    else
        Reason = recompile_for_syntax_error(get_term_context(Term),
            "error in resolved item matches"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

%---------------------%

:- pred read_and_parse_used_classes(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    recomp_parse_term(set(item_name))::out) is det.

read_and_parse_used_classes(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseUsedClasses) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "used classes", ReadTerm),
    (
        ReadTerm = rpt_error(Reason),
        ParseUsedClasses = rpt_error(Reason)
    ;
        ReadTerm = rpt_ok(Term),
        ( if
            Term = term.functor(term.atom("used_classes"), UsedClassTerms, _)
            % XXX The format of the .used file should put UsedClassTerms
            % into a list, to give the used_classes functor a fixed arity.
        then
            list.foldl2(parse_name_and_arity_item_add_to_set,
                UsedClassTerms, set.init, UsedClasses, cord.init, ReasonsCord),
            Reasons = cord.list(ReasonsCord),
            (
                Reasons = [],
                ParseUsedClasses = rpt_ok(UsedClasses)
            ;
                Reasons = [HeadReason | _],
                ParseUsedClasses = rpt_error(HeadReason)
            )
        else
            Context = get_term_context(Term),
            Reason = recompile_for_syntax_error(Context,
                "error in used_typeclasses term"),
            ParseUsedClasses = rpt_error(Reason)
        )
    ).

:- pred parse_name_and_arity_item_add_to_set(term::in,
    set(item_name)::in, set(item_name)::out,
    cord(recompile_reason)::in, cord(recompile_reason)::out) is det.

parse_name_and_arity_item_add_to_set(Term, !UsedClasses, !Reasons) :-
    ( if parse_unqualified_name_and_arity(Term, ClassName, ClassArity) then
        UsedClass = item_name(ClassName, ClassArity),
        set.insert(UsedClass, !UsedClasses)
    else
        Context = get_term_context(Term),
        Reason = recompile_for_syntax_error(Context,
            "error in used_typeclasses term"),
        !:Reasons = cord.snoc(!.Reasons, Reason)
    ).

%---------------------%

:- type recomp_used_module
    --->    recomp_used_module(
                module_name,
                module_timestamp,
                maybe(version_numbers)
            ).

:- pred read_and_parse_used_modules(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    cord(recomp_used_module)::in,
    recomp_parse_term(list(recomp_used_module))::out) is det.

read_and_parse_used_modules(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, !.UsedModulesCord, ParseUsedModules) :-
    read_term_check_for_error_or_eof(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, "used items list", ReadTerm),
    (
        ReadTerm = rpt_ok(Term),
        % There should always be an item `done.' at the end of the list
        % of modules to check. We use this to make sure that the writing
        % of the `.used' file was not interrupted.
        % XXX See the comment in read_and_parse_used_file for a simpler way
        % of doing that.
        ( if
            Term = term.functor(term.atom("done"), [], _)
        then
            % XXX We should check that we are at the end-of-file.
            ParseUsedModules = rpt_ok(cord.list(!.UsedModulesCord))
        else
            % XXX This defaulty representation (the absence of an arrow
            % meaning there is no used items term) is bad design.
            % There should *always* be an arrow (or some other fixed functor),
            % with an explicit representation of an empty set of used items.
            ( if
                Term = term.functor(term.atom("=>"),
                    [TimestampTerm0, UsedItemsTerm], _)
            then
                TimestampTerm = TimestampTerm0,
                parse_version_numbers(UsedItemsTerm, MaybeUsedItems),
                (
                    MaybeUsedItems = ok1(VersionNumbers),
                    ParseVersionNumbers = rpt_ok(yes(VersionNumbers))
                ;
                    MaybeUsedItems = error1(Specs),
                    ParseVersionNumbers =
                        rpt_error(recompile_for_unreadable_used_items(Specs))
                )
            else
                TimestampTerm = Term,
                ParseVersionNumbers = rpt_ok(no)
            ),
            parse_module_timestamp(TimestampTerm, ParseModuleTimestamp),
            (
                ParseModuleTimestamp =
                    rpt_ok({ImportedModuleName, ModuleTimestamp}),
                (
                    ParseVersionNumbers = rpt_ok(MaybeVersionNumbers),
                    UsedModule = recomp_used_module(ImportedModuleName,
                        ModuleTimestamp, MaybeVersionNumbers),
                    !:UsedModulesCord =
                        cord.snoc(!.UsedModulesCord, UsedModule),
                    read_and_parse_used_modules(FileName, FileString,
                        MaxOffset, !LineContext, !LinePosn, !.UsedModulesCord,
                        ParseUsedModules)
                ;
                    ParseVersionNumbers = rpt_error(Reason),
                    ParseUsedModules = rpt_error(Reason)
                )
            ;
                ParseModuleTimestamp = rpt_error(Reason),
                ParseUsedModules = rpt_error(Reason)
            )
        )
    ;
        ReadTerm = rpt_error(Reason),
        ParseUsedModules = rpt_error(Reason)
    ).

%---------------------------------------------------------------------------%

:- pred check_imported_modules(globals::in, list(recomp_used_module)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_modules(_Globals, [], !MaybeStoppingReason, !Info, !IO).
check_imported_modules(Globals, [HeadUsedModule | TailUsedModules],
        !MaybeStoppingReason, !Info, !IO) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        check_imported_module(Globals, HeadUsedModule,
            !:MaybeStoppingReason, !Info, !IO),
        check_imported_modules(Globals, TailUsedModules,
            !MaybeStoppingReason, !Info, !IO)
    ).

    % Check whether the interface file read for a module in the last
    % compilation has changed, and if so whether the items have changed
    % in a way which should cause a recompilation.
    %
:- pred check_imported_module(globals::in, recomp_used_module::in,
    maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_module(Globals, UsedModule, MaybeStoppingReason, !Info, !IO) :-
    UsedModule = recomp_used_module(ImportedModuleName, ModuleTimestamp,
        MaybeUsedVersionNumbers),
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
    ( if
        % If we are checking a nested submodule, don't re-read interface files
        % read for other modules checked during this compilation.
        % XXX We restrict this optimization to nested submodules?
        !.Info ^ rci_is_inline_sub_module = yes,
        find_read_module_some_int(HaveReadModuleMaps,
            ImportedModuleName, IntFileKind,
            do_return_timestamp, FileNamePrime, MaybeNewTimestampPrime,
            ParseTreeSomeIntPrime, SpecsPrime, ErrorsPrime)
    then
        Recorded = bool.yes,
        FileName = FileNamePrime,
        MaybeNewTimestamp = MaybeNewTimestampPrime,
        ParseTreeSomeInt = ParseTreeSomeIntPrime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime
    else
        Recorded = bool.no,
        int_file_kind_to_extension(IntFileKind, IntFileExt, _OtherExt),
        read_module_some_int(Globals,
            "Reading " ++ IntFileExt ++ " file for module",
            do_not_ignore_errors, do_search, ImportedModuleName, IntFileKind,
            FileName, dont_read_module_if_match(RecordedTimestamp),
            MaybeNewTimestamp, ParseTreeSomeInt, Specs, Errors, !IO)
    ),
    ( if set.is_empty(Errors) then
        ( if
            MaybeNewTimestamp = yes(NewTimestamp),
            NewTimestamp \= RecordedTimestamp
        then
            (
                Recorded = no,
                record_read_file_some_int(ImportedModuleName, FileName,
                    ModuleTimestamp ^ mts_timestamp := NewTimestamp,
                    ParseTreeSomeInt, Specs, Errors, !Info)
            ;
                Recorded = yes
            ),
            ( if
                MaybeUsedVersionNumbers = yes(UsedVersionNumbers),
                get_version_numbers_from_parse_tree_some_int(ParseTreeSomeInt,
                    VersionNumbers)
            then
                check_module_used_items(ImportedModuleName, RecompAvail,
                    RecordedTimestamp, UsedVersionNumbers, VersionNumbers,
                    ParseTreeSomeInt, MaybeStoppingReason, !Info)
            else
                Reason = recompile_for_module_changed(FileName),
                record_recompilation_reason(Reason, MaybeStoppingReason, !Info)
            )
        else
            % We are throwing away Specs. Since it should be a repeat of the
            % errors we saw when the file was first read in, this should be OK.
            MaybeStoppingReason = no
        )
    else
        % We are throwing away Specs, even though some of its elements
        % could illuminate the cause of the problem. XXX Is this OK?
        Pieces = [words("error reading file"), quote(FileName),
            suffix("."), nl],
        Reason = recompile_for_file_error(FileName, Pieces),
        MaybeStoppingReason = yes(Reason)
    ).

%---------------------------------------------------------------------------%

:- pred check_module_used_items(module_name::in, recomp_avail::in,
    timestamp::in, version_numbers::in, version_numbers::in,
    parse_tree_some_int::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_module_used_items(ModuleName, RecompAvail, OldTimestamp,
        UsedVersionNumbers, NewVersionNumbers, ParseTreeSomeInt,
        !:MaybeStoppingReason, !Info) :-
    UsedVersionNumbers = version_numbers(UsedItemVersionNumbers,
        UsedInstanceVersionNumbers),
    NewVersionNumbers = version_numbers(NewItemVersionNumbers,
        NewInstanceVersionNumbers),

    !:MaybeStoppingReason = no,
    % Check whether any of the items which were used have changed.
    list.foldl2(
        check_item_version_numbers(ModuleName, UsedItemVersionNumbers,
            NewItemVersionNumbers),
        [type_abstract_item, type_body_item, inst_item, mode_item,
            typeclass_item, predicate_item, function_item],
        !MaybeStoppingReason, !Info),

    % Check whether added or modified items could cause name resolution
    % ambiguities with items which were used.
    get_ambiguity_checkables_parse_tree_some_int(ParseTreeSomeInt,
        Checkables),
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls),
    check_items_for_ambiguities(
        check_type_defn_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedItemVersionNumbers),
        ItemTypeDefns, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_inst_defn_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedItemVersionNumbers),
        ItemInstDefns, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_mode_defn_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedItemVersionNumbers),
        ItemModeDefns, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_typeclass_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedItemVersionNumbers),
        ItemTypeClasses, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_pred_decl_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedItemVersionNumbers),
        ItemPredDecls, !MaybeStoppingReason, !Info),

    % Check whether any instances of used typeclasses have been added,
    % removed or changed.
    check_instance_version_numbers(ModuleName, UsedInstanceVersionNumbers,
        NewInstanceVersionNumbers, !MaybeStoppingReason, !Info),

    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
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
            AddedInstancesList = [],
            !:MaybeStoppingReason = no
        ;
            AddedInstancesList = [FirstAddedInstance | _],
            Reason1 = recompile_for_changed_or_added_instance(ModuleName,
                FirstAddedInstance),
            record_recompilation_reason(Reason1, !:MaybeStoppingReason, !Info)
        )
    ).

:- func make_item_id(module_name, item_type, pair(string, arity)) = item_id.

make_item_id(Module, ItemType, Name - Arity) =
    item_id(ItemType, item_name(qualified(Module, Name), Arity)).

%---------------------------------------------------------------------------%

:- pred check_item_version_numbers(module_name::in, item_version_numbers::in,
    item_version_numbers::in, item_type::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_version_numbers(ModuleName, UsedVersionNumbers, NewVersionNumbers,
        ItemType, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        NewItemTypeVersionNumbers = extract_ids(NewVersionNumbers, ItemType),
        map.foldl2(
            check_item_version_number(ModuleName,
                NewItemTypeVersionNumbers, ItemType),
            extract_ids(UsedVersionNumbers, ItemType),
            no, !:MaybeStoppingReason, !Info)
    ).

:- pred check_item_version_number(module_name::in, version_number_map::in,
    item_type::in, pair(string, arity)::in, version_number::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_version_number(ModuleName, NewItemTypeVersionNumbers, ItemType,
        NameArity, UsedVersionNumber, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        ( if
            map.search(NewItemTypeVersionNumbers, NameArity, NewVersionNumber)
        then
            ( if NewVersionNumber = UsedVersionNumber then
                !:MaybeStoppingReason = no
            else
                ItemId = make_item_id(ModuleName, ItemType, NameArity),
                Reason = recompile_for_changed_item(ItemId),
                record_recompilation_reason(Reason, !:MaybeStoppingReason,
                    !Info)
            )
        else
            ItemId = make_item_id(ModuleName, ItemType, NameArity),
            Reason = recompile_for_removed_item(ItemId),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        )
    ).

:- pred check_instance_version_numbers(module_name::in,
    instance_version_numbers::in, instance_version_numbers::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_instance_version_numbers(ModuleName, UsedInstanceVersionNumbers,
        NewInstanceVersionNumbers, !MaybeStoppingReason, !Info) :-
    map.foldl2(
        check_instance_version_number(ModuleName, NewInstanceVersionNumbers),
        UsedInstanceVersionNumbers, !MaybeStoppingReason, !Info).

:- pred check_instance_version_number(module_name::in,
    instance_version_numbers::in, item_name::in, version_number::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_instance_version_number(ModuleName, NewInstanceVersionNumbers,
        ClassId, UsedVersionNumber, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        ( if
            map.search(NewInstanceVersionNumbers, ClassId, NewVersionNumber)
        then
            ( if UsedVersionNumber = NewVersionNumber then
                !:MaybeStoppingReason = no
            else
                Reason = recompile_for_changed_or_added_instance(ModuleName,
                    ClassId),
                record_recompilation_reason(Reason, !:MaybeStoppingReason,
                    !Info)
            )
        else
            Reason = recompile_for_removed_instance(ModuleName, ClassId),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        )
    ).

%---------------------------------------------------------------------------%

:- pred check_items_for_ambiguities(
    pred(T, maybe(recompile_reason), maybe(recompile_reason),
        recompilation_check_info, recompilation_check_info)
    ::in(pred(in, in, out, in, out) is det),
    list(T)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_items_for_ambiguities(_CheckPred, [], !MaybeStoppingReason, !Info).
check_items_for_ambiguities(CheckPred, [HeadItem | TailItems],
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        CheckPred(HeadItem, no, !:MaybeStoppingReason, !Info),
        check_items_for_ambiguities(CheckPred, TailItems,
            !MaybeStoppingReason, !Info)
    ).

%---------------------%

:- pred check_type_defn_info_for_ambiguities(recomp_avail::in, timestamp::in,
    item_version_numbers::in, item_type_defn_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_type_defn_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemTypeDefn, !MaybeStoppingReason, !Info) :-
    ItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams, TypeBody,
        _, _, _),
    list.length(TypeParams, TypeArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
        VersionNumbers, type_abstract_item, TypeSymName, TypeArity,
        NeedsCheck, !MaybeStoppingReason, !Info),
    (
        NeedsCheck = yes,
        TypeCtor = type_ctor(TypeSymName, TypeArity),
        check_type_defn_ambiguity_with_functor(RecompAvail,
            TypeCtor, TypeBody, !MaybeStoppingReason, !Info)
    ;
        NeedsCheck = no
    ).

%---------------------%

:- pred check_inst_defn_info_for_ambiguities(recomp_avail::in, timestamp::in,
    item_version_numbers::in, item_inst_defn_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_inst_defn_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemInstDefn, !MaybeStoppingReason, !Info) :-
    % XXX IFTC Do we need to check _MaybeForTypeCtor?
    ItemInstDefn = item_inst_defn_info(InstSymName, InstParams,
        _MaybeForTypeCtor, _, _, _, _),
    list.length(InstParams, InstArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp, VersionNumbers,
        inst_item, InstSymName, InstArity, _, !MaybeStoppingReason, !Info).

%---------------------%

:- pred check_mode_defn_info_for_ambiguities(recomp_avail::in, timestamp::in,
    item_version_numbers::in, item_mode_defn_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_mode_defn_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemModeDefn, !MaybeStoppingReason, !Info) :-
    ItemModeDefn = item_mode_defn_info(ModeSymName, ModeParams, _, _, _, _),
    list.length(ModeParams, ModeArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp, VersionNumbers,
        mode_item, ModeSymName, ModeArity, _, !MaybeStoppingReason, !Info).

%---------------------%

:- pred check_typeclass_info_for_ambiguities(recomp_avail::in,
    timestamp::in, item_version_numbers::in, item_typeclass_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_typeclass_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemTypeClass, !MaybeStoppingReason, !Info) :-
    ItemTypeClass = item_typeclass_info(TypeClassSymName, TypeClassParams,
        _, _, Interface, _, _, _),
    list.length(TypeClassParams, TypeClassArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
        VersionNumbers, typeclass_item, TypeClassSymName, TypeClassArity,
        NeedsCheck, !MaybeStoppingReason, !Info),
    ( if
        NeedsCheck = yes,
        Interface = class_interface_concrete(ClassDecls)
    then
        list.foldl2(
            check_class_decl_for_ambiguities(RecompAvail, OldTimestamp,
                VersionNumbers),
            ClassDecls, !MaybeStoppingReason, !Info)
    else
        true
    ).

:- pred check_class_decl_for_ambiguities(recomp_avail::in,
    timestamp::in, item_version_numbers::in, class_decl::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_class_decl_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        Decl, !MaybeStoppingReason, !Info) :-
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(MethodName, PredOrFunc,
            MethodArgs, MethodWithType, _, _, _, _, _, _, _, _),
        check_for_pred_or_func_item_ambiguity(yes, RecompAvail,
            OldTimestamp, VersionNumbers, PredOrFunc, MethodName,
            MethodArgs, MethodWithType, !MaybeStoppingReason, !Info)
    ;
        Decl = class_decl_mode(_)
    ).

%---------------------%

:- pred check_pred_decl_info_for_ambiguities(recomp_avail::in,
    timestamp::in, item_version_numbers::in, item_pred_decl_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_pred_decl_info_for_ambiguities(RecompAvail, OldTimestamp,
        VersionNumbers, ItemPredDecl, !MaybeStoppingReason, !Info) :-
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc, Args,
        WithType, _, _, _, _, _, _, _, _, _, _),
    check_for_pred_or_func_item_ambiguity(no, RecompAvail, OldTimestamp,
        VersionNumbers, PredOrFunc, PredSymName, Args, WithType,
        !MaybeStoppingReason, !Info).

%---------------------------------------------------------------------------%

:- pred check_for_simple_item_ambiguity(recomp_avail::in,
    timestamp::in, item_version_numbers::in, item_type::in(simple_item),
    sym_name::in, arity::in, bool::out,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity(RecompAvail, UsedFileTimestamp,
        VersionNumbers, ItemType, SymName, Arity, NeedsCheck,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_),
        % Since we have found a reason to recompile, we don't need to look for
        % more reasons.
        NeedsCheck = no
    ;
        !.MaybeStoppingReason = no,
        ( if
            item_is_new_or_changed(UsedFileTimestamp, VersionNumbers,
                ItemType, SymName, Arity)
        then
            NeedsCheck = yes,
            UsedItems = !.Info ^ rci_used_items,
            UsedItemMap = extract_simple_item_set(UsedItems, ItemType),
            Name = unqualify_name(SymName),
            ( if map.search(UsedItemMap, Name - Arity, MatchingQualifiers) then
                map.foldl2(
                    check_for_simple_item_ambiguity_2(ItemType,
                        RecompAvail, SymName, Arity),
                    MatchingQualifiers, no, !:MaybeStoppingReason, !Info)
            else
                !:MaybeStoppingReason = no
            )
        else
            NeedsCheck = no,
            !:MaybeStoppingReason = no
        )
    ).

:- pred check_for_simple_item_ambiguity_2(item_type::in,
    recomp_avail::in, sym_name::in, arity::in,
    module_qualifier::in, module_name::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity_2(ItemType, RecompAvail, SymName, Arity,
        OldModuleQualifier, OldMatchingModuleName,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
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
            !:MaybeStoppingReason = no
        else if
            QualifiedName = module_qualify_name(OldModuleQualifier, Name),
            partial_sym_name_matches_full(QualifiedName, SymName),
            not SymName = qualified(OldMatchingModuleName, _)
        then
            OldMatchingName = qualified(OldMatchingModuleName, Name),
            Reason = recompile_for_item_ambiguity(
                item_id(ItemType, item_name(SymName, Arity)),
                [item_id(ItemType, item_name(OldMatchingName, Arity))]),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
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

:- pred check_for_pred_or_func_item_ambiguity(bool::in,
    recomp_avail::in, timestamp::in, item_version_numbers::in,
    pred_or_func::in, sym_name::in,
    list(type_and_mode)::in, maybe(mer_type)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity(NeedsCheck, RecompAvail, OldTimestamp,
        VersionNumbers, PredOrFunc, SymName, Args, WithType,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
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
                list.foldl2(
                    check_for_pred_or_func_item_ambiguity_1(WithType,
                        ItemType, RecompAvail, SymName, Arity),
                    MatchingArityList, no, !:MaybeStoppingReason, !Info)
            else
                !:MaybeStoppingReason = no
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
                ResolvedFunctor = resolved_functor_pred_or_func(PredId,
                    ModuleName, PredOrFunc, Arity),
                check_functor_ambiguities_by_name(RecompAvail, SymName,
                    AritiesToMatch, ResolvedFunctor,
                    !MaybeStoppingReason, !Info)
            ;
                SymName = unqualified(_),
                unexpected($pred, "unqualified predicate name")
            )
        else
            !:MaybeStoppingReason = no
        )
    ).

:- pred check_for_pred_or_func_item_ambiguity_1(maybe(mer_type)::in,
    item_type::in, recomp_avail::in, sym_name::in, arity::in,
    pair(arity, map(sym_name, set(pair(pred_id, module_name))))::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_1(WithType, ItemType, RecompAvail,
        SymName, Arity, MatchArity - MatchingQualifiers,
        !MaybeStoppingReason, !Info) :-
    ( if
        (
            WithType = yes(_),
            MatchArity >= Arity
        ;
            WithType = no,
            MatchArity = Arity
        )
    then
        map.foldl2(
            check_for_pred_or_func_item_ambiguity_2(ItemType, RecompAvail,
                SymName, MatchArity),
            MatchingQualifiers, !MaybeStoppingReason, !Info)
    else
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity_2(item_type::in,
    recomp_avail::in, sym_name::in, arity::in, module_qualifier::in,
    set(pair(pred_id, module_name))::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_2(ItemType, RecompAvail,
        SymName, Arity, OldModuleQualifier, OldMatchingModuleNames,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
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
            !:MaybeStoppingReason = no
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
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
    ).

    % Go over the constructors for a type which has changed and check whether
    % any of them could create an ambiguity with functors used during the
    % last compilation.
    %
:- pred check_type_defn_ambiguity_with_functor(recomp_avail::in,
    type_ctor::in, type_defn::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_type_defn_ambiguity_with_functor(RecompAvail, TypeCtor, TypeDefn,
        !MaybeStoppingReason, !Info) :-
    (
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        )
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(_MaybeSuperType, Ctors, _, _),
        list.foldl2(check_functor_ambiguities(RecompAvail, TypeCtor),
            one_or_more_to_list(Ctors), !MaybeStoppingReason, !Info)
    ).

:- pred check_functor_ambiguities(recomp_avail::in, type_ctor::in,
    constructor::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities(RecompAvail, TypeCtor, Ctor,
        !MaybeStoppingReason, !Info) :-
    Ctor = ctor(_, _, Name, Args, Arity, _),
    TypeCtorItem = type_ctor_to_item_name(TypeCtor),
    ResolvedCtor = resolved_functor_constructor(TypeCtorItem),
    check_functor_ambiguities_by_name(RecompAvail, Name,
        match_arity_exact(Arity), ResolvedCtor, !MaybeStoppingReason, !Info),
    list.foldl2(
        check_field_ambiguities(RecompAvail,
            resolved_functor_field(TypeCtorItem, item_name(Name, Arity))),
        Args, !MaybeStoppingReason, !Info).

:- pred check_field_ambiguities(recomp_avail::in,
    resolved_functor::in, constructor_arg::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_field_ambiguities(RecompAvail, ResolvedCtor, CtorArg,
        !MaybeStoppingReason, !Info) :-
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
            match_arity_exact(1), ResolvedCtor, !MaybeStoppingReason, !Info),
        check_functor_ambiguities_by_name(RecompAvail, UpdateFuncName,
            match_arity_exact(2), ResolvedCtor, !MaybeStoppingReason, !Info)
    ).

    % Predicates and functions used as functors can match any arity
    % less than or equal to the predicate or function's arity.
:- type functor_match_arity
    --->    match_arity_exact(arity)
    ;       match_arity_less_than_or_equal(arity)
    ;       match_arity_any.

:- pred check_functor_ambiguities_by_name(recomp_avail::in,
    sym_name::in, functor_match_arity::in, resolved_functor::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_by_name(RecompAvail, Name, MatchArity, ResolvedCtor,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        UsedItems = !.Info ^ rci_used_items,
        UnqualName = unqualify_name(Name),
        UsedCtors = UsedItems ^ functors,
        ( if map.search(UsedCtors, UnqualName, UsedCtorAL) then
            check_functor_ambiguities_2(RecompAvail, Name, MatchArity,
                ResolvedCtor, UsedCtorAL, no, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
    ).

:- pred check_functor_ambiguities_2(recomp_avail::in, sym_name::in,
    functor_match_arity::in, resolved_functor::in,
    assoc_list(arity, resolved_functor_map)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_2(_, _, _, _, [], !MaybeStoppingReason, !Info).
check_functor_ambiguities_2(RecompAvail, Name, MatchArity, ResolvedCtor,
        [Arity - UsedCtorMap | UsedCtorAL], !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
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
            map.foldl2(
                check_functor_ambiguity(RecompAvail, Name, Arity, ResolvedCtor),
                UsedCtorMap, no, !:MaybeStoppingReason, !Info)
        ;
            Check = no
        ),
        (
            Continue = yes,
            check_functor_ambiguities_2(RecompAvail, Name, MatchArity,
                ResolvedCtor, UsedCtorAL, !MaybeStoppingReason, !Info)
        ;
            Continue = no
        )
    ).

:- pred check_functor_ambiguity(recomp_avail::in,
    sym_name::in, arity::in, resolved_functor::in,
    module_qualifier::in, set(resolved_functor)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguity(RecompAvail, SymName, Arity, ResolvedCtor,
        OldModuleQualifier, OldResolvedCtors, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
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
            !:MaybeStoppingReason = no
        else if
            Name = unqualify_name(SymName),
            OldName = module_qualify_name(OldModuleQualifier, Name),
            partial_sym_name_matches_full(OldName, SymName),
            not set.member(ResolvedCtor, OldResolvedCtors)
        then
            OldModuleQualName = module_qualify_name(OldModuleQualifier, Name),
            Reason = recompile_for_functor_ambiguity(OldModuleQualName, Arity,
                ResolvedCtor, set.to_sorted_list(OldResolvedCtors)),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
    ).

%---------------------------------------------------------------------------%

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

:- pred record_read_file_some_int(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_some_int::in, list(error_spec)::in,
    read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_some_int(ModuleName, FileName, ModuleTimestamp,
        ParseTreeSomeInt, Specs, Errors, !Info) :-
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    HaveReadModuleMaps0 = !.Info ^ rci_have_read_module_maps,
    (
        ParseTreeSomeInt = parse_tree_some_int0(ParseTreeInt0),
        HRMM0 = HaveReadModuleMaps0 ^ hrmm_int0,
        ReadResult = have_successfully_read_module(FileName, yes(Timestamp),
            ParseTreeInt0, Specs, Errors),
        map.set(ModuleName, ReadResult, HRMM0, HRMM),
        HaveReadModuleMaps = HaveReadModuleMaps0 ^ hrmm_int0 := HRMM
    ;
        ParseTreeSomeInt = parse_tree_some_int1(ParseTreeInt1),
        HRMM0 = HaveReadModuleMaps0 ^ hrmm_int1,
        ReadResult = have_successfully_read_module(FileName, yes(Timestamp),
            ParseTreeInt1, Specs, Errors),
        map.set(ModuleName, ReadResult, HRMM0, HRMM),
        HaveReadModuleMaps = HaveReadModuleMaps0 ^ hrmm_int1 := HRMM
    ;
        ParseTreeSomeInt = parse_tree_some_int2(ParseTreeInt2),
        HRMM0 = HaveReadModuleMaps0 ^ hrmm_int2,
        ReadResult = have_successfully_read_module(FileName, yes(Timestamp),
            ParseTreeInt2, Specs, Errors),
        map.set(ModuleName, ReadResult, HRMM0, HRMM),
        HaveReadModuleMaps = HaveReadModuleMaps0 ^ hrmm_int2 := HRMM
    ;
        ParseTreeSomeInt = parse_tree_some_int3(ParseTreeInt3),
        HRMM0 = HaveReadModuleMaps0 ^ hrmm_int3,
        ReadResult = have_successfully_read_module(FileName, yes(Timestamp),
            ParseTreeInt3, Specs, Errors),
        map.set(ModuleName, ReadResult, HRMM0, HRMM),
        HaveReadModuleMaps = HaveReadModuleMaps0 ^ hrmm_int3 := HRMM
    ),
    !Info ^ rci_have_read_module_maps := HaveReadModuleMaps.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- type recomp_parse_term(T)
    --->    rpt_ok(T)
    ;       rpt_error(recompile_reason).

:- pred read_term_check_for_error_or_eof(string::in,
    string::in, int::in, line_context::in, line_context::out,
    line_posn::in, line_posn::out, string::in, recomp_parse_term(term)::out)
    is det.

read_term_check_for_error_or_eof(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, ItemName, ReadTerm) :-
    parser.read_term_from_linestr(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, TermResult),
    (
        TermResult = term(_, Term),
        ReadTerm = rpt_ok(Term)
    ;
        TermResult = error(Message, LineNumber),
        Context = term.context(FileName, LineNumber),
        ReadTerm = rpt_error(recompile_for_syntax_error(Context, Message))
    ;
        TermResult = eof,
        !.LineContext = line_context(LineNumber, _OffsetAtStartOfLine),
        Context = term.context(FileName, LineNumber),
        Message = "unexpected end of file, expected " ++ ItemName ++ ".",
        ReadTerm = rpt_error(recompile_for_syntax_error(Context, Message))
    ).

%---------------------------------------------------------------------------%

:- pred record_recompilation_reason(recompile_reason::in,
    maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_recompilation_reason(Reason, MaybeStoppingReason, !Info) :-
    CollectAllReasons = !.Info ^ rci_collect_all_reasons,
    (
        CollectAllReasons = yes,
        % XXX Note that many places in the code above record a stopping reason
        % *without* either calling this predicate or checking the value of
        % CollectAllReasons themselves, so CollectAllReasons being yes
        % does NOT guarantee that we in fact collect all reasons to recompile.
        MaybeStoppingReason = no,
        !Info ^ rci_recompilation_reasons :=
            [Reason | !.Info ^ rci_recompilation_reasons]
    ;
        CollectAllReasons = no,
        MaybeStoppingReason = yes(Reason)
    ).

%---------------------------------------------------------------------------%

:- pred get_version_numbers_from_parse_tree_some_int(parse_tree_some_int::in,
    version_numbers::out) is semidet.

get_version_numbers_from_parse_tree_some_int(ParseTreeSomeInt,
        VersionNumbers) :-
    (
        ParseTreeSomeInt = parse_tree_some_int0(ParseTreeInt0),
        MaybeVersionNumbers = ParseTreeInt0 ^ pti0_maybe_version_numbers
    ;
        ParseTreeSomeInt = parse_tree_some_int1(ParseTreeInt1),
        MaybeVersionNumbers = ParseTreeInt1 ^ pti1_maybe_version_numbers
    ;
        ParseTreeSomeInt = parse_tree_some_int2(ParseTreeInt2),
        MaybeVersionNumbers = ParseTreeInt2 ^ pti2_maybe_version_numbers
    ;
        ParseTreeSomeInt = parse_tree_some_int3(_ParseTreeInt3),
        % .int3 files never contain version numbers.
        fail
    ),
    MaybeVersionNumbers = version_numbers(VersionNumbers).

%---------------------------------------------------------------------------%

:- type ambiguity_checkables
    --->    ambiguity_checkables(
                % NOTE We should consider making the types of the first
                % three fields type_ctor_defn_map, inst_ctor_defn_map and
                % mode_ctor_defn_map respectively. However, before we do that,
                % we need to decide exactly how we want to handle any entries
                % in the implementation section versions of those maps.
                % I (zs) think it is quite likely that the original code
                % of this module did not consider the treatment of such entries
                % thoroughly enough.
                %
                % Consider that the original motivation to put type
                % definitions into the implementation sections of .int files
                % was to give the compiler the information it needs to decide
                % on the correct representation of values of the type,
                % especially in the context of equivalence types involving
                % floats, which at the time were stored in two words
                % (as 64 bit entities on a 32 bit platform). However,
                % such type definition items specify non-user-visible
                % information, and as such should not be able to affect
                % which type names are ambiguous and which are not.
                % And yet the code of this module has always processed
                % type definition items without regard to which section
                % of an interface file they occurred in. (It is possible
                % that the reason for this is that when this code was first
                % written, interface files did not *have* implementation
                % sections.)
                list(item_type_defn_info),
                list(item_inst_defn_info),
                list(item_mode_defn_info),
                list(item_typeclass_info),
                list(item_pred_decl_info)
            ).

:- pred get_ambiguity_checkables_parse_tree_some_int(parse_tree_some_int::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_parse_tree_some_int(ParseTreeSomeInt, Checkables) :-
    (
        ParseTreeSomeInt = parse_tree_some_int0(ParseTreeInt0),
        get_ambiguity_checkables_parse_tree_int0(ParseTreeInt0, Checkables)
    ;
        ParseTreeSomeInt = parse_tree_some_int1(ParseTreeInt1),
        get_ambiguity_checkables_parse_tree_int1(ParseTreeInt1, Checkables)
    ;
        ParseTreeSomeInt = parse_tree_some_int2(ParseTreeInt2),
        get_ambiguity_checkables_parse_tree_int2(ParseTreeInt2, Checkables)
    ;
        ParseTreeSomeInt = parse_tree_some_int3(ParseTreeInt3),
        get_ambiguity_checkables_parse_tree_int3(ParseTreeInt3, Checkables)
    ).

:- pred get_ambiguity_checkables_parse_tree_int0(parse_tree_int0::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_parse_tree_int0(ParseTreeInt0, Checkables) :-
    ParseTreeInt0 = parse_tree_int0(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntIncls, _ImpIncls, _InclMap,
        _IntImports, _IntUses, _ImpImports, _ImpUses, _ImportUseMap,
        _IntFIMs, _ImpFIMs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, _IntInstances,
        IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, _IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, _ImpInstances,
        ImpPredDecls, _ImpModeDecls, _ImpForeignEnums,
        _ImpDeclPragmas, _ImpPromises),
    ItemTypeDefns =
        type_ctor_defn_map_to_type_defns(IntTypeDefnMap) ++
        type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    ItemInstDefns =
        inst_ctor_defn_map_to_inst_defns(IntInstDefnMap) ++
        inst_ctor_defn_map_to_inst_defns(ImpInstDefnMap),
    ItemModeDefns =
        mode_ctor_defn_map_to_mode_defns(IntModeDefnMap) ++
        mode_ctor_defn_map_to_mode_defns(ImpModeDefnMap),
    ItemTypeClasses = IntTypeClasses ++ ImpTypeClasses,
    ItemPredDecls = IntPredDecls ++ ImpPredDecls,
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

:- pred get_ambiguity_checkables_parse_tree_int1(parse_tree_int1::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_parse_tree_int1(ParseTreeInt1, Checkables) :-
    ParseTreeInt1 = parse_tree_int1(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntIncls, _ImpIncls, _InclMap,
        _IntUses, _ImpUses, _ImportUseMap, _IntFIMs, _ImpFIMs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, _IntItemInstances,
        IntPredDecls, _IntModeDecls, _IntDeclPragmas, _IntPromises,
        _IntTypeRepnMap,
        ImpTypeDefnMap, _ImpFEEs, ImpTypeClasses),
    ItemTypeDefns =
        type_ctor_defn_map_to_type_defns(IntTypeDefnMap) ++
        type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    ItemInstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    ItemModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    ItemTypeClasses = IntTypeClasses ++ ImpTypeClasses,
    ItemPredDecls = IntPredDecls,
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

:- pred get_ambiguity_checkables_parse_tree_int2(parse_tree_int2::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_parse_tree_int2(ParseTreeInt2, Checkables) :-
    ParseTreeInt2 = parse_tree_int2(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntIncls, _InclMap,
        _IntUses, _ImportUseMap, _IntFIMs, _ImpFIMs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntItemTypeClasses, _IntItemInstances, _IntTypeRepnMap,
        ImpTypeDefnMap),
    ItemTypeDefns =
        type_ctor_defn_map_to_type_defns(IntTypeDefnMap) ++
        type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    ItemInstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    ItemModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    ItemTypeClasses = IntItemTypeClasses,
    ItemPredDecls = [],
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

:- pred get_ambiguity_checkables_parse_tree_int3(parse_tree_int3::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_parse_tree_int3(ParseTreeInt3, Checkables) :-
    ParseTreeInt3 = parse_tree_int3(_ModuleName, _ModuleNameContext,
        _Incls, _InclMap, _Avails, _AvailMap,
        TypeDefnMap, InstDefnMap, ModeDefnMap,
        ItemTypeClasses, _ItemInstances, _TypeRepnMap),
    ItemTypeDefns = type_ctor_defn_map_to_type_defns(TypeDefnMap),
    ItemInstDefns = inst_ctor_defn_map_to_inst_defns(InstDefnMap),
    ItemModeDefns = mode_ctor_defn_map_to_mode_defns(ModeDefnMap),
    ItemPredDecls = [],
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

%---------------------------------------------------------------------------%
:- end_module recompilation.check.
%---------------------------------------------------------------------------%
