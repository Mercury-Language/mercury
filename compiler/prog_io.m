%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io.m.
% Main authors: fjh, zs.
%
% This module defines the top level predicates for parsing Mercury programs.
%
% In some ways the representation of programs here is considerably
% more complex than is necessary for the compiler.
% The basic reason for this is that it was designed to preserve
% as much information about the source code as possible, so that
% this representation could also be used for other tools such
% as pretty-printers.
% Currently the only information that is lost is that comments and
% whitespace are stripped, any redundant parenthesization
% are lost, distinctions between different spellings of the same
% operator (eg "\+" vs "not") are lost, and DCG clauses get expanded.
% It would be a good idea to preserve all those too (well, maybe not
% the redundant parentheses), but right now it's not worth the effort.
%
% So that means that this phase of compilation is purely parsing.
% No simplifications are done (other than DCG expansion).
% The results of this phase specify
% basically the same information as is contained in the source code,
% but in a parse tree rather than a flat file.
% Simplifications are done only by make_hlds.m, which transforms
% the parse tree which we built here into the HLDS.
%
% Wishlist:
%
% 1.  implement importing/exporting operators with a particular fixity
%     eg. :- import_op prefix(+). % only prefix +, not infix
%     (not important, but should be there for reasons of symmetry.)
% 2.  improve the handling of type and inst parameters
% 3.  improve the error reporting (most of the semidet preds should
%     be det and should return a meaningful indication of where an
%     error occurred).
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_error.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % actually_read_module(OpenFile, FileName, DefaultModuleName,
    %   ReturnTimestamp, MaybeFileInfo, ActualModuleName, Items,
    %   Specs, Errors, MaybeModuleTimestamp, !IO):
    %
    % Reads and parses the file opened by OpenFile using the default module
    % name DefaultModuleName. If ReturnTimestamp is `yes', attempt to return
    % the modification timestamp in MaybeModuleTimestamp.
    %
    % Errors is the set of errors we have encountered.
    % MaybeFileInfo is the information about the file (usually the file or
    % directory name) returned by OpenFile. ActualModuleName is the module name
    % specified in the `:- module' declaration, if any, or the
    % DefaultModuleName if there is no `:- module' declaration.
    % Specs is a list of warning/error messages. Items is the parse tree.
    %
:- pred actually_read_module(globals::in,
    open_file_pred(FileInfo)::in(open_file_pred), module_name::in,
    maybe_return_timestamp::in, maybe(FileInfo)::out,
    module_name::out, list(item)::out, list(error_spec)::out,
    read_module_errors::out, maybe(io.res(timestamp))::out,
    io::di, io::uo) is det.

:- pred actually_read_module_if_changed(globals::in,
    open_file_pred(FileInfo)::in(open_file_pred),
    module_name::in, timestamp::in, maybe(FileInfo)::out, module_name::out,
    list(item)::out, list(error_spec)::out, read_module_errors::out,
    maybe(io.res(timestamp))::out, io::di, io::uo) is det.

    % Same as actually_read_module, but use intermod_directories instead of
    % search_directories when searching for the file.
    % Also report an error if the actual module name doesn't match
    % the expected module name.
    %
:- pred actually_read_opt_file(globals::in, file_name::in, module_name::in,
    list(item)::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % check_module_has_expected_name(FileName, ExpectedName, ActualName):
    %
    % Check that two module names are equal, and report an error if they
    % aren't.
    %
    % actually_read_opt_file always calls check_module_has_expected_name.
    % actually_read_module and actually_read_module_if_changed do not,
    % but their callers do. However, those callers do not always know
    % WHAT module name they expect until the module has already been read in,
    % so making actually_read_module and actually_read_module_if_changed
    % call check_module_has_expected_name directly is no easy, particularly
    % since the information those callers use in this decision is hidden
    % by the polymorphism provided by the FileInfo type variable.
    %
:- pred check_module_has_expected_name(file_name::in, module_name::in,
    module_name::in, list(error_spec)::out) is det.

    % peek_at_file(DefaultModuleName, SourceFileName, ModuleName, Specs, !IO):
    %
    % When looking for a module, we want to see what the file says
    % about what Mercury module is stored in it.
    %
:- pred peek_at_file(module_name::in, file_name::in, module_name::out,
    list(error_spec)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_item.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_util.

:- import_module bool.
:- import_module counter.
:- import_module parser.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.

%-----------------------------------------------------------------------------%

actually_read_module(Globals, OpenFile, DefaultModuleName, ReturnTimestamp,
        FileData, ModuleName, Items, Specs, Errors, MaybeModuleTimestamp,
        !IO) :-
    do_actually_read_module(Globals, OpenFile, DefaultModuleName,
        no, ReturnTimestamp, FileData, ModuleName,
        Items, Specs, Errors, MaybeModuleTimestamp, !IO).

actually_read_module_if_changed(Globals, OpenFile, DefaultModuleName,
        OldTimestamp, FileData, ModuleName, Items, Specs, Errors,
        MaybeModuleTimestamp, !IO) :-
    do_actually_read_module(Globals, OpenFile, DefaultModuleName,
        yes(OldTimestamp), do_return_timestamp, FileData, ModuleName,
        Items, Specs, Errors, MaybeModuleTimestamp, !IO).

actually_read_opt_file(Globals, FileName, DefaultModuleName,
        Items, Specs, Errors, !IO) :-
    globals.lookup_accumulating_option(Globals, intermod_directories, Dirs),
    do_actually_read_module(Globals,
        search_for_file(open_file, Dirs, FileName),
        DefaultModuleName, no, do_not_return_timestamp, _, ModuleName, Items,
        ItemSpecs, Errors, _, !IO),
    check_module_has_expected_name(FileName, DefaultModuleName, ModuleName,
        NameSpecs),
    Specs = ItemSpecs ++ NameSpecs.

check_module_has_expected_name(FileName, ExpectedName, ActualName, Specs) :-
    ( ActualName \= ExpectedName ->
        Pieces = [words("Error: file"), quote(FileName),
            words("contains the wrong module."), nl,
            words("Expected module"), sym_name(ExpectedName), suffix(","),
            words("found module"), sym_name(ActualName), suffix("."), nl],
        Msg = error_msg(no, treat_as_first, 0, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_read_files, [Msg]),
        Specs = [Spec]
    ;
        Specs = []
    ).

%-----------------------------------------------------------------------------%

    % This implementation uses io.read_term to read in the program one term
    % at a time, and then converts those terms into clauses and declarations,
    % checking for errors as it goes. Note that rather than using difference
    % lists, we just build up the lists of items and messages in reverse order
    % and then reverse them afterwards. (Using difference lists would require
    % late-input modes.)
    %
:- pred do_actually_read_module(globals::in,
    open_file_pred(T)::in(open_file_pred), module_name::in,
    maybe(timestamp)::in, maybe_return_timestamp::in,
    maybe(T)::out, module_name::out, list(item)::out, list(error_spec)::out,
    read_module_errors::out, maybe(io.res(timestamp))::out,
    io::di, io::uo) is det.

do_actually_read_module(Globals, OpenFile, DefaultModuleName,
        MaybeOldTimestamp, ReturnTimestamp, MaybeFileData, ModuleName,
        Items, Specs, Errors, MaybeModuleTimestamp, !IO) :-
    io.input_stream(OldInputStream, !IO),
    OpenFile(OpenResult, !IO),
    (
        OpenResult = ok(FileData),
        MaybeFileData = yes(FileData),
        (
            ReturnTimestamp = do_return_timestamp,
            io.input_stream_name(InputStreamName, !IO),
            io.file_modification_time(InputStreamName, TimestampResult, !IO),
            (
                TimestampResult = ok(Timestamp),
                MaybeModuleTimestamp = yes(ok(time_t_to_timestamp(Timestamp)))
            ;
                TimestampResult = error(IOError),
                MaybeModuleTimestamp = yes(error(IOError))
            )
        ;
            ReturnTimestamp = do_not_return_timestamp,
            MaybeModuleTimestamp = no
        ),
        (
            MaybeOldTimestamp = yes(OldTimestamp),
            MaybeModuleTimestamp = yes(ok(OldTimestamp))
        ->
            % XXX Currently smart recompilation won't work
            % if ModuleName \= DefaultModuleName.
            % In that case, smart recompilation will be disabled
            % and actually_read_module should never be passed an old timestamp.

            ModuleName = DefaultModuleName,
            Items = [],
            Specs = [],
            set.init(Errors)
        ;
            read_all_items(Globals, DefaultModuleName, ModuleName, Items,
                Specs, Errors, !IO)
        ),
        io.set_input_stream(OldInputStream, ModuleInputStream, !IO),
        io.close_input(ModuleInputStream, !IO)
    ;
        OpenResult = error(ErrorMsg),
        MaybeFileData = no,
        ModuleName = DefaultModuleName,
        Items = [],
        MaybeModuleTimestamp = no,

        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words(ErrorMsg), nl],
        Spec = error_spec(severity_error, phase_read_files,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        Specs = [Spec],
        Errors = set.make_singleton_set(rme_could_not_open_file)
    ).

%-----------------------------------------------------------------------------%

    % Read a source file from standard in, first reading in
    % the input term by term and then parsing those terms and producing
    % a high-level representation.
    % Parsing is actually a 3-stage process instead of the
    % normal two-stage process:
    %   lexical analysis (chars -> tokens),
    %   parsing stage 1 (tokens -> terms),
    %   parsing stage 2 (terms -> items).
    % The final stage produces a list of program items, each of which
    % may be a declaration or a clause.
    %
    % We use a continuation-passing style here.
    %
:- pred read_all_items(globals::in, module_name::in, module_name::out,
    list(item)::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

read_all_items(Globals, DefaultModuleName, ModuleName, Items,
        !:Specs, !:Errors, !IO) :-
    some [!SeqNumCounter, !RevItems] (
        counter.init(1, !:SeqNumCounter),

        io.input_stream(Stream, !IO),
        io.input_stream_name(Stream, SourceFileName0, !IO),
        % We handle the first item specially. Read the documentation on
        % read_first_item for the reason.
        read_first_item(DefaultModuleName, SourceFileName0, SourceFileName,
            ModuleName, ModuleDeclItem, MaybeSecondTermResult,
            !:Specs, !:Errors, !SeqNumCounter, !IO),
        !:RevItems = [ModuleDeclItem],
        read_items_loop(Globals, ModuleName, SourceFileName,
            MaybeSecondTermResult, !RevItems, !Specs, !Errors,
            !.SeqNumCounter, _, !IO),
        remove_main_module_start_end_wrappers(ModuleName, !.RevItems, Items)
    ).

    % Unreverse the item list while removing from it both the initial
    % ":- module" declaration and its matching ":- end_module" declaration,
    % if there is one.
    %
:- pred remove_main_module_start_end_wrappers(module_name::in,
    list(item)::in, list(item)::out) is det.

remove_main_module_start_end_wrappers(MainModuleName, RevItems0, !:Items) :-
    (
        % Note: if the module name in the end_module declaration
        % does not match name of the top level module stored in this
        % source file, then that end_module declaration must be
        % for a nested module, and we therefore leave it alone.
        % If it is not for a nested module, the error will have been
        % caught and reported by process_one_item_in_loop.

        RevItems0 = [LastItem | RevItems1],
        LastItem = item_module_end(ItemModuleEnd),
        ItemModuleEnd = item_module_end_info(MainModuleName, _Context, _SeqNum)
    ->
        RevItems = RevItems1
    ;
        RevItems = RevItems0
    ),
    list.reverse(RevItems, !:Items),

    % Double-check that the first item is a `:- module MainModuleName'
    % declaration, and if it is, remove it from the front of the item list.
    (
        !.Items = [Item | !:Items],
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart = item_module_start_info(MainModuleName, _, _)
    ->
        true
    ;
        % If there is no `:- module' declaration at this point, it is
        % an internal error, since read_first_item should have inserted one.
        unexpected($module, $pred,
            "module does not start with `:- module' declaration")
    ).

%-----------------------------------------------------------------------------%

peek_at_file(DefaultModuleName, SourceFileName0, ModuleName, Specs, !IO) :-
    counter.init(1, SeqNumCounter0),
    read_first_item(DefaultModuleName, SourceFileName0, _SourceFileName,
        ModuleName, _ModuleDeclItem, _MaybeSecondTerm, Specs, _Errors,
        SeqNumCounter0, _, !IO).

    % We need to jump through a few hoops when reading the first item,
    % to allow us to recover from a missing initial `:- module' declaration.
    % The reason is that in order to parse an item, we need to know
    % which module it is defined in (because we do some module
    % qualification and checking of module qualifiers at parse time),
    % but the initial `:- module' declaration and the declaration
    % that follows it occur in different scopes, so we need to know
    % what it is that we are parsing before we can parse it!
    % We solve this dilemma by first parsing it in the root scope,
    % and then if it turns out to not be a `:- module' declaration
    % we reparse it in the default module scope. Blecchh.
    %
:- pred read_first_item(module_name::in, file_name::in, file_name::out,
    module_name::out, item::out, maybe(read_term)::out,
    list(error_spec)::out, read_module_errors::out, counter::in, counter::out,
    io::di, io::uo) is det.

read_first_item(DefaultModuleName, !SourceFileName, ModuleName,
        ModuleDeclItem, MaybeSecondTerm, Specs, Errors, !SeqNumCounter, !IO) :-
    % Parse the first term, treating it as occurring within the scope
    % of the special "root" module (so that any `:- module' declaration
    % is taken to be a non-nested module unless explicitly qualified).
    parser.read_term_filename(!.SourceFileName, MaybeFirstTerm, !IO),
    read_term_to_item_result(root_module_name, !.SourceFileName,
        MaybeFirstTerm, !SeqNumCounter, MaybeFirstItem),
    (
        % Apply and then skip `pragma source_file' decls, by calling ourselves
        % recursively with the new source file name.
        MaybeFirstItem = read_item_ok(FirstItem),
        FirstItem = item_pragma(FirstItemPragma),
        FirstItemPragma = item_pragma_info(_, Pragma, _, _),
        Pragma = pragma_source_file(SFNInfo)
    ->
        SFNInfo = pragma_info_source_file(!:SourceFileName),
        read_first_item(DefaultModuleName, !SourceFileName, ModuleName,
            ModuleDeclItem, MaybeSecondTerm, Specs, Errors, !SeqNumCounter,
            !IO)
    ;
        % Check if the first term is a `:- module' decl.
        MaybeFirstItem = read_item_ok(FirstItem),
        FirstItem = item_module_start(FirstItemModuleStart),
        FirstItemModuleStart = item_module_start_info(StartModuleName,
            FirstContext, _FirstItemSeqNum)
    ->
        % If it is, then check that it matches the expected module name.
        % If it does not match, report a warning.
        ( match_sym_name(StartModuleName, DefaultModuleName) ->
            ModuleName = DefaultModuleName,
            Specs = [],
            set.init(Errors)
        ; match_sym_name(DefaultModuleName, StartModuleName) ->
            ModuleName = StartModuleName,
            Specs = [],
            set.init(Errors)
        ;
            % XXX I think this should be an error, not a warning. -zs
            Pieces = [words("Error: source file"), quote(!.SourceFileName),
                words("contains module named"), sym_name(StartModuleName),
                suffix("."), nl],
            Severity = severity_conditional(warn_wrong_module_name, yes,
                severity_error, no),
            Msgs = [option_is_set(warn_wrong_module_name, yes,
                [always(Pieces)])],
            Spec = error_spec(Severity, phase_term_to_parse_tree,
                [simple_msg(FirstContext, Msgs)]),
            Specs = [Spec],

            % Which one should we use here? We used to use the default module
            % name (computed from the filename) but now we use the declared
            % one.
            ModuleName = StartModuleName,
            Errors = set.make_singleton_set(rme_unexpected_module_name)
        ),
        make_module_decl(ModuleName, FirstContext, ModuleDeclItem),
        MaybeSecondTerm = no
    ;
        % If the first term was not a `:- module' decl, then generate an
        % error message, and insert an implicit `:- module ModuleName' decl.
        ( MaybeFirstItem = read_item_ok(FirstItem) ->
            FirstContext = get_item_context(FirstItem)
        ;
            term.context_init(!.SourceFileName, 1, FirstContext)
        ),
        Pieces = [words("Error: module must start with a"),
            decl("module"), words("declaration."), nl],
        Severity = severity_error,
        Msgs = [always(Pieces)],
        Spec = error_spec(Severity, phase_term_to_parse_tree,
            [simple_msg(FirstContext, Msgs)]),
        Specs = [Spec],
        Errors = set.make_singleton_set(rme_no_module_decl_at_start),

        ModuleName = DefaultModuleName,
        make_module_decl(ModuleName, FirstContext, ModuleDeclItem),

        % Reparse the first term, this time treating it as occuring within
        % the scope of the implicit `:- module' decl rather than in the
        % root module.
        MaybeSecondTerm = yes(MaybeFirstTerm)
    ).

:- pred make_module_decl(module_name::in, term.context::in, item::out) is det.

make_module_decl(ModuleName, Context, Item) :-
    ItemInfo = item_module_start_info(ModuleName, Context, -1),
    Item = item_module_start(ItemInfo).

%-----------------------------------------------------------------------------%

    % This loop reads in all the items in a file.
    %
    % Our top-level caller may or may not specify a term that it has already
    % read in. (This is a side effect of the special handling of the first item
    % by read_first_item.) The recursive calls never specify an already-read-in
    % term. To avoid having to check for an already-read-in term on every
    % iteration, we special case the mode where there isn't one.
    %
:- pred read_items_loop(globals, module_name, file_name, maybe(read_term),
    list(item), list(item), list(error_spec), list(error_spec),
    read_module_errors, read_module_errors, counter, counter, io, io).
:- mode read_items_loop(in, in, in, in(bound(no)),
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode read_items_loop(in, in, in, in,
    in, out, in, out, in, out, in, out, di, uo) is det.

read_items_loop(Globals, !.ModuleName, !.SourceFileName, MaybeReadTermResult,
        !RevItems, !Specs, !Errors, !SeqNumCounter, !IO) :-
    (
        MaybeReadTermResult = no,
        parser.read_term_filename(!.SourceFileName, ReadTermResult, !IO)
    ;
        MaybeReadTermResult = yes(ReadTermResult)
    ),
    read_term_to_item_result(!.ModuleName, !.SourceFileName, ReadTermResult,
        !SeqNumCounter, ReadItemResult),
    (
        ReadItemResult = read_item_eof
        % If the next item was end-of-file, then we are done.
    ;
        ReadItemResult = read_item_errors(ItemSpecs, ItemErrors),
        % If the next item had some errors, then insert them
        % into the list of errors and continue looping.

        !:Specs = ItemSpecs ++ !.Specs,
        !:Errors = set.union(!.Errors, ItemErrors),
        read_items_loop(Globals, !.ModuleName, !.SourceFileName, no,
            !RevItems, !Specs, !Errors, !SeqNumCounter, !IO)
    ;
        ReadItemResult = read_item_ok(Item),
        process_one_item_in_loop(Globals, Item, !ModuleName, !SourceFileName,
            !RevItems, !Specs, !Errors, !IO),
        read_items_loop(Globals, !.ModuleName, !.SourceFileName, no,
            !RevItems, !Specs, !Errors, !SeqNumCounter, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type read_item_result
    --->    read_item_eof
    ;       read_item_errors(list(error_spec), set(read_module_error))
    ;       read_item_ok(item).

:- pred read_term_to_item_result(module_name::in, string::in, read_term::in,
    counter::in, counter::out, read_item_result::out) is det.

read_term_to_item_result(ModuleName, FileName, ReadTermResult,
        !SeqNumCounter, ReadItemResult) :-
    (
        ReadTermResult = eof,
        ReadItemResult = read_item_eof
    ;
        ReadTermResult = error(ErrorMsg, LineNumber),
        % XXX Do we need to add an "Error:" prefix?
        Pieces = [words(ErrorMsg), suffix("."), nl],
        Context = term.context_init(FileName, LineNumber),
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        ReadItemResult = read_item_errors([Spec],
            set.make_singleton_set(rme_could_not_read_term))
    ;
        ReadTermResult = term(VarSet, Term),
        counter.allocate(SeqNum, !SeqNumCounter),
        parse_item(ModuleName, VarSet, Term, SeqNum, MaybeItem),
        (
            MaybeItem = ok1(Item),
            ReadItemResult = read_item_ok(Item)
        ;
            MaybeItem = error1(Specs),
            ReadItemResult = read_item_errors(Specs,
                set.make_singleton_set(rme_could_not_parse_item))
        )
    ).

%-----------------------------------------------------------------------------%

:- pred process_one_item_in_loop(globals::in, item::in,
    module_name::in, module_name::out, file_name::in, file_name::out,
    list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

process_one_item_in_loop(Globals, Item, !ModuleName, !SourceFileName,
        !RevItems, !Specs, !Errors, !IO) :-
    % If the next item was a valid item, check whether it was a declaration
    % that affects the current parsing context -- i.e. either a `module' or
    % `end_module' declaration, or a `pragma source_file' declaration.
    % If so, update the new parsing context. Next, unless the item
    % is a `pragma source_file' declaration, insert it into the item list.
    % Then return to our caller to continue looping.
    (
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart = item_module_start_info(NewModuleSymName,
            ItemContext, _),
        (
            NewModuleSymName = unqualified(NewModuleName),
            !:ModuleName = qualified(!.ModuleName, NewModuleName)
        ;
            NewModuleSymName = qualified(NewQualifier, NewModuleName),
            OldQualifiersList = sym_name_to_list(!.ModuleName),
            NewQualifiersList = sym_name_to_list(NewQualifier),
            ( list.sublist(NewQualifiersList, OldQualifiersList) ->
                !:ModuleName = qualified(!.ModuleName, NewModuleName)
            ;
                Pieces = [words("Error:"),
                    words("the module"), sym_name(NewModuleSymName),
                    words("in this"), decl("module"), words("declaration"),
                    words("cannot be a submodule of the current module"),
                    sym_name(!.ModuleName), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(ItemContext, [always(Pieces)])]),
                !:Specs = [Spec | !.Specs],
                set.insert(rme_bad_submodule_start, !Errors),

                % XXX Believing the incorrect submodule declaration follows
                % the algorithm that this code used to use, but I (zs)
                % am far from sure that this the least harmful thing we can
                % do here, since it will surely lead to errors when the
                % corresponding ":- end_module" declaration is reached.
                !:ModuleName = NewModuleSymName
            )
        ),
        !:RevItems = [Item | !.RevItems]
    ;
        Item = item_module_end(ItemModuleEnd),
        ItemModuleEnd = item_module_end_info(EndModuleSymName, ItemContext, _),
        sym_name_to_qualifier_list_and_name(EndModuleSymName,
            EndQualifierList, EndModuleName),
        sym_name_to_qualifier_list_and_name(!.ModuleName,
            CurQualifierList, CurModuleName),
        % The language spec says that a single ":- end_module" declaration
        % can end only one nested module.
        (
            EndModuleName = CurModuleName,
            list.sublist(EndQualifierList, CurQualifierList)
        ->
            sym_name_get_module_name_default(EndModuleSymName,
                root_module_name, ParentModuleSymName),
            !:ModuleName = ParentModuleSymName
        ;
            PrefixPieces = [words("Error:"),
                words("this"), decl("end_module"), words("declaration"),
                words("is not for the until-then-current module.")],
            ( EndModuleName = CurModuleName ->
                NamePieces = []
            ;
                NamePieces = [words("The module names do not match:"),
                    words("actual"), quote(EndModuleName), words("vs"),
                    words("expected"), quote(CurModuleName)]
            ),
            ( list.sublist(EndQualifierList, CurQualifierList) ->
                QualifierPieces = []
            ;
                (
                    EndModuleSymName = qualified(EndQualifier, _)
                ;
                    EndModuleSymName = unqualified(_),
                    % An empty EndQualifierList should match EVERY
                    % CurQualifierList, so the call to list.sublist above
                    % should have succeeded.
                    unexpected($module, $pred,
                        "unqualified symname does not pass sublist test")
                ),
                (
                    !.ModuleName = qualified(CurQualifier, _),
                    CurQualifierPiece = sym_name(CurQualifier)
                ;
                    !.ModuleName = unqualified(_),
                    CurQualifierPiece = words("no qualification")
                ),
                (
                    NamePieces = [],
                    LinkPieces = [words("The module qualifiers")]
                ;
                    NamePieces = [_ | _],
                    LinkPieces = [suffix(", "),
                        words("and the module qualifiers")]
                ),
                QualifierPieces = LinkPieces ++ [words("do not match:"),
                    words("actual"), sym_name(EndQualifier), words("vs"),
                    words("expected"), CurQualifierPiece]
            ),

            SuffixPieces = [suffix("."), nl],
            Pieces = PrefixPieces ++ NamePieces ++ QualifierPieces
                ++ SuffixPieces,
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(ItemContext, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs],
            set.insert(rme_bad_module_end, !Errors),

            % This setting of !:ModuleName effectively replaces
            % the incorrect end_module declaration with the end_module
            % declaration we expected here. This is the right thing
            % to do e.g.if the end_module declaration is simply mistyped,
            % but if the user actually intended to end several nested
            % submodules with a single end_module declaration, then
            % it will put the following items in a context that
            % the programmer did not intend.
            sym_name_get_module_name_default(EndModuleSymName,
                root_module_name, ParentModuleSymName),
            !:ModuleName = ParentModuleSymName
        ),
        !:RevItems = [Item | !.RevItems]
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
        ( ModuleDefn = md_import(Modules) ->
            list.map(make_pseudo_import_module_decl(Context, SeqNum),
                Modules, ImportItems),
            !:RevItems = ImportItems ++ !.RevItems
        ; ModuleDefn = md_use(Modules) ->
            list.map(make_pseudo_use_module_decl(Context, SeqNum),
                Modules, UseItems),
            !:RevItems = UseItems ++ !.RevItems
        ; ModuleDefn = md_include_module(Modules) ->
            list.map(make_pseudo_include_module_decl(Context, SeqNum),
                Modules, IncludeItems),
            !:RevItems = IncludeItems ++ !.RevItems
        ;
            !:RevItems = [Item | !.RevItems]
        )
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, Pragma, _, _),
        ( Pragma = pragma_source_file(SFNInfo) ->
            SFNInfo = pragma_info_source_file(!:SourceFileName)
        ;
            !:RevItems = [Item | !.RevItems]
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        !:RevItems = [Item | !.RevItems]
    ;
        Item = item_nothing(ItemNothing),
        ItemNothing = item_nothing_info(MaybeWarning, Context, NothingSeqNum),
        (
            MaybeWarning = no,
            !:RevItems = [Item | !.RevItems]
        ;
            MaybeWarning = yes(Warning),
            Warning = item_warning(MaybeOption, Msg, Term),
            (
                MaybeOption = yes(Option),
                globals.lookup_bool_option(Globals, Option, Warn)
            ;
                MaybeOption = no,
                Warn = yes
            ),
            (
                Warn = yes,
                Pieces = [words("Warning: "), words(Msg), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                !:Specs = [Spec | !.Specs],

                globals.lookup_bool_option(Globals, halt_at_warn, Halt),
                (
                    Halt = yes,
                    set.insert(rme_warn_item_nothing, !Errors)
                ;
                    Halt = no
                )
            ;
                Warn = no
            ),
            NoWarnItemNothing = item_nothing_info(no, Context, NothingSeqNum),
            NoWarnItem = item_nothing(NoWarnItemNothing),
            !:RevItems = [NoWarnItem | !.RevItems]
        )
    ).

:- pred make_pseudo_import_module_decl(prog_context::in, int::in,
    module_specifier::in, item::out) is det.

make_pseudo_import_module_decl(Context, SeqNum, ModuleSpecifier, Item) :-
    ModuleDefn = md_import([ModuleSpecifier]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
    Item = item_module_defn(ItemModuleDefn).

:- pred make_pseudo_use_module_decl(prog_context::in, int::in,
    module_specifier::in, item::out) is det.

make_pseudo_use_module_decl(Context, SeqNum, ModuleSpecifier, Item) :-
    ModuleDefn = md_use([ModuleSpecifier]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
    Item = item_module_defn(ItemModuleDefn).

:- pred make_pseudo_include_module_decl(prog_context::in, int::in,
    module_name::in, item::out) is det.

make_pseudo_include_module_decl(Context, SeqNum, ModuleSpecifier, Item) :-
    ModuleDefn = md_include_module([ModuleSpecifier]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
    Item = item_module_defn(ItemModuleDefn).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io.
%-----------------------------------------------------------------------------%
