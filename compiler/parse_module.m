%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_module.m.
% Main author of original version: fjh.
% Main author of current version: zs.
%
% This module defines the top level predicates for parsing Mercury programs.
%
% The main predicates of this module, actually_read_module_{src,int,opt},
% read in and parse the contents of source files, interface files and
% optimization files respectively.
%
% The original design of this module represented all three kinds of files
% as simple lists of items. The design of the items themselves was made
% considerably more complex than is necessary for the compiler, because
% one objective was to preserve as much information about the source code
% as possible, to allow tools such as prettyprinters to reconstruct the
% source code as closely as possible. (For example, this is why each item
% has a sequence number.) The only information that was designed to be lost
% was the stripping of comments, whitespace and redundant parenthesization,
% the standardization of the spellings of the names of operators (such as
% rewriting "\+" to "not"), and the expansion of DCG clauses.
%
% We have not found a need for such prettyprinters in more than twenty years,
% so the original concern for minimizing the differences between the source
% code and the AST, which was Fergus's argument for keeping the AST a simple
% item list, has been shown to be irrelevant.
%
% We therefore now use a representation for the parse tree that is more suited
% to the needs of the compiler itself, even though it is more complex
% that a simple list of items. Part of this complexity is that source files,
% interface files and optimization files have different parse trees,
% and we accordingly have different predicates for creating those parse trees.
% The main predicates for reading these three kinds of files are
%
% - read_parse_tree_src,
% - read_parse_tree_int, and
% - read_parse_tree_opt.
%
% Each of these kinds of files has its grammar describing the structure
% its items should follow. The predicates parsing parts of those structures
% each have a comment before them specifying what part of the relevant file
% kind they are designed to parse. The grammar is simplest for opt files, and
% next simplest for int files, so the order of those predicates in this module
% is read_parse_tree_opt, read_parse_tree_int, and then read_parse_tree_src.
%
% Our parsing process has four stages instead of the usual two.
% Our stages are:
%
%   lexical analysis: chars -> tokens
%   parsing stage 1:  tokens -> terms
%   parsing stage 2:  terms -> items and markers
%   parsing stage 3:  items and markers -> structured parse tree
%
% An item represents a clause or a declaration. A marker represents
% a declaration that affects the structure of the parse tree, usually by
% controlling what section or what (sub)module the following items belong to.
%
% We never materialize the intermediate representations (a token list,
% a term list or an item/marker list) all at once. Instead, we read in the
% contents of the relevant file one term at a time, and convert that
% to an item or marker before reading in the next term.
%
% Some predicates, when they parse a term, discover that it represents an
% item or marker that it is not their job to handle. In such cases, they
% return the term that the item or marker came from, together with its varset,
% to their caller. This term is lookahead; the next term in the input, which
% the caller can get directly from the lookahead without reading in its term.
% If there is no term in the lookahead, the next term has to be read in.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_module.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

    % peek_at_file(DefaultModuleName, DefaultExpectationContexts,
    %   SourceFileName, ModuleName, Specs, !IO):
    %
    % When looking for a module, we sometimes want to see what the file says
    % about what Mercury module is stored in it. So we read the first thing
    % in it, and see whether it is a module declaration. If it is, return
    % the name of the module as ModuleName. If it isn't, return
    % DefaultModuleName as ModuleName.
    % XXX ITEM_LIST In the latter case, we should return an error indication.
    %
:- pred peek_at_file(io.text_input_stream::in,
    module_name::in, list(prog_context)::in, file_name::in,
    module_name::out, list(error_spec)::out, io::di, io::uo) is det.

    % actually_read_module_src(Globals,
    %   DefaultModuleName, DefaultExpectationContexts,
    %   MaybeFileNameAndStream, ReadModuleAndTimestamps,
    %   MaybeModuleTimestampRes, ParseTree, Specs, Errors, !IO):
    %
    % Read a Mercury source program from FileNameAndStream, if it exists.
    % Close the stream when the reading is done. Return the parse tree
    % of that module in ParseTree (which may be a dummy if the file
    % couldn't be opened), and an indication of the errors found
    % in Specs and Errors.
    %
    % For the meaning of ReadModuleAndTimestamps and MaybeModuleTimestampRes,
    % read the comments on read_module_src in read_modules.m.
    % XXX ITEM_LIST Move actually_read_module_{src,int,opt} to read_modules.m.
    %
:- pred actually_read_module_src(globals::in,
    module_name::in, list(prog_context)::in,
    maybe_error(path_name_and_stream)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    parse_tree_src::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % actually_read_module_int(IntFileKind, Globals,
    %   DefaultModuleName, DefaultExpectationContexts,
    %   MaybeFileNameAndStream, ReadModuleAndTimestamps,
    %   MaybeModuleTimestampRes, ParseTree, Specs, Errors, !IO):
    %
    % Analogous to actually_read_module_src, but opens the IntFileKind
    % interface file for DefaultModuleName.
    %
:- pred actually_read_module_int(int_file_kind::in, globals::in,
    module_name::in, list(prog_context)::in,
    maybe_error(path_name_and_stream)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    parse_tree_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % actually_read_module_opt(OptFileKind, Globals, FileName,
    %   DefaultModuleName, DefaultExpectationContexts, ParseTree,
    %   Specs, Errors, !IO):
    %
    % Analogous to actually_read_module_src, but opens the OptFileKind
    % optimization file for DefaultModuleName. It differs in being
    % given the FileName, and using intermod_directories instead of
    % search_directories when searching for that file. Also reports an error
    % if the actual module name doesn't match the expected module name.
    %
:- pred actually_read_module_opt(opt_file_kind::in, globals::in,
    file_name::in, module_name::in, list(prog_context)::in,
    parse_tree_opt::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

:- type maybe_require_module_decl
    --->    dont_require_module_decl
    ;       require_module_decl.

    % check_module_has_expected_name(FileName,
    %   ExpectedModuleName, ExpectationContexts,
    %   ActualModuleName, MaybeActualModuleNameContext, Specs):
    %
    % Check that ActualModuleName is equal to ExpectedModuleName. If it isn't,
    % generate an error message about FileName containing the wrong module.
    %
    % Note that while actually_read_opt_file always calls
    % check_module_has_expected_name, actually_read_module_src and
    % actually_read_module_int do not, though their callers may.
    % However, those callers do not always know WHAT module name they expect
    % until the module has already been read in,
    % so making actually_read_module_src and actually_read_module_int
    % call check_module_has_expected_name directly would not be easy,
    % particularly since the information those callers use in this decision
    % is hidden by the polymorphism provided by the FileInfo type variable.
    %
:- pred check_module_has_expected_name(file_name::in,
    module_name::in, list(prog_context)::in,
    module_name::in, maybe(term.context)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_item.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_types.
:- import_module recompilation.

:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module pair.
:- import_module parser.
:- import_module require.
:- import_module set.
:- import_module term_io.
:- import_module varset.

:- type missing_section_start_warning
    --->    have_not_given_missing_section_start_warning
    ;       have_given_missing_section_start_warning.

%---------------------------------------------------------------------------%

peek_at_file(Stream, DefaultModuleName, DefaultExpectationContexts,
        SourceFileName0, ModuleName, Specs, !IO) :-
    counter.init(1, SeqNumCounter0),
    read_first_module_decl(Stream, dont_require_module_decl,
        DefaultModuleName, DefaultExpectationContexts,
        ModuleDeclPresent, SourceFileName0, _SourceFileName,
        SeqNumCounter0, _SeqNumCounter, [], Specs, set.init, _Errors, !IO),
    (
        ModuleDeclPresent = no_module_decl_present(_),
        ModuleName = DefaultModuleName
    ;
        ModuleDeclPresent =
            wrong_module_decl_present(ModuleName, _ModuleNameContext)
    ;
        ModuleDeclPresent =
            right_module_decl_present(ModuleName, _ModuleNameContext)
    ).

%---------------------------------------------------------------------------%

actually_read_module_src(Globals,
        DefaultModuleName, DefaultExpectationContexts,
        MaybeFileNameAndStream, ReadModuleAndTimestamps,
        MaybeModuleTimestampRes, ParseTree, Specs, Errors, !IO) :-
    do_actually_read_module(Globals,
        DefaultModuleName, DefaultExpectationContexts, MaybeFileNameAndStream,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        make_dummy_parse_tree_src, read_parse_tree_src,
        ParseTree, Specs, Errors, !IO).

%---------------------%

actually_read_module_int(IntFileKind, Globals,
        DefaultModuleName, DefaultExpectationContexts,
        MaybeFileNameAndStream, ReadModuleAndTimestamps,
        MaybeModuleTimestampRes, ParseTree, Specs, Errors, !IO) :-
    do_actually_read_module(Globals,
        DefaultModuleName, DefaultExpectationContexts, MaybeFileNameAndStream,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        make_dummy_parse_tree_int(IntFileKind),
        read_parse_tree_int(IntFileKind),
        ParseTree, Specs, Errors, !IO).

%---------------------%

actually_read_module_opt(OptFileKind, Globals, FileName,
        DefaultModuleName, DefaultExpectationContexts,
        ParseTreeOpt, Specs, Errors, !IO) :-
    globals.lookup_accumulating_option(Globals, intermod_directories, Dirs),
    search_for_file_and_stream(Dirs, FileName, MaybeFileNameAndStream, !IO),
    do_actually_read_module(Globals,
        DefaultModuleName, DefaultExpectationContexts, MaybeFileNameAndStream,
        always_read_module(dont_return_timestamp), _,
        make_dummy_parse_tree_opt(OptFileKind),
        read_parse_tree_opt(OptFileKind),
        ParseTreeOpt, ItemSpecs, Errors, !IO),
    ModuleName = ParseTreeOpt ^ pto_module_name,
    check_module_has_expected_name(FileName, DefaultModuleName,
        DefaultExpectationContexts,ModuleName, no, NameSpecs),
    Specs = ItemSpecs ++ NameSpecs.

%---------------------%

check_module_has_expected_name(FileName, ExpectedName, ExpectationContexts,
        ActualName, MaybeActualContext, Specs) :-
    ( if ActualName = ExpectedName then
        Specs = []
    else
        ( if
            MaybeActualContext = yes(ActualContext),
            ActualContext \= term.context_init
        then
            MaybeContext = MaybeActualContext
        else
            MaybeContext = no
        ),
        MainPieces = [words("Error: file"), quote(FileName),
            words("contains the wrong module."),
            words("Expected module"), qual_sym_name(ExpectedName), suffix(","),
            words("found module"), qual_sym_name(ActualName), suffix("."), nl],
        % We make the warning conditional on the warn_wrong_module_name option.
        % This option is turned on by default, and it is turned off
        % automatically by the compiler only in situations where it
        % clearly makes sense to do so (in handle_options.m and options.m).
        % If it is turned off manually by the user, he/she presumably
        % has a good reason.
        %
        % Despite the option name having the "warn_" prefix,
        % the severity is an error. The severity is deliberate.
        % XXX The option should be renamed, but there is no obvious
        % non-misleading name.
        Severity = severity_conditional(warn_wrong_module_name,
            yes, severity_error, no),
        MainComponent = option_is_set(warn_wrong_module_name, yes,
            [always(MainPieces)]),
        MainMsg = error_msg(MaybeContext, treat_as_first, 0,
            [MainComponent]),
        list.sort_and_remove_dups(ExpectationContexts,
            SortedExpectationContexts),
        list.map(expectation_context_to_msg, SortedExpectationContexts,
            SubMsgs),
        Spec = error_spec(Severity, phase_module_name, [MainMsg | SubMsgs]),
        Specs = [Spec]
    ).

:- pred expectation_context_to_msg(prog_context::in, error_msg::out) is det.

expectation_context_to_msg(Context, SubMsg) :-
    SubPieces = [words("This module specifies the expected name."), nl],
    SubComponent = option_is_set(warn_wrong_module_name, yes,
        [always(SubPieces)]),
    SubMsg = simple_msg(Context, [SubComponent]).

%---------------------%

:- type read_parse_tree(PT) ==
    pred(io.text_input_stream, string, globals, module_name,
        list(prog_context), PT, list(error_spec), read_module_errors, io, io).
:- inst read_parse_tree ==
    (pred(in, in,in, in, in, out, out, out, di, uo) is det).

:- type make_dummy_parse_tree(PT) == pred(module_name, PT).
:- inst make_dummy_parse_tree == (pred(in, out) is det).

%---------------------%

:- pred make_dummy_parse_tree_src(module_name::in, parse_tree_src::out) is det.

make_dummy_parse_tree_src(ModuleName, ParseTree) :-
    ParseTree = parse_tree_src(ModuleName, term.context_init, cord.init).

:- pred make_dummy_parse_tree_int(int_file_kind::in, module_name::in,
    parse_tree_int::out) is det.

make_dummy_parse_tree_int(IntFileKind, ModuleName, ParseTree) :-
    ParseTree = parse_tree_int(ModuleName, IntFileKind, term.context_init,
        no, [], [], [], [], [], []).

:- pred make_dummy_parse_tree_opt(opt_file_kind::in, module_name::in,
    parse_tree_opt::out) is det.

make_dummy_parse_tree_opt(OptFileKind, ModuleName, ParseTree) :-
    ParseTree = parse_tree_opt(ModuleName, OptFileKind, term.context_init,
        [], []).

%---------------------------------------------------------------------------%

    % This predicate implements all three of actually_read_module_{src,int,opt}
    % through the polymorphism provided by the ReadParseTree (sometimes the
    % MakeDummyParseTree) higher order variables. All the actual parsing
    % takes place inside ReadParseTree, which will be one of
    % read_parse_tree_src, read_parse_tree_int and read_parse_tree_src.
    %
:- pred do_actually_read_module(globals::in,
    module_name::in, list(prog_context)::in,
    maybe_error(path_name_and_stream)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    make_dummy_parse_tree(PT)::in(make_dummy_parse_tree),
    read_parse_tree(PT)::in(read_parse_tree), PT::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.

do_actually_read_module(Globals, DefaultModuleName, DefaultExpectationContexts,
        MaybeFileNameAndStream,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MakeDummyParseTree, ReadParseTree, ParseTree, Specs, Errors, !IO) :-
    (
        MaybeFileNameAndStream =
            ok(path_name_and_stream(_FilePathName, FileStream)),
        io.input_stream_name(FileStream, FileStreamName, !IO),
        (
            ( ReadModuleAndTimestamps = always_read_module(do_return_timestamp)
            ; ReadModuleAndTimestamps = dont_read_module_if_match(_)
            ),
            io.file_modification_time(FileStreamName, TimestampResult, !IO),
            (
                TimestampResult = ok(Timestamp),
                MaybeModuleTimestampRes =
                    yes(ok(time_t_to_timestamp(Timestamp)))
            ;
                TimestampResult = error(IOError),
                MaybeModuleTimestampRes = yes(error(IOError))
            )
        ;
            ReadModuleAndTimestamps =
                always_read_module(dont_return_timestamp),
            MaybeModuleTimestampRes = no
        ),
        ( if
            ReadModuleAndTimestamps = dont_read_module_if_match(OldTimestamp),
            MaybeModuleTimestampRes = yes(ok(OldTimestamp))
        then
            % XXX Currently smart recompilation won't work
            % if ModuleName \= DefaultModuleName.
            % In that case, smart recompilation will be disabled
            % and actually_read_module should never be passed an old timestamp.

            MakeDummyParseTree(DefaultModuleName, ParseTree),
            Specs = [],
            set.init(Errors)
        else
            ReadParseTree(FileStream, FileStreamName, Globals,
                DefaultModuleName, DefaultExpectationContexts,
                ParseTree, Specs, Errors, !IO)
        ),
        io.close_input(FileStream, !IO)
    ;
        MaybeFileNameAndStream = error(ErrorMsg),
        MakeDummyParseTree(DefaultModuleName, ParseTree),
        MaybeModuleTimestampRes = no,

        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words(ErrorMsg), nl],
        Spec = error_spec(severity_error, phase_read_files,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        Specs = [Spec],
        Errors = set.make_singleton_set(rme_could_not_open_file)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% This part of the module parses optimization files.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% opt:      STARTHERE module_start, item* ENDHERE
%
%---------------------------------------------------------------------------%

    % Read an optimization file (.opt or .trans_opt) from standard input.
    %
:- pred read_parse_tree_opt(opt_file_kind::in,
    io.text_input_stream::in, string::in, globals::in,
    module_name::in, list(prog_context)::in,
    parse_tree_opt::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

read_parse_tree_opt(OptFileKind, Stream, SourceFileName0, Globals,
        DefaultModuleName, DefaultExpectationContexts,
        ParseTree, !:Specs, !:Errors, !IO) :-
    !:Specs = [],
    set.init(!:Errors),
    counter.init(1, SeqNumCounter0),

    % We handle the first module declaration specially. Read the documentation
    % on read_first_module_decl for the reason.
    read_first_module_decl(Stream, require_module_decl,
        DefaultModuleName, DefaultExpectationContexts,
        ModuleDeclPresent, SourceFileName0, SourceFileName1,
        SeqNumCounter0, SeqNumCounter1, !Specs, !Errors, !IO),
    (
        ModuleDeclPresent = no_module_decl_present(LookAhead),
        (
            LookAhead = no_lookahead,
            LookAheadContext = term.context(SourceFileName0, 1)
        ;
            LookAhead = lookahead(_, LookAheadTerm),
            LookAheadContext = get_term_context(LookAheadTerm)
        ),
        report_missing_module_start(LookAheadContext, !Specs, !Errors),
        ModuleName = DefaultModuleName,
        ModuleNameContext = term.context_init,
        Uses = [],
        Items = []
    ;
        ModuleDeclPresent =
            wrong_module_decl_present(ModuleName, ModuleNameContext),
        report_wrong_module_start(ModuleNameContext,
            DefaultModuleName, ModuleName, !Specs, !Errors),
        Uses = [],
        Items = []
    ;
        ModuleDeclPresent =
            right_module_decl_present(ModuleName, ModuleNameContext),
        read_item_sequence(Stream, Globals, ModuleName,
            no_lookahead, FinalLookAhead, dont_allow_version_numbers, _,
            cord.init, InclsCord, cord.init, AvailsCord, cord.init, ItemsCord,
            SourceFileName1, SourceFileName, SeqNumCounter1, SeqNumCounter,
            !Specs, !Errors, !IO),
        check_for_unexpected_item(Stream, ModuleName, fk_opt(OptFileKind),
            FinalLookAhead, SourceFileName, SeqNumCounter,
            !Specs, !Errors, !IO),
        expect(cord.is_empty(InclsCord), $module, $pred, "Incls != []"),
        Avails = cord.list(AvailsCord),
        avail_imports_uses(Avails, Imports, Uses),
        expect(unify(Imports, []), $module, $pred, "Imports != []"),
        Items = cord.list(ItemsCord)
    ),
    ParseTree = parse_tree_opt(ModuleName, OptFileKind, ModuleNameContext,
        Uses, Items).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% This part of the module parses interface files.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% int:      STARTHERE module_start, section* ENDHERE
%
% section:  interface_marker, item*
%           implementation_marker, item*
%
%---------------------------------------------------------------------------%

    % Read an interface file (.int0, .int3, .int2 or .int).
    %
:- pred read_parse_tree_int(int_file_kind::in,
    io.text_input_stream::in, string::in, globals::in,
    module_name::in, list(prog_context)::in,
    parse_tree_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

read_parse_tree_int(IntFileKind, Stream, SourceFileName0, Globals,
        DefaultModuleName, DefaultExpectationContexts, ParseTree,
        !:Specs, !:Errors, !IO) :-
    !:Specs = [],
    set.init(!:Errors),
    counter.init(1, SeqNumCounter0),

    % We handle the first module declaration specially. Read the documentation
    % on read_first_module_decl for the reason.
    read_first_module_decl(Stream, require_module_decl,
        DefaultModuleName, DefaultExpectationContexts,
        ModuleDeclPresent, SourceFileName0, SourceFileName1,
        SeqNumCounter0, SeqNumCounter1, !Specs, !Errors, !IO),
    (
        ModuleDeclPresent = no_module_decl_present(LookAhead),
        (
            LookAhead = no_lookahead,
            LookAheadContext = term.context(SourceFileName0, 1)
        ;
            LookAhead = lookahead(_, LookAheadTerm),
            LookAheadContext = get_term_context(LookAheadTerm)
        ),
        report_missing_module_start(LookAheadContext, !Specs, !Errors),
        ModuleName = DefaultModuleName,
        ModuleNameContext = term.context_init,
        MaybeVersionNumbers = no,
        IntIncls = [],
        ImpIncls = [],
        IntAvails = [],
        ImpAvails = [],
        IntItems = [],
        ImpItems = []
    ;
        ModuleDeclPresent =
            wrong_module_decl_present(ModuleName, ModuleNameContext),
        report_wrong_module_start(ModuleNameContext,
            DefaultModuleName, ModuleName, !Specs, !Errors),
        MaybeVersionNumbers = no,
        IntIncls = [],
        ImpIncls = [],
        IntAvails = [],
        ImpAvails = [],
        IntItems = [],
        ImpItems = []
    ;
        ModuleDeclPresent =
            right_module_decl_present(ModuleName, ModuleNameContext),
        read_parse_tree_int_sections(Stream, Globals, ModuleName,
            no_lookahead, FinalLookAhead,
            allow_version_numbers_not_seen, VNInfo, RawItemBlocks,
            SourceFileName1, SourceFileName, SeqNumCounter1, SeqNumCounter,
            !Specs, !Errors, !IO),
        (
            VNInfo = allow_version_numbers_not_seen,
            MaybeVersionNumbers = no
        ;
            VNInfo = allow_version_numbers_seen(MVN),
            MaybeVersionNumbers = yes(MVN)
        ;
            VNInfo = dont_allow_version_numbers,
            % If you start with allow_version_numbers_not_seen, you shouldn't
            % end up with dont_allow_version_numbers.
            unexpected($module, $pred, "dont_allow_version_numbers")
        ),
        check_for_unexpected_item(Stream, ModuleName, fk_int(IntFileKind),
            FinalLookAhead, SourceFileName, SeqNumCounter,
            !Specs, !Errors, !IO),
        separate_int_imp_items(RawItemBlocks, IntIncls, ImpIncls,
            IntAvails, ImpAvails, IntItems, ImpItems)
    ),
    ParseTree = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntItems, ImpItems).

:- pred separate_int_imp_items(list(raw_item_block)::in,
    list(item_include)::out, list(item_include)::out,
    list(item_avail)::out, list(item_avail)::out,
    list(item)::out, list(item)::out) is det.

separate_int_imp_items([], [], [], [], [], [], []).
separate_int_imp_items([ItemBlock | ItemBlocks], IntIncls, ImpIncls,
        IntAvails, ImpAvails, IntItems, ImpItems) :-
    separate_int_imp_items(ItemBlocks, IntIncls0, ImpIncls0,
        IntAvails0, ImpAvails0, IntItems0, ImpItems0),
    ItemBlock = item_block(Section, _Context, Incls, Avails, Items),
    (
        Section = ms_interface,
        IntIncls = Incls ++ IntIncls0,
        IntAvails = Avails ++ IntAvails0,
        IntItems = Items ++ IntItems0,
        ImpIncls = ImpIncls0,
        ImpAvails = ImpAvails0,
        ImpItems = ImpItems0
    ;
        Section = ms_implementation,
        IntIncls = IntIncls0,
        IntAvails = IntAvails0,
        IntItems = IntItems0,
        ImpIncls = Incls ++ ImpIncls0,
        ImpAvails = Avails ++ ImpAvails0,
        ImpItems = Items ++ ImpItems0
    ).

%---------------------------------------------------------------------------%
%
% int:      module_start, STARTHERE section* ENDHERE
%
% section:  interface_marker, item*
%           implementation_marker, item*
%
%---------------------------------------------------------------------------%

:- pred read_parse_tree_int_sections(io.text_input_stream::in, globals::in,
    module_name::in, maybe_lookahead::in, maybe_lookahead::out,
    version_number_info::in, version_number_info::out,
    list(raw_item_block)::out, file_name::in, file_name::out,
    counter::in, counter::out, list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_parse_tree_int_sections(Stream, Globals, CurModuleName,
        InitLookAhead, FinalLookAhead, !VNInfo, RawItemBlocks,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO) :-
    read_parse_tree_int_section(Stream, Globals, CurModuleName,
        have_not_given_missing_section_start_warning,
        InitLookAhead, MidLookAhead, !VNInfo, MaybeHeadRawItemBlock,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
    (
        MaybeHeadRawItemBlock = no,
        FinalLookAhead = MidLookAhead,
        RawItemBlocks = []
    ;
        MaybeHeadRawItemBlock = yes(HeadRawItemBlock),
        read_parse_tree_int_sections(Stream, Globals, CurModuleName,
            MidLookAhead, FinalLookAhead, !VNInfo, TailRawItemBlocks,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
        RawItemBlocks = [HeadRawItemBlock | TailRawItemBlocks]
    ).

%---------------------------------------------------------------------------%
%
% int:      module_start, section*
%
% section:  STARTHERE interface_marker, (item | vns)* ENDHERE
%           STARTHERE implementation_marker, (item | vns)* ENDHERE
%
%---------------------------------------------------------------------------%

:- pred read_parse_tree_int_section(io.text_input_stream::in, globals::in,
    module_name::in, missing_section_start_warning::in,
    maybe_lookahead::in, maybe_lookahead::out,
    version_number_info::in, version_number_info::out,
    maybe(raw_item_block)::out, file_name::in, file_name::out,
    counter::in, counter::out, list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_parse_tree_int_section(Stream, Globals, CurModuleName,
        !.MissingStartSectionWarning, InitLookAhead, FinalLookAhead,
        !VNInfo, MaybeRawItemBlock,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO) :-
    read_next_item_or_marker(Stream, InitLookAhead, CurModuleName,
        !.SourceFileName, ReadIOMResult, !SeqNumCounter, !IO),
    (
        ReadIOMResult = read_iom_eof,
        % If we have found end-of-file, then we are done.
        MaybeRawItemBlock = no,
        FinalLookAhead = no_lookahead
    ;
        ReadIOMResult = read_iom_read_error(ItemSpec),
        % Add the read error to the list of errors and continue looking
        % for a section marker.
        !:Specs = [ItemSpec | !.Specs],
        set.insert(rme_could_not_read_term, !Errors),
        read_parse_tree_int_section(Stream, Globals, CurModuleName,
            !.MissingStartSectionWarning, no_lookahead, FinalLookAhead,
            !VNInfo, MaybeRawItemBlock,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
    ;
        ReadIOMResult = read_iom_parse_errors(IOMVarSet, IOMTerm,
            _ItemSpecs, _ItemErrors),
        Context = get_term_context(IOMTerm),
        % Generate an error for the missing section marker. Do not add
        % the parse errors to the list of errors YET; instead, leave the
        % unparseable term in the lookahead, and let the second call treat it
        % as the first term in the section.
        generate_missing_start_section_warning_int(CurModuleName,
            Context, !.MissingStartSectionWarning,
            _MissingStartSectionWarning, !Specs, !Errors),
        read_item_sequence_in_hdr_file_without_section_marker(Stream, Globals,
            CurModuleName, IOMVarSet, IOMTerm, FinalLookAhead,
            !VNInfo, MaybeRawItemBlock, !SourceFileName, !SeqNumCounter,
            !Specs, !Errors, !IO)
    ;
        ReadIOMResult = read_iom_ok(IOMVarSet, IOMTerm, IOM),
        (
            IOM = iom_marker_src_file(!:SourceFileName),
            read_parse_tree_int_section(Stream, Globals, CurModuleName,
                !.MissingStartSectionWarning, no_lookahead, FinalLookAhead,
                !VNInfo, MaybeRawItemBlock,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
        ;
            IOM = iom_marker_section(SectionKind, SectionContext,
                _SectionSeqNum),
            read_item_sequence(Stream, Globals, CurModuleName,
                no_lookahead, FinalLookAhead, !VNInfo, cord.init, InclsCord,
                cord.init, AvailsCord, cord.init, ItemsCord,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
            RawItemBlock = item_block(SectionKind, SectionContext,
                cord.list(InclsCord), cord.list(AvailsCord),
                cord.list(ItemsCord)),
            MaybeRawItemBlock = yes(RawItemBlock)
        ;
            IOM = iom_marker_version_numbers(MVN),
            record_version_numbers(MVN, IOMTerm, !VNInfo, !Specs),
            read_parse_tree_int_section(Stream, Globals, CurModuleName,
                !.MissingStartSectionWarning, InitLookAhead, FinalLookAhead,
                !VNInfo, MaybeRawItemBlock,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
        ;
            ( IOM = iom_item(_)
            ; IOM = iom_marker_include(_)
            ; IOM = iom_marker_avail(_)
            ),
            Context = get_term_context(IOMTerm),
            % Generate an error for the missing section marker.
            % Leave the term in the lookahead, and let the second call
            % treat it as the first term in the section.
            generate_missing_start_section_warning_int(CurModuleName,
                Context, !.MissingStartSectionWarning,
                _MissingStartSectionWarning, !Specs, !Errors),
            read_item_sequence_in_hdr_file_without_section_marker(Stream,
                Globals, CurModuleName, IOMVarSet, IOMTerm, FinalLookAhead,
                !VNInfo, MaybeRawItemBlock, !SourceFileName, !SeqNumCounter,
                !Specs, !Errors, !IO)
        ;
            ( IOM = iom_marker_module_start(_, _, _)
            ; IOM = iom_marker_module_end(_, _, _)
            ),
            FinalLookAhead = lookahead(IOMVarSet, IOMTerm),
            MaybeRawItemBlock = no
        )
    ).

:- pred generate_missing_start_section_warning_int(module_name::in,
    prog_context::in,
    missing_section_start_warning::in, missing_section_start_warning::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

generate_missing_start_section_warning_int(CurModuleName,
        Context, !MissingStartSectionWarning, !Specs, !Errors) :-
    (
        !.MissingStartSectionWarning =
            have_not_given_missing_section_start_warning,
        !:MissingStartSectionWarning =
            have_given_missing_section_start_warning,
        % XXX ITEM_LIST The wording here is modelled after the corresponding
        % error in source files, but maybe we should put the emphasis not
        % on the error itself, but on the fact that this file has an error
        % at all; since it should be automatically generated, it should not
        % have any errors at all. Any bug is in the compiler that generated
        % the interface file, NOT in the user's own code.
        %
        % XXX Note: for interface files we assume that the missing section
        % marker is for an interface section, as required in the example below,
        % while for source files we assume that it is for an implementation
        % section, since that is what backwards compatibility (and the law
        % of least astonishment) require.
        Pieces = [invis_order_default_start(1),
            words("Error: module"), qual_sym_name(CurModuleName),
            words("should start with either an"), decl("interface"),
            words("or"), decl("implementation"), words("declaration."), nl,
            words("The following assumes that"),
            words("the missing declaration is an"),
            decl("interface"), words("declaration."), nl],

        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs],
        set.insert(rme_no_section_decl_at_start, !Errors)
    ;
        !.MissingStartSectionWarning =
            have_given_missing_section_start_warning
        % Do not generate duplicate warnings.
    ).

:- pred read_item_sequence_in_hdr_file_without_section_marker(
    io.text_input_stream::in, globals::in,
    module_name::in, varset::in, term::in, maybe_lookahead::out,
    version_number_info::in, version_number_info::out,
    maybe(raw_item_block)::out, file_name::in, file_name::out,
    counter::in, counter::out, list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_item_sequence_in_hdr_file_without_section_marker(Stream, Globals,
        CurModuleName, IOMVarSet, IOMTerm, FinalLookAhead,
        !VNInfo, MaybeRawItemBlock, !SourceFileName, !SeqNumCounter,
        !Specs, !Errors, !IO) :-
    SectionKind = ms_interface,
    SectionContext = term.context_init,
    ItemSeqInitLookAhead = lookahead(IOMVarSet, IOMTerm),
    read_item_sequence(Stream, Globals, CurModuleName,
        ItemSeqInitLookAhead, FinalLookAhead, !VNInfo,
        cord.init, InclsCord, cord.init, AvailsCord, cord.init, ItemsCord,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
    RawItemBlock = item_block(SectionKind, SectionContext,
        cord.list(InclsCord), cord.list(AvailsCord), cord.list(ItemsCord)),
    MaybeRawItemBlock = yes(RawItemBlock).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% This part of the module parses source files.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% src:      STARTHERE module_start, component*, (module_end | epsilon) ENDHERE
%
% component: (interface_marker | epsilon), item*
%            (implementation_marker | epsilon), item*
%            module_start, component*, (module_end | epsilon)
%
% The marker that starts a component may be missing if the previous components
% were (one or more) nested submodules, and before those nested submodules,
% there was an interface or implementation section. That previous section
% may have started with an explicit section marker, or it may have been
% preceded by other nested submodules, and identified as an interface or
% implementation section by the preceding section, and so on.
%
% A submodule may have its final end_module marker missing if there is nothing
% following it: no item, no marker, only the implicit EOF.
%
%---------------------------------------------------------------------------%

:- pred read_parse_tree_src(io.text_input_stream::in, string::in, globals::in,
    module_name::in, list(prog_context)::in, parse_tree_src::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.

read_parse_tree_src(Stream, !.SourceFileName, Globals,
        DefaultModuleName, DefaultExpectationContexts,
        ParseTree, !:Specs, !:Errors, !IO) :-
    some [!SeqNumCounter] (
        !:Specs = [],
        set.init(!:Errors),
        counter.init(1, !:SeqNumCounter),

        % We handle the first module declaration specially. Read the
        % documentation on read_first_module_decl for the reason.
        read_first_module_decl(Stream, dont_require_module_decl,
            DefaultModuleName, DefaultExpectationContexts, ModuleDeclPresent,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
        (
            ModuleDeclPresent = no_module_decl_present(InitLookAhead),
            % Reparse the first term, this time treating it as occuring within
            % the scope of the implicit `:- module' decl rather than in the
            % root module.
            ModuleName = DefaultModuleName,
            (
                InitLookAhead = no_lookahead,
                ModuleNameContext = term.context_init
            ;
                InitLookAhead =
                    lookahead(_InitLookAheadVarSet, InitLookAheadTerm),
                ModuleNameContext = get_term_context(InitLookAheadTerm)
            )
        ;
            % XXX ITEM_LIST wrong_module_decl_present and
            % right_module_decl_present do the same thing.
            ModuleDeclPresent =
                wrong_module_decl_present(ModuleName, ModuleNameContext),
            InitLookAhead = no_lookahead
        ;
            ModuleDeclPresent =
                right_module_decl_present(ModuleName, ModuleNameContext),
            InitLookAhead = no_lookahead
        ),

        ContainingModules = [],
        MaybePrevSection = no,
        read_parse_tree_src_components(Stream, Globals, ModuleName,
            ContainingModules, MaybePrevSection,
            have_not_given_missing_section_start_warning,
            InitLookAhead, FinalLookAhead, cord.init, ModuleComponents,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
        check_for_unexpected_item(Stream, ModuleName, fk_src, FinalLookAhead,
            !.SourceFileName, !.SeqNumCounter, !Specs, !Errors, !IO),
        ParseTree = parse_tree_src(ModuleName, ModuleNameContext,
            ModuleComponents)
    ).

%---------------------------------------------------------------------------%
%
% src:      module_start, STARTHERE component*, (module_end | epsilon) ENDHERE
%
% component: (interface_marker | epsilon), item*
%            (implementation_marker | epsilon), item*
%            module_start, STARTHERE component*, (module_end | epsilon) ENDHERE
%
%---------------------------------------------------------------------------%

:- pred read_parse_tree_src_components(io.text_input_stream::in, globals::in,
    module_name::in, list(module_name)::in,
    maybe(pair(module_section, prog_context))::in,
    missing_section_start_warning::in,
    maybe_lookahead::in, maybe_lookahead::out,
    cord(module_component)::in, cord(module_component)::out,
    file_name::in, file_name::out, counter::in, counter::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_parse_tree_src_components(Stream, Globals,
        CurModuleName, ContainingModules,
        MaybePrevSection, !.MissingStartSectionWarning,
        InitLookAhead, FinalLookAhead, !ModuleComponents,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO) :-
    read_next_item_or_marker(Stream, InitLookAhead, CurModuleName,
        !.SourceFileName, ReadIOMResult, !SeqNumCounter, !IO),
    (
        ReadIOMResult = read_iom_eof,
        % If we have found end-of-file, then we are done.
        FinalLookAhead = no_lookahead
    ;
        ReadIOMResult = read_iom_read_error(ItemSpec),
        % Add the new errors to the list of errors and continue looking
        % for a section marker.
        !:Specs = [ItemSpec | !.Specs],
        set.insert(rme_could_not_read_term, !Errors),
        read_parse_tree_src_components(Stream, Globals, CurModuleName,
            ContainingModules, MaybePrevSection, !.MissingStartSectionWarning,
            no_lookahead, FinalLookAhead, !ModuleComponents,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
    ;
        ReadIOMResult = read_iom_parse_errors(IOMVarSet, IOMTerm,
            _Specs, _Errors),
        % Generate an error for the missing section marker.
        % Leave the term in the lookahead, but otherwise handle the term
        % as it were an unexpected but perfectly parseable term, i.e. follow
        % the pattern of the iom_item case below.
        Context = get_term_context(IOMTerm),
        generate_missing_start_section_warning_src(CurModuleName,
            Context, !.MissingStartSectionWarning, _MissingStartSectionWarning,
            !Specs, !Errors),
        SectionKind = ms_implementation,
        SectionContext = term.context_init,
        ItemSeqInitLookAhead = lookahead(IOMVarSet, IOMTerm),
        read_item_sequence(Stream, Globals, CurModuleName,
            ItemSeqInitLookAhead, ItemSeqFinalLookAhead,
            dont_allow_version_numbers, _, cord.init, InclsCord,
            cord.init, AvailsCord, cord.init, ItemsCord,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
        add_section_component(SectionKind, SectionContext,
            InclsCord, AvailsCord, ItemsCord, !ModuleComponents),
        % We have read in one component; recurse to read in other components.
        read_parse_tree_src_components(Stream, Globals, CurModuleName,
            ContainingModules, yes(SectionKind - SectionContext),
            have_not_given_missing_section_start_warning,
            ItemSeqFinalLookAhead, FinalLookAhead, !ModuleComponents,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
    ;
        ReadIOMResult = read_iom_ok(IOMVarSet, IOMTerm, IOM),
        (
            IOM = iom_marker_src_file(!:SourceFileName),
            read_parse_tree_src_components(Stream, Globals, CurModuleName,
                ContainingModules, MaybePrevSection,
                !.MissingStartSectionWarning, no_lookahead, FinalLookAhead,
                !ModuleComponents, !SourceFileName, !SeqNumCounter,
                !Specs, !Errors, !IO)
        ;
            IOM = iom_marker_version_numbers(_),
            Pieces = [words("Error: unexpected version_numbers record"),
                words("in source file."), nl],
            Msg = simple_msg(get_term_context(IOMTerm), [always(Pieces)]),
            Spec = error_spec(severity_error, phase_read_files, [Msg]),
            !:Specs = [Spec | !.Specs],
            read_parse_tree_src_components(Stream, Globals, CurModuleName,
                ContainingModules, MaybePrevSection,
                !.MissingStartSectionWarning, no_lookahead, FinalLookAhead,
                !ModuleComponents, !SourceFileName, !SeqNumCounter,
                !Specs, !Errors, !IO)
        ;
            IOM = iom_marker_module_start(RawStartModuleName, StartContext,
                _StartSeqNum),
            (
                RawStartModuleName = unqualified(RawBaseName),
                StartModuleName = qualified(CurModuleName, RawBaseName)
            ;
                RawStartModuleName = qualified(RawModuleName, RawBaseName),
                ( if
                    partial_sym_name_matches_full(RawModuleName, CurModuleName)
                then
                    StartModuleName = qualified(CurModuleName, RawBaseName)
                else
                    Pieces = [words("Error: module qualification of"),
                        words("nested submodule"),
                        qual_sym_name(RawStartModuleName),
                        words("does not match the then-current module,"),
                        qual_sym_name(CurModuleName), suffix("."), nl],
                    Msg = always(Pieces),
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(StartContext, [Msg])]),
                    !:Specs = [Spec | !.Specs],
                    % Recover partially by ignoring the bad module
                    % qualification. The recovery is only partial because
                    % an end_module marker that matches the incorrect module
                    % name will get another error message about the
                    % `:- end_module' not matching the `:- module' declaration,
                    % which will be at least a bit misleading.
                    StartModuleName = qualified(CurModuleName, RawBaseName)
                )
            ),
            read_parse_tree_src_submodule(Stream, Globals, ContainingModules,
                MaybePrevSection, StartModuleName, StartContext,
                no_lookahead, SubModuleFinalLookAhead, !ModuleComponents,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
            % We have read in one component; recurse to read in others.
            read_parse_tree_src_components(Stream, Globals, CurModuleName,
                ContainingModules, MaybePrevSection,
                !.MissingStartSectionWarning,
                SubModuleFinalLookAhead, FinalLookAhead,
                !ModuleComponents, !SourceFileName, !SeqNumCounter,
                !Specs, !Errors, !IO)
        ;
            ( IOM = iom_marker_section(_, _, _)
            ; IOM = iom_marker_include(_)
            ; IOM = iom_marker_avail(_)
            ; IOM = iom_item(_)
            ),
            (
                IOM = iom_marker_section(SectionKind,
                    SectionContext, _SectionSeqNum),
                ItemSeqInitLookAhead = no_lookahead
            ;
                ( IOM = iom_marker_include(_)
                ; IOM = iom_marker_avail(_)
                ; IOM = iom_item(_)
                ),
                (
                    MaybePrevSection = yes(SectionKind - SectionContext)
                    % When a nested module occurs in a section, the section
                    % continues after the nested module without the need
                    % for a new section declaration.
                ;
                    MaybePrevSection = no,
                    Context = get_term_context(IOMTerm),
                    generate_missing_start_section_warning_src(CurModuleName,
                        Context, !.MissingStartSectionWarning,
                        _MissingStartSectionWarning, !Specs, !Errors),
                    % The following code is duplicated in the case for
                    % read_iom_parse_errors above.
                    SectionKind = ms_implementation,
                    SectionContext = term.context_init
                ),
                ItemSeqInitLookAhead = lookahead(IOMVarSet, IOMTerm)
            ),
            % The following code is duplicated in the case for
            % read_iom_parse_errors above.
            read_item_sequence(Stream, Globals, CurModuleName,
                ItemSeqInitLookAhead, ItemSeqFinalLookAhead,
                dont_allow_version_numbers, _, cord.init, InclsCord,
                cord.init, AvailsCord, cord.init, ItemsCord,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
            add_section_component(SectionKind, SectionContext,
                InclsCord, AvailsCord, ItemsCord, !ModuleComponents),
            % We have read in one component; recurse to read in other
            % components.
            read_parse_tree_src_components(Stream, Globals, CurModuleName,
                ContainingModules, yes(SectionKind - SectionContext),
                have_not_given_missing_section_start_warning,
                ItemSeqFinalLookAhead, FinalLookAhead, !ModuleComponents,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
        ;
            IOM = iom_marker_module_end(EndedModuleName, EndContext,
                _EndSeqNum),
            handle_module_end_marker(CurModuleName, ContainingModules,
                IOMVarSet, IOMTerm, EndedModuleName, EndContext,
                FinalLookAhead, !Specs, !Errors)
        )
    ).

:- pred add_section_component(module_section::in, prog_context::in,
    cord(item_include)::in, cord(item_avail)::in, cord(item)::in,
    cord(module_component)::in, cord(module_component)::out) is det.

add_section_component(SectionKind, SectionContext,
        InclsCord, AvailsCord, ItemsCord, !ModuleComponents) :-
    ( if
        cord.is_empty(InclsCord),
        cord.is_empty(AvailsCord),
        cord.is_empty(ItemsCord)
    then
        true
    else
        Component = mc_section(SectionKind, SectionContext,
            InclsCord, AvailsCord, ItemsCord),
        !:ModuleComponents = cord.snoc(!.ModuleComponents, Component)
    ).

:- pred generate_missing_start_section_warning_src(module_name::in,
    prog_context::in,
    missing_section_start_warning::in, missing_section_start_warning::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

generate_missing_start_section_warning_src(CurModuleName,
        Context, !MissingStartSectionWarning, !Specs, !Errors) :-
    (
        !.MissingStartSectionWarning =
            have_not_given_missing_section_start_warning,
        !:MissingStartSectionWarning =
            have_given_missing_section_start_warning,
        MissingSectionPieces = [invis_order_default_start(1),
            words("Error: module"),
            qual_sym_name(CurModuleName), words("should start with"),
            words("either an"), decl("interface"), words("or an"),
            decl("implementation"), words("declaration."), nl,
            words("The following assumes that"),
            words("the missing declaration is an"),
            decl("implementation"), words("declaration."), nl],
        MissingSectionSpec =
            error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context,
                    [always(MissingSectionPieces)])]),
        !:Specs = [MissingSectionSpec | !.Specs],
        set.insert(rme_no_section_decl_at_start, !Errors)
    ;
        !.MissingStartSectionWarning =
            have_given_missing_section_start_warning
        % Do not generate duplicate warnings.
    ).

:- pred read_parse_tree_src_submodule(io.text_input_stream::in, globals::in,
    list(module_name)::in, maybe(pair(module_section, prog_context))::in,
    module_name::in, prog_context::in,
    maybe_lookahead::in, maybe_lookahead::out,
    cord(module_component)::in, cord(module_component)::out,
    file_name::in, file_name::out,
    counter::in, counter::out, list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_parse_tree_src_submodule(Stream, Globals, ContainingModules,
        MaybePrevSection, StartModuleName, StartContext,
        InitLookAhead, FinalLookAhead, !ModuleComponents,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO) :-
    (
        MaybePrevSection = yes(SectionKind - SectionContext)
    ;
        MaybePrevSection = no,
        NoSectionPieces = [words("Error: nested submodule"),
            qual_sym_name(StartModuleName), words("should be preceded by"),
            words("either an"), decl("interface"), words("or an"),
            decl("implementation"), words("declaration."), nl,
            words("The following assumes that"),
            words("the missing declaration is an"),
            decl("interface"), words("declaration."), nl],
        NoSectionSpec =
            error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(StartContext, [always(NoSectionPieces)])]),
        !:Specs = [NoSectionSpec | !.Specs],
        % XXX ITEM_LIST Should this be a situation-specific rme_X value?
        set.insert(rme_no_section_decl_at_start, !Errors),
        SectionKind = ms_interface,
        SectionContext = term.context_init
    ),
    NestedContainingModules = [StartModuleName | ContainingModules],
    NestedMaybePrevSection = no,
    read_parse_tree_src_components(Stream, Globals, StartModuleName,
        NestedContainingModules, NestedMaybePrevSection,
        have_not_given_missing_section_start_warning,
        InitLookAhead, FinalLookAhead, cord.init, NestedModuleComponents,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
    SubModuleParseTreeSrc = parse_tree_src(StartModuleName, StartContext,
        NestedModuleComponents),
    Component = mc_nested_submodule(SectionKind, SectionContext,
        SubModuleParseTreeSrc),
    !:ModuleComponents = cord.snoc(!.ModuleComponents, Component).

:- pred handle_module_end_marker(module_name::in, list(module_name)::in,
    varset::in, term::in, module_name::in, prog_context::in,
    maybe_lookahead::out, list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

handle_module_end_marker(CurModuleName, ContainingModules, IOMVarSet, IOMTerm,
        EndedModuleName, EndContext, FinalLookAhead, !Specs, !Errors) :-
    ( if
        CurModuleName = EndedModuleName
    then
        FinalLookAhead = no_lookahead
    else if
        partial_sym_name_matches_full(EndedModuleName,
            CurModuleName)
    then
        % XXX ITEM_LIST Should this be an error? Warning?
        FinalLookAhead = no_lookahead
    else if
        % XXX ITEM_LIST Do thing without nondet code.
        some [ContainingModule] (
            list.member(ContainingModule, ContainingModules),
            partial_sym_name_matches_full(EndedModuleName,
                ContainingModule)
        )
    then
        Pieces = [words("Error: missing"), decl("end_module"),
            words("declaration for"), qual_sym_name(CurModuleName),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(EndContext, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs],
        set.insert(rme_bad_module_end, !Errors),
        FinalLookAhead = lookahead(IOMVarSet, IOMTerm)
    else
        Pieces = [words("Error: this"), decl("end_module"),
            words("declaration for"), qual_sym_name(EndedModuleName),
            words("is not for the module at whose end it appears,"),
            words("which is"), qual_sym_name(CurModuleName), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(EndContext, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs],
        set.insert(rme_bad_module_end, !Errors),
        % Eat the bad end_module declaration.
        FinalLookAhead = no_lookahead
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% This part of the module contains utility predicates.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_module_decl_present
    --->    no_module_decl_present(maybe_lookahead)
    ;       wrong_module_decl_present(module_name, prog_context)
    ;       right_module_decl_present(module_name, prog_context).

    % We used to have to jump through a few hoops when reading the first item,
    % to allow us to recover from a missing initial `:- module' declaration.
    %
    % We used to solve this dilemma by first parsing the first item
    % in the root scope, and then if it turns out to not be a `:- module'
    % declaration, we used special code to reparse it in the default module
    % scope. We now also reparse it in the default module context, but
    % using the general lookahead mechanism that the rest of the parser
    % also uses.
    %
    % XXX ITEM_LIST SHOULD we recover from a missing initial `:- module'
    % declaration? The reason is that in order to parse an item, we need
    % to know which module it is defined in (because we do some module
    % qualification and checking of module qualifiers at parse time),
    % but the initial `:- module' declaration and the declaration
    % that follows it occur in different scopes, so we need to know
    % what it is that we are parsing before we can parse it!
    %
:- pred read_first_module_decl(io.text_input_stream::in,
    maybe_require_module_decl::in,
    module_name::in, list(prog_context)::in,
    maybe_module_decl_present::out,
    file_name::in, file_name::out, counter::in, counter::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out,
    io::di, io::uo) is det.

read_first_module_decl(Stream, RequireModuleDecl,
        DefaultModuleName, DefaultExpectationContexts,
        ModuleDeclPresent, !SourceFileName, !SeqNumCounter,
        !Specs, !Errors, !IO) :-
    % Parse the first term, treating it as occurring within the scope
    % of the special "root" module (so that any `:- module' declaration
    % is taken to be a non-nested module unless explicitly qualified).
    parser.read_term_filename(Stream, !.SourceFileName, FirstReadTerm, !IO),
    read_term_to_iom_result(root_module_name, !.SourceFileName,
        FirstReadTerm, !SeqNumCounter, MaybeFirstIOM),
    (
        MaybeFirstIOM = read_iom_ok(FirstVarSet, FirstTerm, FirstIOM),
        (
            FirstIOM = iom_marker_src_file(!:SourceFileName),
            % Apply and then skip `pragma source_file' decls, by calling
            % ourselves recursively with the new source file name.
            read_first_module_decl(Stream, RequireModuleDecl,
                DefaultModuleName, DefaultExpectationContexts,
                ModuleDeclPresent, !SourceFileName, !SeqNumCounter,
                !Specs, !Errors, !IO)
        ;
            FirstIOM = iom_marker_module_start(StartModuleName,
                ModuleNameContext, _ModuleNameSeqNum),
            % The first term is a `:- module' decl, as expected.
            % Check whether it matches the expected module name.
            % If it doesn't, report a warning.
            ( if
                partial_sym_name_matches_full(DefaultModuleName,
                    StartModuleName)
            then
                ModuleName = StartModuleName,
                ModuleDeclPresent =
                    right_module_decl_present(ModuleName, ModuleNameContext)
            else if
                partial_sym_name_matches_full(StartModuleName,
                    DefaultModuleName)
            then
                % XXX ITEM_LIST Should this be an error?
                ModuleName = DefaultModuleName,
                ModuleDeclPresent =
                    right_module_decl_present(ModuleName, ModuleNameContext)
            else
                check_module_has_expected_name(!.SourceFileName,
                    DefaultModuleName, DefaultExpectationContexts,
                    StartModuleName, yes(ModuleNameContext), NameSpecs),
                !:Specs = NameSpecs ++ !.Specs,
                set.insert(rme_unexpected_module_name, !Errors),

                % Which one should we use here? We used to use the default
                % module name (computed from the filename) but now we use
                % the declared one.
                ModuleName = StartModuleName,
                ModuleDeclPresent =
                    wrong_module_decl_present(ModuleName, ModuleNameContext)
            )
        ;
            ( FirstIOM = iom_marker_module_end(_, _, _)
            ; FirstIOM = iom_marker_version_numbers(_)
            ; FirstIOM = iom_marker_section(_, _, _)
            ; FirstIOM = iom_marker_include(_)
            ; FirstIOM = iom_marker_avail(_)
            ; FirstIOM = iom_item(_)
            ),
            FirstContext = get_term_context(FirstTerm),
            report_missing_module_start(FirstContext, !Specs, !Errors),
            FirstLookAhead = lookahead(FirstVarSet, FirstTerm),
            ModuleDeclPresent = no_module_decl_present(FirstLookAhead)
        )
    ;
        MaybeFirstIOM = read_iom_parse_errors(FirstVarSet, FirstTerm, _, _),
        FirstContext = get_term_context(FirstTerm),
        report_missing_module_start(FirstContext, !Specs, !Errors),
        LookAhead = lookahead(FirstVarSet, FirstTerm),
        ModuleDeclPresent = no_module_decl_present(LookAhead)
    ;
        ( MaybeFirstIOM = read_iom_eof
        ; MaybeFirstIOM = read_iom_read_error(_)
        ),
        term.context_init(!.SourceFileName, 1, FirstContext),
        report_missing_module_start(FirstContext, !Specs, !Errors),
        ModuleDeclPresent = no_module_decl_present(no_lookahead)
        % XXX ITEM_LIST Should report "stop processing".
    ).

%---------------------------------------------------------------------------%

:- type version_number_info
    --->    dont_allow_version_numbers
    ;       allow_version_numbers_not_seen
    ;       allow_version_numbers_seen(version_numbers).

    % Read a sequence of items, until we find a marker that indicates
    % a change in section or module. If and when we find such a marker,
    % we stop reading, and return the term of that marker as the final
    % lookahead.
    %
    % We use the standard two level loop to avoid running out of stack
    % on long item sequences in grades that do not allow tail recursion.
    %
    % XXX ITEM_LIST specialize the modes for lookahead/no_lookahead.
    %
:- pred read_item_sequence(io.text_input_stream::in, globals::in,
    module_name::in, maybe_lookahead::in, maybe_lookahead::out,
    version_number_info::in, version_number_info::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out,
    file_name::in, file_name::out, counter::in, counter::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_item_sequence(Stream, Globals, ModuleName, InitLookAhead, FinalLookAhead,
        !VNInfo, !InclsCord, !AvailsCord, !ItemsCord,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO) :-
    read_item_sequence_inner(Stream, Globals, ModuleName, 1024, NumItemsLeft,
        InitLookAhead, MidLookAhead, !VNInfo,
        !InclsCord, !AvailsCord, !ItemsCord,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO),
    ( if NumItemsLeft = 0 then
        read_item_sequence(Stream, Globals, ModuleName, MidLookAhead,
            FinalLookAhead, !VNInfo, !InclsCord, !AvailsCord, !ItemsCord,
            !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
    else
        FinalLookAhead = MidLookAhead
    ).

    % XXX ITEM_LIST specialize the modes for lookahead/no_lookahead.
    %
:- pred read_item_sequence_inner(io.text_input_stream::in, globals::in,
    module_name::in, int::in, int::out,
    maybe_lookahead::in, maybe_lookahead::out,
    version_number_info::in, version_number_info::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item)::in, cord(item)::out, file_name::in, file_name::out,
    counter::in, counter::out, list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_item_sequence_inner(Stream, Globals, ModuleName, !NumItemsLeft,
        InitLookAhead, FinalLookAhead, !VNInfo,
        !InclsCord, !AvailsCord, !ItemsCord,
        !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO) :-
    ( if !.NumItemsLeft =< 0 then
        FinalLookAhead = InitLookAhead
    else
        read_next_item_or_marker(Stream, InitLookAhead, ModuleName,
            !.SourceFileName, ReadIOMResult, !SeqNumCounter, !IO),
        (
            ReadIOMResult = read_iom_eof,
            FinalLookAhead = no_lookahead
        ;
            (
                ReadIOMResult = read_iom_read_error(ItemSpec),
                ItemSpecs = [ItemSpec],
                ItemErrors = set.make_singleton_set(rme_could_not_read_term)
            ;
                ReadIOMResult = read_iom_parse_errors(_, _,
                    ItemSpecs, ItemErrors)
            ),
            !:Specs = ItemSpecs ++ !.Specs,
            !:Errors = set.union(!.Errors, ItemErrors),
            read_item_sequence_inner(Stream, Globals, ModuleName,
                !NumItemsLeft, no_lookahead, FinalLookAhead, !VNInfo,
                !InclsCord, !AvailsCord, !ItemsCord,
                !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
        ;
            ReadIOMResult = read_iom_ok(IOMVarSet, IOMTerm, IOM),
            !:NumItemsLeft = !.NumItemsLeft - 1,
            (
                ( IOM = iom_marker_module_start(_, _, _)
                ; IOM = iom_marker_module_end(_, _, _)
                ; IOM = iom_marker_section(_, _, _)
                ),
                FinalLookAhead = lookahead(IOMVarSet, IOMTerm)
            ;
                IOM = iom_marker_version_numbers(MVN),
                record_version_numbers(MVN, IOMTerm, !VNInfo, !Specs),
                read_item_sequence_inner(Stream, Globals, ModuleName,
                    !NumItemsLeft, no_lookahead, FinalLookAhead, !VNInfo,
                    !InclsCord, !AvailsCord, !ItemsCord,
                    !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
            ;
                (
                    IOM = iom_marker_src_file(!:SourceFileName)
                ;
                    IOM = iom_marker_include(Incls),
                    Incls = one_or_more(HeadIncl, TailIncls),
                    !:InclsCord = !.InclsCord ++
                        cord.from_list([HeadIncl | TailIncls])
                ;
                    IOM = iom_marker_avail(Avails),
                    Avails = one_or_more(HeadAvail, TailAvails),
                    !:AvailsCord = !.AvailsCord ++
                        cord.from_list([HeadAvail | TailAvails])
                ;
                    IOM = iom_item(Item0),
                    ( if Item0 = item_nothing(ItemNothingInfo) then
                        process_item_nothing_warning(Globals,
                            ItemNothingInfo, Item, !Specs, !Errors)
                    else
                        Item = Item0
                    ),
                    !:ItemsCord = cord.snoc(!.ItemsCord, Item)
                ),
                read_item_sequence_inner(Stream, Globals, ModuleName,
                    !NumItemsLeft, no_lookahead, FinalLookAhead, !VNInfo,
                    !InclsCord, !AvailsCord, !ItemsCord,
                    !SourceFileName, !SeqNumCounter, !Specs, !Errors, !IO)
            )
        )
    ).

:- pred record_version_numbers(version_numbers::in, term::in,
    version_number_info::in, version_number_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_version_numbers(MVN, IOMTerm, !VNInfo, !Specs) :-
    (
        !.VNInfo = allow_version_numbers_not_seen,
        !:VNInfo = allow_version_numbers_seen(MVN)
    ;
        !.VNInfo = allow_version_numbers_seen(_),
        Pieces = [words("Error: duplicate version_numbers"),
            words("record. This indicates an internal error"),
            words("in the Mercury compiler that"),
            words("generated this file."), nl],
        Msg = simple_msg(get_term_context(IOMTerm),
            [always(Pieces)]),
        Spec = error_spec(severity_error, phase_read_files, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        !.VNInfo = dont_allow_version_numbers,
        Pieces = [words("Error: version number records"),
            words("should not appear anywhere"),
            words("except in automatically generated"),
            words("interface files."), nl],
        Msg = simple_msg(get_term_context(IOMTerm),
            [always(Pieces)]),
        Spec = error_spec(severity_error, phase_read_files, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

    % process_item_nothing_warning(Globals, ItemNothingInfo, !ItemsCord,
    %     !Specs, !Errors):
    %
    % If the given item_nothing_info has a (possibly conditional) warning
    % embedded inside it, and if the condition (if present) is true,
    % then put that warning into !Specs and (if asked for) into !Errors.
    %
    % In any case, return the item_nothing, stripped of any warnings,
    % in NoWarnItem.
    %
:- pred process_item_nothing_warning(globals::in,
    item_nothing_info::in, item::out,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

process_item_nothing_warning(Globals, ItemNothingInfo, NoWarnItem,
        !Specs, !Errors) :-
    ItemNothingInfo = item_nothing_info(MaybeWarning, Context, NothingSeqNum),
    (
        MaybeWarning = no,
        % There is no warning to strip away.
        NoWarnItem = item_nothing(ItemNothingInfo)
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
        NoWarnItemNothingInfo = item_nothing_info(no, Context, NothingSeqNum),
        NoWarnItem = item_nothing(NoWarnItemNothingInfo)
    ).

%---------------------------------------------------------------------------%

:- type maybe_lookahead
    --->    no_lookahead
    ;       lookahead(varset, term).

:- pred read_next_item_or_marker(io.text_input_stream::in,
    maybe_lookahead::in, module_name::in, string::in, read_iom_result::out,
    counter::in, counter::out, io::di, io::uo) is det.

read_next_item_or_marker(Stream, InitLookAhead, ModuleName, SourceFileName,
        ReadIOMResult, !SeqNumCounter, !IO) :-
    (
        InitLookAhead = no_lookahead,
        parser.read_term_filename(Stream, SourceFileName, ReadTermResult, !IO),
        read_term_to_iom_result(ModuleName, SourceFileName, ReadTermResult,
            !SeqNumCounter, ReadIOMResult)
    ;
        InitLookAhead = lookahead(LookAheadVarSet, LookAheadTerm),
        term_to_iom_result(ModuleName, LookAheadVarSet, LookAheadTerm,
            !SeqNumCounter, ReadIOMResult)
    ).

%---------------------------------------------------------------------------%

:- type read_iom_result
    --->    read_iom_eof
    ;       read_iom_read_error(error_spec)
    ;       read_iom_parse_errors(varset, term,
                list(error_spec), set(read_module_error))
    ;       read_iom_ok(varset, term, item_or_marker).

:- pred read_term_to_iom_result(module_name::in, string::in, read_term::in,
    counter::in, counter::out, read_iom_result::out) is det.

read_term_to_iom_result(ModuleName, FileName, ReadTermResult,
        !SeqNumCounter, ReadIOMResult) :-
    % XXX ITEM_LIST Should add a prefix to eof, error, and term
    % in library/term_io.m?
    (
        ReadTermResult = eof,
        ReadIOMResult = read_iom_eof
    ;
        ReadTermResult = error(ErrorMsg, LineNumber),
        % XXX Do we need to add an "Error:" prefix?
        Pieces = [words(ErrorMsg), suffix("."), nl],
        Context = term.context_init(FileName, LineNumber),
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        ReadIOMResult = read_iom_read_error(Spec)
    ;
        ReadTermResult = term(VarSet, Term),
        term_to_iom_result(ModuleName, VarSet, Term, !SeqNumCounter,
            ReadIOMResult)
    ).

:- pred term_to_iom_result(module_name::in, varset::in, term::in,
    counter::in, counter::out, read_iom_result::out) is det.

term_to_iom_result(ModuleName, VarSet, Term, !SeqNumCounter, ReadIOMResult) :-
    counter.allocate(SeqNum, !SeqNumCounter),
    parse_item_or_marker(ModuleName, VarSet, Term, SeqNum, MaybeItemOrMarker),
    (
        MaybeItemOrMarker = ok1(ItemOrMarker),
        ReadIOMResult = read_iom_ok(VarSet, Term, ItemOrMarker)
    ;
        MaybeItemOrMarker = error1(Specs),
        ReadIOMResult = read_iom_parse_errors(VarSet, Term, Specs,
            set.make_singleton_set(rme_could_not_parse_item))
    ).

%---------------------------------------------------------------------------%

:- pred report_missing_module_start(prog_context::in,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

report_missing_module_start(FirstContext, !Specs, !Errors) :-
    Pieces = [invis_order_default_start(0),
        words("Error: module must start with a"),
        decl("module"), words("declaration."), nl],
    Msgs = [always(Pieces)],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(FirstContext, Msgs)]),
    !:Specs = [Spec | !.Specs],
    set.insert(rme_no_module_decl_at_start, !Errors).

:- pred report_wrong_module_start(prog_context::in,
    module_name::in, module_name::in,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

report_wrong_module_start(FirstContext, Expected, Actual, !Specs, !Errors) :-
    Pieces = [words("Error: module starts with the wrong"),
        decl("module"), words("declaration."), nl,
        words("Expected module"), qual_sym_name(Expected), suffix(","),
        words("found module"), qual_sym_name(Actual), suffix("."), nl],
    Msgs = [always(Pieces)],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(FirstContext, Msgs)]),
    !:Specs = [Spec | !.Specs],
    set.insert(rme_no_module_decl_at_start, !Errors).

    % The predicate that reads in source file handles all items and markers
    % it reads in, and stops only at an end_module declaration that matches
    % the name of the top level module in the file. If FileKind is
    % fk_src, we look for any items after this end_module marker.
    %
    % The predicates that read in interface and optimization files
    % handle only the items they expect in those files, since their contents
    % should be automatically generated by mmc itself, stopping (and returning
    % as lookahead) at items that don't fit the expected structure of those
    % files. If FileKind is fk_int or fk_opt, we look for any such unexpected
    % items.
    %
:- pred check_for_unexpected_item(io.text_input_stream::in,
    module_name::in, file_kind::in,
    maybe_lookahead::in, file_name::in, counter::in,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

check_for_unexpected_item(Stream, ModuleName, FileKind, FinalLookAhead,
        SourceFileName, SeqNumCounter0, !Specs, !Errors, !IO) :-
    read_next_item_or_marker(Stream, FinalLookAhead, ModuleName,
        SourceFileName, IOMResult, SeqNumCounter0, _SeqNumCounter, !IO),
    (
        IOMResult = read_iom_eof
    ;
        IOMResult = read_iom_read_error(ItemSpec),
        !:Specs = [ItemSpec | !.Specs],
        set.insert(rme_could_not_read_term, !Errors)
    ;
        IOMResult = read_iom_parse_errors(_VarSet, Term,
            ItemSpecs, ItemErrors),
        !:Specs = ItemSpecs ++ !.Specs,
        !:Errors = set.union(!.Errors, ItemErrors),
        report_unexpected_term_at_end(FileKind, Term, !Specs, !Errors)
    ;
        IOMResult = read_iom_ok(_IOMVarSet, IOMTerm, _IOM),
        report_unexpected_term_at_end(FileKind, IOMTerm, !Specs, !Errors)
    ).

:- pred report_unexpected_term_at_end(file_kind::in, term::in,
    list(error_spec)::in, list(error_spec)::out,
    read_module_errors::in, read_module_errors::out) is det.

report_unexpected_term_at_end(FileKind, Term, !Specs, !Errors) :-
    Context = get_term_context(Term),
    (
        FileKind = fk_src,
        Error = rme_end_module_not_at_end_of_src,
        Pieces = [words("Error: item(s) after the"),
            decl("end_module"), words("declaration."), nl]
    ;
        FileKind = fk_int(_IntFileKind),
        Error = rme_unexpected_term_in_int_or_opt,
        Pieces = [words("Error: unexpected item in interface file"), nl]
    ;
        FileKind = fk_opt(_OptFileKind),
        Error = rme_unexpected_term_in_int_or_opt,
        Pieces = [words("Error: unexpected item in optimization file"), nl]
    ),
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]),
    !:Specs = [Spec | !.Specs],
    set.insert(Error, !Errors).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_module.
%---------------------------------------------------------------------------%
