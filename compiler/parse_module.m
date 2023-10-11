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
%
% The usual stages are:
%   lexical analysis: chars -> tokens
%   parsing:          tokens -> structured parse tree
%
% Our stages are:
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
% return it to their caller, for the next step in the caller to process.
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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.find_module.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.

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

    % actually_read_module_src(Globals, FileNameAndStream,
    %   DefaultModuleName, DefaultExpectationContexts,
    %   ReadModuleAndTimestamps, MaybeModuleTimestampRes,
    %   ParseTree, Errors, !IO):
    %
    % Read a Mercury source program from FileNameAndStream.
    % Close the stream when the reading is done. Return the parse tree
    % of that module in ParseTree (which may be a dummy if the file
    % couldn't be opened), and an indication of the errors found
    % in Specs and Errors.
    %
    % For the meaning of ReadModuleAndTimestamps and MaybeModuleTimestampRes,
    % read the comments on read_module_src in read_modules.m.
    % XXX ITEM_LIST Move actually_read_module_{src,int,opt} to read_modules.m.
    %
:- pred actually_read_module_src(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_src)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % actually_read_module_intN(Globals, FileNameAndStream,
    %   DefaultModuleName, DefaultExpectationContexts,
    %   ReadModuleAndTimestamps, MaybeModuleTimestampRes,
    %   ParseTree, Errors, !IO):
    %
    % Analogous to actually_read_module_src, but opens the specified kind
    % of interface file for DefaultModuleName.
    %
:- pred actually_read_module_int0(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int0)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred actually_read_module_int1(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int1)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred actually_read_module_int2(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int2)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred actually_read_module_int3(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int3)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % actually_read_module_{plain,trans}_opt(Globals, FileNameAndStream,
    %   DefaultModuleName, ParseTree, Errors, !IO):
    %
    % Analogous to actually_read_module_src, but opens the specified kind
    % of optimization file for DefaultModuleName.
    %
:- pred actually_read_module_plain_opt(globals::in, path_name_and_stream::in,
    module_name::in, maybe(parse_tree_plain_opt)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred actually_read_module_trans_opt(globals::in, path_name_and_stream::in,
    module_name::in, maybe(parse_tree_trans_opt)::out, read_module_errors::out,
    io::di, io::uo) is det.

:- type maybe_require_module_decl
    --->    dont_require_module_decl
    ;       require_module_decl.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.convert_parse_tree.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.item_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_item.
:- import_module parse_tree.parse_types.
:- import_module recompilation.

:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module io.file.
:- import_module mercury_term_lexer.
:- import_module mercury_term_parser.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

:- type missing_section_start_warning
    --->    have_not_given_missing_section_start_warning
    ;       have_given_missing_section_start_warning.

%---------------------------------------------------------------------------%

peek_at_file(FileStream, DefaultModuleName, DefaultExpectationContexts,
        SourceFileName0, ModuleName, Specs, !IO) :-
    io.read_file_as_string_and_num_code_units(FileStream, MaybeResult, !IO),
    (
        MaybeResult = ok2(FileString, FileStringLen),
        counter.init(1, SeqNumCounter0),
        LineContext0 = line_context(1, 0),
        LinePosn0 = line_posn(0),
        read_first_module_decl(FileString, FileStringLen,
            dont_require_module_decl, DefaultModuleName,
            DefaultExpectationContexts, ModuleDeclPresent,
            may_change_source_file_name, SourceFileName0, _SourceFileName,
            SeqNumCounter0, _SeqNumCounter,
            LineContext0, _LineContext, LinePosn0, _LinePosn),
        (
            ModuleDeclPresent = no_module_decl_present(_MaybeLookAhead,
                _Context, _NoModuleSpec),
            ModuleName = DefaultModuleName
        ;
            ModuleDeclPresent = wrong_module_decl_present(ModuleName,
                _ModuleNameContext, _WrongSpec)
        ;
            ModuleDeclPresent = right_module_decl_present(ModuleName,
                _ModuleNameContext)
        ),
        Specs = []
    ;
        MaybeResult = error2(_PartialFileString, _FileStringLen, ErrorCode),
        ModuleName = DefaultModuleName,
        io.error_message(ErrorCode, ErrorMsg0),
        ErrorMsg = "I/O error: " ++ ErrorMsg0,
        io_error_to_error_spec(ErrorMsg, Spec, !IO),
        Specs = [Spec]
    ).

%---------------------------------------------------------------------------%

actually_read_module_src(_Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTree, Errors, !IO) :-
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        read_parse_tree_src, MaybeParseTree, Errors, !IO).

%---------------------------------------------------------------------------%

actually_read_module_int0(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt0, Errors, !IO) :-
    IntFileKind = ifk_int0,
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        read_parse_tree_int(IntFileKind), MaybeInitParseTreeInt, Errors0, !IO),
    (
        MaybeInitParseTreeInt = yes(InitParseTreeInt),
        check_convert_parse_tree_int_to_int0(InitParseTreeInt, ParseTreeInt0,
            [], ConvertSpecs),
        MaybeParseTreeInt0 = yes(ParseTreeInt0),
        maybe_add_convert_specs(Globals, ConvertSpecs, Errors0, Errors)
    ;
        MaybeInitParseTreeInt = no,
        MaybeParseTreeInt0 = no,
        Errors = Errors0
    ).

actually_read_module_int1(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt1, Errors, !IO) :-
    IntFileKind = ifk_int1,
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        read_parse_tree_int(IntFileKind), MaybeInitParseTreeInt, Errors0, !IO),
    (
        MaybeInitParseTreeInt = yes(InitParseTreeInt),
        check_convert_parse_tree_int_to_int1(InitParseTreeInt, ParseTreeInt1,
            [], ConvertSpecs),
        MaybeParseTreeInt1 = yes(ParseTreeInt1),
        maybe_add_convert_specs(Globals, ConvertSpecs, Errors0, Errors)
    ;
        MaybeInitParseTreeInt = no,
        MaybeParseTreeInt1 = no,
        Errors = Errors0
    ).

actually_read_module_int2(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt2, Errors, !IO) :-
    IntFileKind = ifk_int2,
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        read_parse_tree_int(IntFileKind), MaybeInitParseTreeInt, Errors0, !IO),
    (
        MaybeInitParseTreeInt = yes(InitParseTreeInt),
        check_convert_parse_tree_int_to_int2(InitParseTreeInt, ParseTreeInt2,
            [], ConvertSpecs),
        MaybeParseTreeInt2 = yes(ParseTreeInt2),
        maybe_add_convert_specs(Globals, ConvertSpecs, Errors0, Errors)
    ;
        MaybeInitParseTreeInt = no,
        MaybeParseTreeInt2 = no,
        Errors = Errors0
    ).

actually_read_module_int3(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt3, Errors, !IO) :-
    IntFileKind = ifk_int3,
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        read_parse_tree_int(IntFileKind), MaybeInitParseTreeInt, Errors0, !IO),
    (
        MaybeInitParseTreeInt = yes(InitParseTreeInt),
        check_convert_parse_tree_int_to_int3(InitParseTreeInt, ParseTreeInt3,
            [], ConvertSpecs),
        MaybeParseTreeInt3 = yes(ParseTreeInt3),
        maybe_add_convert_specs(Globals, ConvertSpecs, Errors0, Errors)
    ;
        MaybeInitParseTreeInt = no,
        MaybeParseTreeInt3 = no,
        Errors = Errors0
    ).

:- pred maybe_add_convert_specs(globals::in, list(error_spec)::in,
    read_module_errors::in, read_module_errors::out) is det.

maybe_add_convert_specs(Globals, ConvertSpecs, !Errors) :-
    globals.lookup_bool_option(Globals, halt_at_invalid_interface,
        HaltAtInvalidInterface),
    (
        HaltAtInvalidInterface = no
    ;
        HaltAtInvalidInterface = yes,
        add_any_nec_errors(ConvertSpecs, !Errors)
    ).

%---------------------------------------------------------------------------%

actually_read_module_plain_opt(_Globals, FileNameAndStream, DefaultModuleName,
        MaybeParseTreePlainOpt, Errors, !IO) :-
    DefaultExpectationContexts = [],
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        always_read_module(dont_return_timestamp), _,
        read_parse_tree_opt(ofk_opt), MaybeParseTreeOpt, Errors0, !IO),
    (
        MaybeParseTreeOpt = yes(ParseTreeOpt),
        check_convert_parse_tree_opt_to_plain_opt(ParseTreeOpt,
            ParseTreePlainOpt, [], ConvertSpecs),
        MaybeParseTreePlainOpt = yes(ParseTreePlainOpt),
        add_any_nec_errors(ConvertSpecs, Errors0, Errors)
    ;
        MaybeParseTreeOpt = no,
        MaybeParseTreePlainOpt = no,
        Errors = Errors0
    ).

actually_read_module_trans_opt(_Globals, FileNameAndStream, DefaultModuleName,
        MaybeParseTreeTransOpt, Errors, !IO) :-
    DefaultExpectationContexts = [],
    do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        always_read_module(dont_return_timestamp), _,
        read_parse_tree_opt(ofk_trans_opt), MaybeParseTreeOpt, Errors0, !IO),
    (
        MaybeParseTreeOpt = yes(ParseTreeOpt),
        check_convert_parse_tree_opt_to_trans_opt(ParseTreeOpt,
            ParseTreeTransOpt, [], ConvertSpecs),
        MaybeParseTreeTransOpt = yes(ParseTreeTransOpt),
        add_any_nec_errors(ConvertSpecs, Errors0, Errors)
    ;
        MaybeParseTreeOpt = no,
        MaybeParseTreeTransOpt = no,
        Errors = Errors0
    ).

%---------------------------------------------------------------------------%

:- pred report_module_has_unexpected_name(file_name::in,
    module_name::in, list(prog_context)::in,
    module_name::in, maybe(term.context)::in, error_spec::out) is det.

report_module_has_unexpected_name(FileName, ExpectedName, ExpectationContexts,
        ActualName, MaybeActualContext, Spec) :-
    ( if
        MaybeActualContext = yes(ActualContext),
        not term_context.is_dummy_context(ActualContext)
    then
        MaybeContext = MaybeActualContext
    else
        MaybeContext = maybe.no
    ),
    MainPieces = [words("Error: file"), quote(FileName),
        words("contains the wrong module."),
        words("Expected module"), qual_sym_name(ExpectedName), suffix(","),
        words("found module"), qual_sym_name(ActualName), suffix("."), nl],
    MainMsg = error_msg(MaybeContext, always_treat_as_first, 0,
        [always(MainPieces)]),
    list.sort_and_remove_dups(ExpectationContexts, SortedExpectationContexts0),
    list.delete_all(SortedExpectationContexts0, dummy_context,
        SortedExpectationContexts),
    list.map(expectation_context_to_msg, SortedExpectationContexts, SubMsgs),
    % We make the warning conditional on the warn_wrong_module_name option.
    % This option is turned on by default, and it is turned off automatically
    % by the compiler only in situations where it clearly makes sense to do so
    % (in handle_options.m and options.m). If it is turned off manually
    % by the user, he/she presumably has a good reason.
    %
    % Despite the option name having the "warn_" prefix,
    % the severity is an error. The severity is deliberate.
    % XXX The option should be renamed, but there is no obvious
    % non-misleading name.
    Spec = conditional_spec($pred, warn_wrong_module_name, yes,
        severity_error, phase_module_name, [MainMsg | SubMsgs]).

:- pred expectation_context_to_msg(prog_context::in, error_msg::out) is det.

expectation_context_to_msg(Context, SubMsg) :-
    SubPieces = [words("The expected name is specified here."), nl],
    SubMsg = simplest_msg(Context, SubPieces).

%---------------------%

:- type read_parse_tree(PT) ==
    pred(file_name, string, int, line_context, line_posn, module_name,
        list(prog_context), maybe(PT), read_module_errors).
:- inst read_parse_tree
    == (pred(in, in, in, in, in, in, in, out, out) is det).

:- type make_dummy_parse_tree(PT) == pred(module_name, PT).
:- inst make_dummy_parse_tree == (pred(in, out) is det).

%---------------------------------------------------------------------------%

    % This predicate implements all three of actually_read_module_{src,int,opt}
    % through the polymorphism provided by the ReadParseTree (sometimes the
    % MakeDummyParseTree) higher order variables. All the actual parsing
    % takes place inside ReadParseTree, which will be one of
    % read_parse_tree_src, read_parse_tree_int and read_parse_tree_src.
    %
:- pred do_actually_read_module(path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    read_parse_tree(PT)::in(read_parse_tree), maybe(PT)::out,
    read_module_errors::out, io::di, io::uo) is det.

do_actually_read_module(FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        ReadParseTree, MaybeParseTree, Errors, !IO) :-
    FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
    io.input_stream_name(FileStream, FileStreamName, !IO),
    (
        ( ReadModuleAndTimestamps = always_read_module(do_return_timestamp)
        ; ReadModuleAndTimestamps = dont_read_module_if_match(_)
        ),
        io.file.file_modification_time(FileStreamName, TimestampResult, !IO),
        (
            TimestampResult = ok(Timestamp),
            MaybeModuleTimestampRes =
                yes(ok(time_t_to_timestamp(Timestamp)))
        ;
            TimestampResult = error(IOError),
            MaybeModuleTimestampRes = yes(error(IOError))
        )
    ;
        ReadModuleAndTimestamps = always_read_module(dont_return_timestamp),
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
        MaybeParseTree = no,
        Errors = init_read_module_errors
    else
        io.read_file_as_string_and_num_code_units(FileStream,
            MaybeResult, !IO),
        (
            MaybeResult = ok2(FileString, NumCodeUnits),
            FileStringLen = string.length(FileString),
            ( if NumCodeUnits = FileStringLen then
                true
            else
                Msg = string.format(
                    "NumCodeUnits = %d, FileStringLen = %d\n<<<\n%s>>>\n",
                    [i(NumCodeUnits), i(FileStringLen), s(FileString)]),
                unexpected($pred, Msg)
            ),
            LineContext0 = line_context(1, 0),
            LinePosn0 = line_posn(0),
            ReadParseTree(FileStreamName, FileString, FileStringLen,
                LineContext0, LinePosn0,
                DefaultModuleName, DefaultExpectationContexts,
                MaybeParseTree, Errors)
        ;
            MaybeResult = error2(_PartialStr, _PartialLen, ErrorCode),
            MaybeParseTree = no,
            io.error_message(ErrorCode, ErrorMsg0),
            ErrorMsg = "I/O error: " ++ ErrorMsg0,
            io_error_to_read_module_errors(ErrorMsg,
                frme_could_not_read_file, Errors, !IO)
        )
    ),
    % XXX This should be moved next to the code that opens FileStream,
    % in one of our ancestors.
    io.close_input(FileStream, !IO).

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
:- pred read_parse_tree_opt(opt_file_kind::in, file_name::in,
    string::in, int::in, line_context::in, line_posn::in,
    module_name::in, list(prog_context)::in, maybe(parse_tree_opt)::out,
    read_module_errors::out) is det.

read_parse_tree_opt(OptFileKind, SourceFileName0,
        FileString, FileStringLen, !.LineContext, !.LinePosn,
        DefaultModuleName, DefaultExpectationContexts,
        MaybeParseTree, !:Errors) :-
    read_module_header(FileString, FileStringLen,
        DefaultModuleName, DefaultExpectationContexts, SourceFileName0,
        MaybeModuleHeader, SeqNumCounter1, !LineContext, !LinePosn),
    (
        MaybeModuleHeader = no_valid_module_header(!:Errors),
        MaybeParseTree = no
    ;
        MaybeModuleHeader = valid_module_header(ModuleName, ModuleNameContext),
        % XXX We should allow the update of SourceFileName.
        read_item_sequence(FileString, FileStringLen, ModuleName,
            no_lookahead, FinalLookAhead,
            cord.init, InclsCord, cord.init, AvailsCord,
            cord.init, FIMsCord, cord.init, ItemsCord,
            SourceFileName0, SourceFileName, SeqNumCounter1, SeqNumCounter,
            init_read_module_errors, !:Errors, !LineContext, !LinePosn),
        check_for_unexpected_item_at_end(SourceFileName, FileString,
            FileStringLen, ModuleName, fk_opt(OptFileKind), FinalLookAhead,
            SeqNumCounter, !Errors, !.LineContext, !.LinePosn),
        expect(cord.is_empty(InclsCord), $pred, "Incls != []"),
        Avails = cord.list(AvailsCord),
        avail_imports_uses(Avails, Imports, Uses),
        expect(unify(Imports, []), $pred, "Imports != []"),
        FIMs = cord.list(FIMsCord),
        Items = cord.list(ItemsCord),
        ParseTree = parse_tree_opt(ModuleName, OptFileKind, ModuleNameContext,
            Uses, FIMs, Items),
        MaybeParseTree = yes(ParseTree)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% This part of the module parses interface files.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% int:      STARTHERE module_start, vns? section, section? ENDHERE
%
% section:  interface_marker, item*
%           implementation_marker, item*
%
%---------------------------------------------------------------------------%

    % Read an interface file (.int0, .int3, .int2 or .int).
    %
:- pred read_parse_tree_int(int_file_kind::in, file_name::in,
    string::in, int::in, line_context::in, line_posn::in,
    module_name::in, list(prog_context)::in, maybe(parse_tree_int)::out,
    read_module_errors::out) is det.

read_parse_tree_int(IntFileKind, SourceFileName,
        FileString, FileStringLen, !.LineContext, !.LinePosn,
        DefaultModuleName, DefaultExpectationContexts,
        MaybeParseTreeInt, !:Errors) :-
    some [!SeqNumCounter, !LookAhead] (
        read_module_header(FileString, FileStringLen,
            DefaultModuleName, DefaultExpectationContexts, SourceFileName,
            MaybeModuleHeader, !:SeqNumCounter, !LineContext, !LinePosn),
        (
            MaybeModuleHeader = no_valid_module_header(!:Errors),
            MaybeParseTreeInt = no
        ;
            MaybeModuleHeader =
                valid_module_header(ModuleName, ModuleNameContext),
            !:LookAhead = no_lookahead,
            read_any_version_number_item(FileString, FileStringLen,
                ModuleName, SourceFileName, !LookAhead, VersionNumbersResult,
                !SeqNumCounter, !LineContext, !LinePosn),
            (
                VersionNumbersResult = vnr_error(Spec, Error),
                !:Errors = init_read_module_errors,
                add_nonfatal_error(Error, [Spec], !Errors),
                MaybeParseTreeInt = no
            ;
                VersionNumbersResult = vnr_ok(MaybeVersionNumbers),
                read_parse_tree_int_sections(FileString, FileStringLen,
                    ModuleName, ModuleNameContext, IntFileKind,
                    SourceFileName, MaybeVersionNumbers,
                    !LookAhead, MaybeParseTreeInt, !SeqNumCounter,
                    !:Errors, !LineContext, !LinePosn),
                check_for_unexpected_item_at_end(SourceFileName,
                    FileString, FileStringLen, ModuleName, fk_int(IntFileKind),
                    !.LookAhead, !.SeqNumCounter, !Errors,
                    !.LineContext, !.LinePosn)
            )
        )
    ).

:- type version_number_result
    --->    vnr_ok(maybe_version_numbers)
    ;       vnr_error(error_spec, nonfatal_read_module_error).

:- pred read_any_version_number_item(string::in, int::in,
    module_name::in, file_name::in, maybe_lookahead::in, maybe_lookahead::out,
    version_number_result::out, counter::in, counter::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_any_version_number_item(FileString, FileStringLen,
        ModuleName, SourceFileName, InitLookAhead, FinalLookAhead,
        VersionNumberResult, !SeqNumCounter, !LineContext, !LinePosn) :-
    get_next_item_or_marker(SourceFileName, FileString, FileStringLen,
        InitLookAhead, ModuleName, ReadIOMResult,
        !SeqNumCounter, !LineContext, !LinePosn),
    (
        ReadIOMResult = read_iom_eof,
        % If we have found end-of-file, then we are done.
        FinalLookAhead = no_lookahead,
        VersionNumberResult = vnr_ok(no_version_numbers)
    ;
        ReadIOMResult = read_iom_parse_term_error(ItemSpec),
        % We used to continue looking for a version number in this situation,
        % but (a) given that version numbers are put into .intN files
        % by automatic processes, *any* error detected in them renders
        % the entire contents of the file suspect, and (b) when we are
        % expecting a section marker but find read_iom_parse_term_error,
        % we don't continue looking.
        FinalLookAhead = no_lookahead,
        VersionNumberResult = vnr_error(ItemSpec, rme_could_not_read_term)
    ;
        ReadIOMResult = read_iom_parse_item_errors(_, _, _),
        % Ignore the errors; our caller will compute them again,
        % and record them, if and when it parses FinalLookAhead again.
        FinalLookAhead = lookahead(ReadIOMResult),
        VersionNumberResult = vnr_ok(no_version_numbers)
    ;
        ReadIOMResult = read_iom_ok(_IOMVarSet, _IOMTerm, IOM),
        % If the term we have read in is a version number item, return it.
        % If the term is anything else, leave it in the input to be handled
        % later.
        (
            IOM = iom_marker_version_numbers(VN),
            FinalLookAhead = no_lookahead,
            VersionNumberResult = vnr_ok(version_numbers(VN))
        ;
            ( IOM = iom_marker_module_start(_, _, _)
            ; IOM = iom_marker_module_end(_, _, _)
            ; IOM = iom_marker_src_file(_)
            ; IOM = iom_marker_section(_, _, _)
            ; IOM = iom_marker_include(_)
            ; IOM = iom_marker_avail(_)
            ; IOM = iom_marker_fim(_)
            ; IOM = iom_item(_)
            ; IOM = iom_item_and_error_specs(_, _)
            % The reasoning for read_iom_parse_item_errors also applies
            % to iom_item_and_specs.
            ; IOM = iom_handled_no_error
            ; IOM = iom_handled_error(_)
            ),
            FinalLookAhead = lookahead(ReadIOMResult),
            VersionNumberResult = vnr_ok(no_version_numbers)
        )
    ).

%---------------------------------------------------------------------------%
%
% int:      module_start, vns? STARTHERE int_section, int_section? ENDHERE
%
%---------------------------------------------------------------------------%

    % XXX ITEM_LIST We should return *either* a ParseTreeInt *or* some errors,
    % but *not* both.
    %
:- pred read_parse_tree_int_sections(string::in, int::in,
    module_name::in, prog_context::in, int_file_kind::in, file_name::in,
    maybe_version_numbers::in, maybe_lookahead::in, maybe_lookahead::out,
    maybe(parse_tree_int)::out, counter::in, counter::out,
    read_module_errors::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_parse_tree_int_sections(FileString, FileStringLen,
        ModuleName, ModuleNameContext, IntFileKind, SourceFileName,
        MaybeVersionNumbers, !LookAhead, MaybeParseTreeInt, !SeqNumCounter,
        !:Errors, !LineContext, !LinePosn) :-
    read_parse_tree_int_section(FileString, FileStringLen,
        ModuleName, SourceFileName, "interface", !LookAhead,
        MaybeFirstRawItemBlock, !SeqNumCounter,
        init_read_module_errors, !:Errors, !LineContext, !LinePosn),
    (
        MaybeFirstRawItemBlock = no,
        % Ignore MaybeVersionNumbers if the interface is empty,
        % since there are no items whose version numbers we may be
        % interested in.
        ParseTreeInt = parse_tree_int(ModuleName, IntFileKind,
            ModuleNameContext, no_version_numbers,
            [], [], [], [], [], [], [], []),
        MaybeParseTreeInt = yes(ParseTreeInt)
    ;
        MaybeFirstRawItemBlock = yes({FirstRawItemBlock, _}),
        FirstRawItemBlock = item_block(_, FirstSectionKind,
            FirstIncls, FirstAvails, FirstFIMs, FirstItems),
        (
            FirstSectionKind = ms_interface,
            read_parse_tree_int_section(FileString, FileStringLen,
                ModuleName, SourceFileName, "implementation", !LookAhead,
                MaybeSecondRawItemBlock, !SeqNumCounter, !Errors,
                !LineContext, !LinePosn),
            (
                MaybeSecondRawItemBlock = no,
                ParseTreeInt = parse_tree_int(ModuleName, IntFileKind,
                    ModuleNameContext, MaybeVersionNumbers,
                    FirstIncls, [], FirstAvails, [],
                    FirstFIMs, [], FirstItems, []),
                MaybeParseTreeInt = yes(ParseTreeInt)
            ;
                MaybeSecondRawItemBlock =
                    yes({SecondRawItemBlock, SectionContext}),
                SecondRawItemBlock = item_block(_, SecondSectionKind,
                    SecondIncls, SecondAvails, SecondFIMs, SecondItems),
                (
                    SecondSectionKind = ms_interface,
                    Pieces = [words("Error: an interface file"),
                        words("should not have two consecutive"),
                        words("interface sections."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, SectionContext, Pieces),
                    add_nonfatal_error(rme_nec, [Spec], !Errors),
                    MaybeParseTreeInt = no
                ;
                    SecondSectionKind = ms_implementation,
                    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind,
                        ModuleNameContext, MaybeVersionNumbers,
                        FirstIncls, SecondIncls, FirstAvails, SecondAvails,
                        FirstFIMs, SecondFIMs, FirstItems, SecondItems),
                    MaybeParseTreeInt = yes(ParseTreeInt)
                )
            )
        ;
            FirstSectionKind = ms_implementation,
            % This should not happen, but I (zs) cannot come up
            % with a convincing argument for *why*.
            ParseTreeInt = parse_tree_int(ModuleName, IntFileKind,
                ModuleNameContext, MaybeVersionNumbers,
                [], FirstIncls, [], FirstAvails,
                [], FirstFIMs, [], FirstItems),
            MaybeParseTreeInt = yes(ParseTreeInt)
        )
    ).

%---------------------------------------------------------------------------%
%
% int_section:
%           STARTHERE interface_marker, item* ENDHERE
%           STARTHERE implementation_marker, item* ENDHERE
%
%---------------------------------------------------------------------------%

:- pred read_parse_tree_int_section(string::in, int::in,
    module_name::in, file_name::in, string::in,
    maybe_lookahead::in, maybe_lookahead::out,
    maybe({raw_item_block, prog_context})::out,
    counter::in, counter::out, read_module_errors::in, read_module_errors::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_parse_tree_int_section(FileString, FileStringLen,
        ModuleName, SourceFileName, ExpectedSectionKindStr,
        InitLookAhead, FinalLookAhead, MaybeRawItemBlock,
        !SeqNumCounter, !Errors, !LineContext, !LinePosn) :-
    get_next_item_or_marker(SourceFileName, FileString, FileStringLen,
        InitLookAhead, ModuleName, ReadIOMResult, !SeqNumCounter,
        !LineContext, !LinePosn),
    (
        (
            ReadIOMResult = read_iom_eof
        ;
            ReadIOMResult = read_iom_parse_term_error(ItemSpec),
            add_nonfatal_error(rme_could_not_read_term, [ItemSpec], !Errors)
        ;
            ReadIOMResult = read_iom_parse_item_errors(_IOMVarSet, _IOMTerm,
                ItemSpecs),
            add_nonfatal_error(rme_could_not_parse_item, ItemSpecs, !Errors)
        ),
        MaybeRawItemBlock = no,
        FinalLookAhead = no_lookahead
    ;
        ReadIOMResult = read_iom_ok(_IOMVarSet, IOMTerm, IOM),
        (
            IOM = iom_marker_section(SectionKind, SectionContext,
                _SectionSeqNum),
            read_item_sequence(FileString, FileStringLen,
                ModuleName, no_lookahead, FinalLookAhead,
                cord.init, InclsCord, cord.init, AvailsCord,
                cord.init, FIMsCord, cord.init, ItemsCord,
                SourceFileName, _UpdatedSourceFileName,
                !SeqNumCounter, !Errors, !LineContext, !LinePosn),
            RawItemBlock = item_block(ModuleName, SectionKind,
                cord.list(InclsCord), cord.list(AvailsCord),
                cord.list(FIMsCord), cord.list(ItemsCord)),
            MaybeRawItemBlock = yes({RawItemBlock, SectionContext})
        ;
            ( IOM = iom_marker_src_file(_)
            ; IOM = iom_marker_module_start(_, _, _)
            ; IOM = iom_marker_module_end(_, _, _)
            ; IOM = iom_marker_version_numbers(_)
            ; IOM = iom_marker_include(_)
            ; IOM = iom_marker_avail(_)
            ; IOM = iom_marker_fim(_)
            ; IOM = iom_item(_)
            % The fact that IOM is not a section marker trumps any error_specs
            % in iom_item_and_error_specs, and in iom_handled_error.
            ; IOM = iom_item_and_error_specs(_, _)
            ; IOM = iom_handled_no_error
            ; IOM = iom_handled_error(_)
            ),
            Context = get_term_context(IOMTerm),
            IOMPieces = iom_desc_pieces(IOM),
            Pieces = [words("Error: expected the start of an"),
                words(ExpectedSectionKindStr), words("section, got")] ++
                IOMPieces ++ [suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            add_nonfatal_error(rme_nec, [Spec], !Errors),
            FinalLookAhead = lookahead(ReadIOMResult),
            MaybeRawItemBlock = no
        )
    ).

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

:- pred read_parse_tree_src(file_name::in, string::in, int::in,
    line_context::in, line_posn::in, module_name::in, list(prog_context)::in,
    maybe(parse_tree_src)::out, read_module_errors::out) is det.

read_parse_tree_src(!.SourceFileName, FileString, FileStringLen,
        !.LineContext, !.LinePosn,
        DefaultModuleName, DefaultExpectationContexts,
        MaybeParseTree, !:Errors) :-
    some [!SeqNumCounter] (
        !:Errors = init_read_module_errors,
        counter.init(1, !:SeqNumCounter),

        % We handle the first module declaration specially. Read the
        % documentation on read_first_module_decl for the reason.
        read_first_module_decl(FileString, FileStringLen,
            dont_require_module_decl, DefaultModuleName,
            DefaultExpectationContexts, ModuleDeclPresent,
            may_change_source_file_name, !SourceFileName,
            !SeqNumCounter, !LineContext, !LinePosn),
        (
            ModuleDeclPresent = no_module_decl_present(InitLookAhead,
                InitLookAheadContext, NoModuleSpec),
            add_nonfatal_error(rme_no_module_decl_at_start, [NoModuleSpec],
                !Errors),
            % Reparse the first term, this time treating it as occuring within
            % the scope of the implicit `:- module' decl rather than in the
            % root module.
            ModuleName = DefaultModuleName,
            ModuleNameContext = InitLookAheadContext
        ;
            % XXX ITEM_LIST wrong_module_decl_present and
            % right_module_decl_present do the same thing.
            ModuleDeclPresent = wrong_module_decl_present(ModuleName,
                ModuleNameContext, WrongSpec),
            add_nonfatal_error(rme_unexpected_module_name, [WrongSpec],
                !Errors),
            InitLookAhead = no_lookahead
        ;
            ModuleDeclPresent =
                right_module_decl_present(ModuleName, ModuleNameContext),
            InitLookAhead = no_lookahead
        ),

        ContainingModules = [],
        MaybePrevSection = no,
        read_parse_tree_src_components(FileString, FileStringLen,
            ModuleName, ContainingModules, MaybePrevSection,
            have_not_given_missing_section_start_warning,
            InitLookAhead, FinalLookAhead, cord.init, ModuleComponents,
            !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn),
        check_for_unexpected_item_at_end(!.SourceFileName, FileString,
            FileStringLen, ModuleName, fk_src, FinalLookAhead, !.SeqNumCounter,
            !Errors, !.LineContext, !.LinePosn),
        ParseTree = parse_tree_src(ModuleName, ModuleNameContext,
            ModuleComponents),
        MaybeParseTree = yes(ParseTree)
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

:- pred read_parse_tree_src_components(string::in, int::in,
    module_name::in, list(module_name)::in,
    maybe(pair(module_section, prog_context))::in,
    missing_section_start_warning::in,
    maybe_lookahead::in, maybe_lookahead::out,
    cord(module_component)::in, cord(module_component)::out,
    file_name::in, file_name::out, counter::in, counter::out,
    read_module_errors::in, read_module_errors::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_parse_tree_src_components(FileString, FileStringLen,
        CurModuleName, ContainingModules,
        MaybePrevSection, !.MissingStartSectionWarning,
        InitLookAhead, FinalLookAhead, !ModuleComponents, !SourceFileName,
        !SeqNumCounter, !Errors, !LineContext, !LinePosn) :-
    get_next_item_or_marker(!.SourceFileName, FileString, FileStringLen,
        InitLookAhead, CurModuleName, ReadIOMResult, !SeqNumCounter,
        !LineContext, !LinePosn),
    (
        ReadIOMResult = read_iom_eof,
        % If we have found end-of-file, then we are done.
        FinalLookAhead = no_lookahead
    ;
        ReadIOMResult = read_iom_parse_term_error(ItemSpec),
        add_nonfatal_error(rme_could_not_read_term, [ItemSpec], !Errors),
        % Continue looking for a section marker.
        read_parse_tree_src_components(FileString, FileStringLen,
            CurModuleName, ContainingModules,
            MaybePrevSection, !.MissingStartSectionWarning,
            no_lookahead, FinalLookAhead, !ModuleComponents, !SourceFileName,
            !SeqNumCounter, !Errors, !LineContext, !LinePosn)
    ;
        ReadIOMResult = read_iom_parse_item_errors(_IOMVarSet, IOMTerm,
            _Specs),
        % Generate an error for the missing section marker.
        % Leave the term in the lookahead, but otherwise handle the term
        % as it were an unexpected but perfectly parseable term, i.e. follow
        % the pattern of the iom_item case below.
        Context = get_term_context(IOMTerm),
        generate_missing_start_section_warning_src(CurModuleName,
            Context, !.MissingStartSectionWarning, _MissingStartSectionWarning,
            !Errors),
        SectionKind = ms_implementation,
        SectionContext = dummy_context,
        ItemSeqInitLookAhead = lookahead(ReadIOMResult),
        read_item_sequence(FileString, FileStringLen, CurModuleName,
            ItemSeqInitLookAhead, ItemSeqFinalLookAhead,
            cord.init, InclsCord, cord.init, AvailsCord,
            cord.init, FIMsCord, cord.init, ItemsCord,
            !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn),
        add_section_component(CurModuleName, SectionKind, SectionContext,
            InclsCord, AvailsCord, FIMsCord, ItemsCord, !ModuleComponents),
        % We have read in one component; recurse to read in other components.
        read_parse_tree_src_components(FileString, FileStringLen,
            CurModuleName, ContainingModules,
            yes(SectionKind - SectionContext),
            have_not_given_missing_section_start_warning,
            ItemSeqFinalLookAhead, FinalLookAhead, !ModuleComponents,
            !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn)
    ;
        ReadIOMResult = read_iom_ok(_IOMVarSet, IOMTerm, IOM),
        (
            IOM = iom_marker_src_file(!:SourceFileName),
            read_parse_tree_src_components(FileString, FileStringLen,
                CurModuleName, ContainingModules, MaybePrevSection,
                !.MissingStartSectionWarning, no_lookahead, FinalLookAhead,
                !ModuleComponents, !SourceFileName, !SeqNumCounter,
                !Errors, !LineContext, !LinePosn)
        ;
            IOM = iom_marker_version_numbers(_),
            Pieces = [words("Error: unexpected version_numbers record"),
                words("in source file."), nl],
            Spec = simplest_spec($pred, severity_error, phase_read_files,
                get_term_context(IOMTerm), Pieces),
            add_nonfatal_error(rme_nec, [Spec], !Errors),
            read_parse_tree_src_components(FileString, FileStringLen,
                CurModuleName, ContainingModules, MaybePrevSection,
                !.MissingStartSectionWarning, no_lookahead, FinalLookAhead,
                !ModuleComponents, !SourceFileName, !SeqNumCounter,
                !Errors, !LineContext, !LinePosn)
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
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, StartContext, Pieces),
                    add_nonfatal_error(rme_nec, [Spec], !Errors),
                    % Recover partially by ignoring the bad module
                    % qualification. The recovery is only partial because
                    % an end_module marker that matches the incorrect module
                    % name will get another error message about the
                    % `:- end_module' not matching the `:- module' declaration,
                    % which will be at least a bit misleading.
                    StartModuleName = qualified(CurModuleName, RawBaseName)
                )
            ),
            read_parse_tree_src_submodule(FileString, FileStringLen,
                ContainingModules, MaybePrevSection, CurModuleName,
                StartModuleName, StartContext,
                no_lookahead, SubModuleFinalLookAhead, !ModuleComponents,
                !SourceFileName, !SeqNumCounter, !Errors,
                !LineContext, !LinePosn),
            % We have read in one component; recurse to read in others.
            read_parse_tree_src_components(FileString, FileStringLen,
                CurModuleName, ContainingModules, MaybePrevSection,
                !.MissingStartSectionWarning,
                SubModuleFinalLookAhead, FinalLookAhead, !ModuleComponents,
                !SourceFileName, !SeqNumCounter, !Errors,
                !LineContext, !LinePosn)
        ;
            ( IOM = iom_marker_section(_, _, _)
            ; IOM = iom_marker_include(_)
            ; IOM = iom_marker_avail(_)
            ; IOM = iom_marker_fim(_)
            ; IOM = iom_item(_)
            ; IOM = iom_item_and_error_specs(_, _)
            ; IOM = iom_handled_no_error
            ; IOM = iom_handled_error(_)
            ),
            (
                IOM = iom_marker_section(SectionKind, SectionContext,
                    _SectionSeqNum),
                ItemSeqInitLookAhead = no_lookahead
            ;
                (
                    ( IOM = iom_marker_include(_)
                    ; IOM = iom_marker_avail(_)
                    ; IOM = iom_marker_fim(_)
                    ; IOM = iom_item(_)
                    ; IOM = iom_handled_no_error
                    )
                ;
                    IOM = iom_handled_error(ItemSpecs),
                    add_nonfatal_error(rme_nec, ItemSpecs, !Errors)
                ;
                    IOM = iom_item_and_error_specs(_, ItemSpecs),
                    add_nonfatal_error(rme_nec, ItemSpecs, !Errors)
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
                        _MissingStartSectionWarning, !Errors),
                    % The following code is duplicated in the case for
                    % read_iom_parse_item_errors above.
                    SectionKind = ms_implementation,
                    SectionContext = dummy_context
                ),
                ItemSeqInitLookAhead = lookahead(ReadIOMResult)
            ),
            % The following code is duplicated in the case for
            % read_iom_parse_item_errors above.
            read_item_sequence(FileString, FileStringLen, CurModuleName,
                ItemSeqInitLookAhead, ItemSeqFinalLookAhead,
                cord.init, InclsCord, cord.init, AvailsCord,
                cord.init, FIMsCord, cord.init, ItemsCord, !SourceFileName,
                !SeqNumCounter, !Errors, !LineContext, !LinePosn),
            add_section_component(CurModuleName, SectionKind, SectionContext,
                InclsCord, AvailsCord, FIMsCord, ItemsCord, !ModuleComponents),
            % We have read in one component; recurse to read in other
            % components.
            read_parse_tree_src_components(FileString, FileStringLen,
                CurModuleName, ContainingModules,
                yes(SectionKind - SectionContext),
                have_not_given_missing_section_start_warning,
                ItemSeqFinalLookAhead, FinalLookAhead, !ModuleComponents,
                !SourceFileName, !SeqNumCounter, !Errors,
                !LineContext, !LinePosn)
        ;
            IOM = iom_marker_module_end(EndedModuleName, EndContext,
                _EndSeqNum),
            handle_module_end_marker(CurModuleName, ContainingModules,
                ReadIOMResult, EndedModuleName, EndContext, FinalLookAhead,
                !Errors)
        )
    ).

:- pred add_section_component(module_name::in, module_section::in,
    prog_context::in, cord(item_include)::in, cord(item_avail)::in,
    cord(item_fim)::in, cord(item)::in,
    cord(module_component)::in, cord(module_component)::out) is det.

add_section_component(ModuleName, SectionKind, SectionContext,
        InclsCord, AvailsCord, FIMsCord, ItemsCord, !ModuleComponents) :-
    ( if
        cord.is_empty(InclsCord),
        cord.is_empty(AvailsCord),
        cord.is_empty(FIMsCord),
        cord.is_empty(ItemsCord)
    then
        true
    else
        Component = mc_section(ModuleName, SectionKind, SectionContext,
            InclsCord, AvailsCord, FIMsCord, ItemsCord),
        !:ModuleComponents = cord.snoc(!.ModuleComponents, Component)
    ).

:- pred generate_missing_start_section_warning_src(module_name::in,
    prog_context::in,
    missing_section_start_warning::in, missing_section_start_warning::out,
    read_module_errors::in, read_module_errors::out) is det.

generate_missing_start_section_warning_src(CurModuleName,
        Context, !MissingStartSectionWarning, !Errors) :-
    (
        !.MissingStartSectionWarning =
            have_not_given_missing_section_start_warning,
        !:MissingStartSectionWarning =
            have_given_missing_section_start_warning,
        Pieces = [invis_order_default_start(1, ""),
            words("Error: module"),
            qual_sym_name(CurModuleName), words("should start with"),
            words("either an"), decl("interface"), words("or an"),
            decl("implementation"), words("declaration."), nl,
            words("The following assumes that"),
            words("the missing declaration is an"),
            decl("implementation"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        add_nonfatal_error(rme_no_section_decl_at_start, [Spec], !Errors)
    ;
        !.MissingStartSectionWarning =
            have_given_missing_section_start_warning
        % Do not generate duplicate warnings.
    ).

:- pred read_parse_tree_src_submodule(string::in, int::in,
    list(module_name)::in, maybe(pair(module_section, prog_context))::in,
    module_name::in, module_name::in, prog_context::in,
    maybe_lookahead::in, maybe_lookahead::out,
    cord(module_component)::in, cord(module_component)::out,
    file_name::in, file_name::out, counter::in, counter::out,
    read_module_errors::in, read_module_errors::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_parse_tree_src_submodule(FileString, FileStringLen, ContainingModules,
        MaybePrevSection, ContainingModuleName, StartModuleName, StartContext,
        InitLookAhead, FinalLookAhead, !ModuleComponents, !SourceFileName,
        !SeqNumCounter, !Errors, !LineContext, !LinePosn) :-
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
        NoSectionSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, StartContext, NoSectionPieces),
        % XXX ITEM_LIST Should this be a situation-specific rme_X value?
        add_nonfatal_error(rme_no_section_decl_at_start, [NoSectionSpec],
            !Errors),
        SectionKind = ms_interface,
        SectionContext = dummy_context
    ),
    NestedContainingModules = [StartModuleName | ContainingModules],
    NestedMaybePrevSection = no,
    read_parse_tree_src_components(FileString, FileStringLen, StartModuleName,
        NestedContainingModules, NestedMaybePrevSection,
        have_not_given_missing_section_start_warning,
        InitLookAhead, FinalLookAhead, cord.init, NestedModuleComponents,
        !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn),
    SubModuleParseTreeSrc = parse_tree_src(StartModuleName, StartContext,
        NestedModuleComponents),
    Component = mc_nested_submodule(ContainingModuleName, SectionKind,
        SectionContext, SubModuleParseTreeSrc),
    !:ModuleComponents = cord.snoc(!.ModuleComponents, Component).

:- pred handle_module_end_marker(module_name::in, list(module_name)::in,
    read_iom_result::in, module_name::in, prog_context::in,
    maybe_lookahead::out, read_module_errors::in, read_module_errors::out)
    is det.

handle_module_end_marker(CurModuleName, ContainingModules, ReadIOMResult,
        EndedModuleName, EndContext, FinalLookAhead, !Errors) :-
    ( if CurModuleName = EndedModuleName then
        FinalLookAhead = no_lookahead
    else if partial_sym_name_matches_full(EndedModuleName, CurModuleName) then
        % XXX ITEM_LIST Should this be an error? Warning?
        FinalLookAhead = no_lookahead
    else if is_for_containing_module(EndedModuleName, ContainingModules) then
        Pieces = [words("Error: missing"), decl("end_module"),
            words("declaration for"), qual_sym_name(CurModuleName),
            suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            EndContext, Pieces),
        add_fatal_error(frme_bad_module_end, [Spec], !Errors),
        FinalLookAhead = lookahead(ReadIOMResult)
    else
        Pieces = [words("Error: this"), decl("end_module"),
            words("declaration for"), qual_sym_name(EndedModuleName),
            words("is not for the module at whose end it appears,"),
            words("which is"), qual_sym_name(CurModuleName), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            EndContext, Pieces),
        add_fatal_error(frme_bad_module_end, [Spec], !Errors),
        % Eat the bad end_module declaration.
        FinalLookAhead = no_lookahead
    ).

:- pred is_for_containing_module(module_name::in, list(module_name)::in)
    is semidet.

is_for_containing_module(EndedModuleName,
        [ContainingModule | ContainingModules]) :-
    ( if partial_sym_name_matches_full(EndedModuleName, ContainingModule) then
        true
    else
        is_for_containing_module(EndedModuleName, ContainingModules)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% This part of the module contains utility predicates.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_module_header
    --->    no_valid_module_header(
                % A complete description of the errors involved.
                read_module_errors
            )
    ;       valid_module_header(
                % The name in the ":- module" decl, and its context.
                module_name,
                prog_context
            ).

:- pred read_module_header(string::in, int::in,
    module_name::in, list(prog_context)::in, file_name::in,
    maybe_module_header::out, counter::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_module_header(FileString, FileStringLen,
        DefaultModuleName, DefaultExpectationContexts, SourceFileName,
        MaybeModuleHeader, !:SeqNumCounter,
        !LineContext, !LinePosn) :-
    counter.init(1, !:SeqNumCounter),
    read_first_module_decl(FileString, FileStringLen, require_module_decl,
        DefaultModuleName, DefaultExpectationContexts, ModuleDeclPresent,
        may_not_change_source_file_name, SourceFileName, _SourceFileName,
        !SeqNumCounter, !LineContext, !LinePosn),
    (
        ModuleDeclPresent = no_module_decl_present(_MaybeLookAhead, _Context,
            NoModuleSpec),
        Errors0 = init_read_module_errors,
        add_nonfatal_error(rme_no_module_decl_at_start,
            [NoModuleSpec], Errors0, Errors),
        MaybeModuleHeader = no_valid_module_header(Errors)
    ;
        ModuleDeclPresent = wrong_module_decl_present(ModuleName,
            ModuleNameContext, WrongModuleSpec),
        NoModuleSpec = report_wrong_module_start(ModuleNameContext,
            DefaultModuleName, ModuleName),
        Errors0 = init_read_module_errors,
        add_nonfatal_error(rme_no_module_decl_at_start,
            [NoModuleSpec], Errors0, Errors1),
        add_nonfatal_error(rme_unexpected_module_name,
            [WrongModuleSpec], Errors1, Errors),
        MaybeModuleHeader = no_valid_module_header(Errors)
    ;
        ModuleDeclPresent =
            right_module_decl_present(ModuleName, ModuleNameContext),
        MaybeModuleHeader = valid_module_header(ModuleName, ModuleNameContext)
    ).

:- type maybe_module_decl_present
    --->    no_module_decl_present(
                % Any lookahead left from the input. This lookahead is needed
                % ONLY to allow us to continue parsing .m files after
                % a missing initial ":- module" decl.
                maybe_lookahead,

                % The context of the term that is there instead of the module
                % declaration. Will contain term.context_init if there is
                % no such term.
                prog_context,

                % A message for the error. The error category is
                % always rme_no_module_decl_at_start.
                error_spec
            )
    ;       wrong_module_decl_present(
                % The name in the ":- module" decl, and its context.
                module_name,
                prog_context,

                % A message for the error, which may be conditional.
                % The category is always rme_unexpected_module_name.
                error_spec
            )
    ;       right_module_decl_present(
                % The name in the ":- module" decl, and its context.
                module_name,
                prog_context
            ).

    % Files written by users may contain source_file pragmas, which change
    % the parser's notion of the current filename. For these, callers should
    % therefore pass may_change_source_file_name. On the other hand, files
    % automatically generated by the compiler (.int* and .*opt files)
    % never contain source_file pragmas. For these, callers should pass
    % may_not_change_source_file_name, which calls for any occurrence of
    % a source_file pragma to be treated as the error it is.
    %
:- type may_change_source_file_name
    --->    may_not_change_source_file_name
    ;       may_change_source_file_name.

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
:- pred read_first_module_decl(string::in, int::in,
    maybe_require_module_decl::in, module_name::in, list(prog_context)::in,
    maybe_module_decl_present::out, may_change_source_file_name::in,
    file_name::in, file_name::out, counter::in, counter::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_first_module_decl(FileString, FileStringLen, RequireModuleDecl,
        DefaultModuleName, DefaultExpectationContexts,
        ModuleDeclPresent, MayChangeSourceFileName, !SourceFileName,
        !SeqNumCounter, !LineContext, !LinePosn) :-
    % Parse the first term, treating it as occurring within the scope
    % of the special "root" module (so that any `:- module' declaration
    % is taken to be a non-nested module unless explicitly qualified).
    mercury_term_parser.read_term_from_linestr(!.SourceFileName,
        FileString, FileStringLen, !LineContext, !LinePosn, FirstReadTerm),
    read_term_to_iom_result(DefaultModuleName, !.SourceFileName,
        FirstReadTerm, MaybeFirstIOM, !SeqNumCounter),
    (
        MaybeFirstIOM = read_iom_ok(_FirstVarSet, FirstTerm, FirstIOM),
        (
            FirstIOM = iom_marker_src_file(!:SourceFileName),
            (
                MayChangeSourceFileName = may_not_change_source_file_name,
                FirstLookAhead = lookahead(MaybeFirstIOM),
                FirstContext = get_term_context(FirstTerm),
                ModuleDeclPresent = no_module_decl_present(FirstLookAhead,
                    FirstContext, report_missing_module_start(FirstContext))
            ;
                MayChangeSourceFileName = may_change_source_file_name,
                % Apply and then skip `pragma source_file' decls, by calling
                % ourselves recursively with the new source file name.
                read_first_module_decl(FileString, FileStringLen,
                    RequireModuleDecl, DefaultModuleName,
                    DefaultExpectationContexts, ModuleDeclPresent,
                    MayChangeSourceFileName, !SourceFileName,
                    !SeqNumCounter, !LineContext, !LinePosn)
            )
        ;
            FirstIOM = iom_marker_module_start(StartModuleName,
                ModuleNameContext, _ModuleNameSeqNum),
            % The first term is a `:- module' decl, as expected.
            % Check whether it matches the expected module name.
            % If it doesn't, report a (conditionally enabled) error.
            ( if DefaultModuleName = StartModuleName then
                ModuleDeclPresent = right_module_decl_present(StartModuleName,
                    ModuleNameContext)
            else
                report_module_has_unexpected_name(!.SourceFileName,
                    DefaultModuleName, DefaultExpectationContexts,
                    StartModuleName, yes(ModuleNameContext), NameSpec),
                ModuleDeclPresent = wrong_module_decl_present(StartModuleName,
                    ModuleNameContext, NameSpec)
            )
        ;
            ( FirstIOM = iom_marker_module_end(_, _, _)
            ; FirstIOM = iom_marker_version_numbers(_)
            ; FirstIOM = iom_marker_section(_, _, _)
            ; FirstIOM = iom_marker_include(_)
            ; FirstIOM = iom_marker_avail(_)
            ; FirstIOM = iom_marker_fim(_)
            ; FirstIOM = iom_item(_)
            % Ignore the error_specs.
            ; FirstIOM = iom_item_and_error_specs(_, _)
            ; FirstIOM = iom_handled_no_error
            % Ignore the error_specs.
            ; FirstIOM = iom_handled_error(_)
            ),
            FirstLookAhead = lookahead(MaybeFirstIOM),
            FirstContext = get_term_context(FirstTerm),
            ModuleDeclPresent = no_module_decl_present(FirstLookAhead,
                get_term_context(FirstTerm),
                report_missing_module_start(FirstContext))
        )
    ;
        MaybeFirstIOM = read_iom_parse_item_errors(_, FirstTerm, _),
        LookAhead = lookahead(MaybeFirstIOM),
        FirstContext = get_term_context(FirstTerm),
        ModuleDeclPresent = no_module_decl_present(LookAhead,
            get_term_context(FirstTerm),
            report_missing_module_start(FirstContext))
    ;
        ( MaybeFirstIOM = read_iom_eof
        ; MaybeFirstIOM = read_iom_parse_term_error(_)
        ),
        FirstContext = term_context.context_init(!.SourceFileName, 1),
        ModuleDeclPresent = no_module_decl_present(no_lookahead,
            dummy_context, report_missing_module_start(FirstContext))
        % XXX ITEM_LIST Should report "stop processing".
    ).

%---------------------------------------------------------------------------%

    % Read a sequence of items, until we find a marker that indicates
    % a change in section or module. If and when we find such a marker,
    % we stop reading, and return the term of that marker as the final
    % lookahead.
    %
    % We use the standard two level loop to avoid running out of stack
    % on long item sequences in grades that do not allow tail recursion.
    %
:- pred read_item_sequence(string::in, int::in, module_name::in,
    maybe_lookahead::in, maybe_lookahead::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_fim)::in, cord(item_fim)::out,
    cord(item)::in, cord(item)::out,
    file_name::in, file_name::out, counter::in, counter::out,
    read_module_errors::in, read_module_errors::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_item_sequence(FileString, FileStringLen, ModuleName,
        InitLookAhead, FinalLookAhead,
        !InclsCord, !AvailsCord, !FIMsCord, !ItemsCord,
        !SourceFileName, !SeqNumCounter, !Errors,
        !LineContext, !LinePosn) :-
    get_next_item_or_marker(!.SourceFileName, FileString, FileStringLen,
        InitLookAhead, ModuleName, ReadIOMResult,
        !SeqNumCounter, !LineContext, !LinePosn),
    read_item_sequence_inner(FileString, FileStringLen, ModuleName,
        1024, NumItemsLeft, ReadIOMResult, MidLookAhead,
        !InclsCord, !AvailsCord, !FIMsCord, !ItemsCord,
        !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn),
    ( if NumItemsLeft = 0 then
        read_item_sequence(FileString, FileStringLen, ModuleName,
            MidLookAhead, FinalLookAhead,
            !InclsCord, !AvailsCord, !FIMsCord, !ItemsCord,
            !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn)
    else
        FinalLookAhead = MidLookAhead
    ).

:- pred read_item_sequence_inner(string::in, int::in,
    module_name::in, int::in, int::out,
    read_iom_result::in, maybe_lookahead::out,
    cord(item_include)::in, cord(item_include)::out,
    cord(item_avail)::in, cord(item_avail)::out,
    cord(item_fim)::in, cord(item_fim)::out,
    cord(item)::in, cord(item)::out, file_name::in, file_name::out,
    counter::in, counter::out, read_module_errors::in, read_module_errors::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_item_sequence_inner(FileString, FileStringLen, ModuleName,
        !NumItemsLeft, ReadIOMResult, FinalLookAhead,
        !InclsCord, !AvailsCord, !FIMsCord, !ItemsCord,
        !SourceFileName, !SeqNumCounter, !Errors, !LineContext, !LinePosn) :-
    ( if !.NumItemsLeft =< 0 then
        FinalLookAhead = lookahead(ReadIOMResult)
    else
        (
            ReadIOMResult = read_iom_eof,
            FinalLookAhead = no_lookahead
        ;
            (
                ReadIOMResult = read_iom_parse_term_error(ItemSpec),
                add_nonfatal_error(rme_could_not_read_term, [ItemSpec],
                    !Errors)
            ;
                ReadIOMResult = read_iom_parse_item_errors(_, _, ItemSpecs),
                add_nonfatal_error(rme_could_not_parse_item, ItemSpecs,
                    !Errors)
            ),
            read_next_item_or_marker(!.SourceFileName,
                FileString, FileStringLen, ModuleName, NextReadIOMResult,
                !SeqNumCounter, !LineContext, !LinePosn),
            read_item_sequence_inner(FileString, FileStringLen, ModuleName,
                !NumItemsLeft, NextReadIOMResult, FinalLookAhead,
                !InclsCord, !AvailsCord, !FIMsCord, !ItemsCord,
                !SourceFileName, !SeqNumCounter, !Errors,
                !LineContext, !LinePosn)
        ;
            ReadIOMResult = read_iom_ok(_IOMVarSet, IOMTerm, IOM),
            !:NumItemsLeft = !.NumItemsLeft - 1,
            (
                ( IOM = iom_marker_module_start(_, _, _)
                ; IOM = iom_marker_module_end(_, _, _)
                ; IOM = iom_marker_section(_, _, _)
                ),
                FinalLookAhead = lookahead(ReadIOMResult)
            ;
                (
                    IOM = iom_marker_src_file(!:SourceFileName)
                ;
                    IOM = iom_marker_version_numbers(_),
                    Pieces = [words("Error: version number records"),
                        words("should not appear anywhere except in"),
                        words("automatically generated interface files."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_read_files, get_term_context(IOMTerm), Pieces),
                    add_nonfatal_error(rme_nec, [Spec], !Errors)
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
                    IOM = iom_marker_fim(FIM),
                    !:FIMsCord = cord.snoc(!.FIMsCord, FIM)
                ;
                    IOM = iom_item(Item),
                    !:ItemsCord = cord.snoc(!.ItemsCord, Item)
                ;
                    IOM = iom_item_and_error_specs(Item, ItemSpecs),
                    !:ItemsCord = cord.snoc(!.ItemsCord, Item),
                    add_nonfatal_error(rme_nec, ItemSpecs, !Errors)
                ;
                    IOM = iom_handled_no_error
                ;
                    IOM = iom_handled_error(HandledSpecs),
                    add_nonfatal_error(rme_nec, HandledSpecs, !Errors)
                ),
                read_next_item_or_marker(!.SourceFileName,
                    FileString, FileStringLen, ModuleName, NextReadIOMResult,
                    !SeqNumCounter, !LineContext, !LinePosn),
                read_item_sequence_inner(FileString, FileStringLen, ModuleName,
                    !NumItemsLeft, NextReadIOMResult, FinalLookAhead,
                    !InclsCord, !AvailsCord, !FIMsCord, !ItemsCord,
                    !SourceFileName, !SeqNumCounter, !Errors,
                    !LineContext, !LinePosn)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- type maybe_lookahead
    --->    no_lookahead
    ;       lookahead(read_iom_result).

:- pred get_next_item_or_marker(file_name::in, string::in, int::in,
    maybe_lookahead::in, module_name::in, read_iom_result::out,
    counter::in, counter::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

get_next_item_or_marker(FileName, FileString, FileStringLen, InitLookAhead,
        ModuleName, ReadIOMResult, !SeqNumCounter, !LineContext, !LinePosn) :-
    (
        InitLookAhead = no_lookahead,
        read_next_item_or_marker(FileName, FileString, FileStringLen,
            ModuleName, ReadIOMResult, !SeqNumCounter,
            !LineContext, !LinePosn)
    ;
        InitLookAhead = lookahead(ReadIOMResult)
    ).

:- pred read_next_item_or_marker(file_name::in, string::in, int::in,
    module_name::in, read_iom_result::out, counter::in, counter::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

read_next_item_or_marker(FileName, FileString, FileStringLen, ModuleName,
        ReadIOMResult, !SeqNumCounter, !LineContext, !LinePosn) :-
    mercury_term_parser.read_term_from_linestr(FileName,
        FileString, FileStringLen, !LineContext, !LinePosn, ReadTermResult),
    read_term_to_iom_result(ModuleName, FileName, ReadTermResult,
        ReadIOMResult, !SeqNumCounter).

%---------------------------------------------------------------------------%

:- type read_iom_result
    --->    read_iom_eof
            % We have reached end-of-file.
    ;       read_iom_parse_term_error(error_spec)
            % The call to mercury_term_parser.read_term_from_linestr
            % has failed, which means that what we found in the file string
            % is not a valid term.
    ;       read_iom_parse_item_errors(varset, term, list(error_spec))
            % We have successfully read a term from the file string,
            % but could not parse it as an item or marker.
            % The error category is implicitly rme_could_not_parse_item.
    ;       read_iom_ok(varset, term, item_or_marker).
            % We have successfully read a term from the file string,
            % and parsed it as an item or marker.

:- pred read_term_to_iom_result(module_name::in, string::in, read_term::in,
    read_iom_result::out, counter::in, counter::out) is det.

read_term_to_iom_result(ModuleName, FileName, ReadTermResult, ReadIOMResult,
        !SeqNumCounter) :-
    % XXX ITEM_LIST Should add a prefix to eof, error, and term
    % in library/term_io.m?
    (
        ReadTermResult = eof,
        ReadIOMResult = read_iom_eof
    ;
        ReadTermResult = error(ErrorMsg, LineNumber),
        Context = term_context.context_init(FileName, LineNumber),
        % XXX Do we need to add an "Error:" prefix?
        Pieces = [words(ErrorMsg), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        ReadIOMResult = read_iom_parse_term_error(Spec)
    ;
        ReadTermResult = term(VarSet, Term),
        counter.allocate(SeqNum, !SeqNumCounter),
        parse_item_or_marker(ModuleName, VarSet, Term, item_seq_num(SeqNum),
            MaybeItemOrMarker),
        (
            MaybeItemOrMarker = ok1(ItemOrMarker),
            ReadIOMResult = read_iom_ok(VarSet, Term, ItemOrMarker)
        ;
            MaybeItemOrMarker = error1(Specs),
            ReadIOMResult = read_iom_parse_item_errors(VarSet, Term, Specs)
        )
    ).

%---------------------------------------------------------------------------%

:- func report_missing_module_start(prog_context) = error_spec.

report_missing_module_start(FirstContext) = Spec :-
    Pieces = [invis_order_default_start(0, ""),
        words("Error: module must start with a"),
        decl("module"), words("declaration."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        FirstContext, Pieces).

:- func report_wrong_module_start(prog_context, module_name, module_name)
    = error_spec.

report_wrong_module_start(FirstContext, Expected, Actual) = Spec :-
    Pieces = [words("Error: module starts with the wrong"),
        decl("module"), words("declaration."), nl,
        words("Expected module"), qual_sym_name(Expected), suffix(","),
        words("found module"), qual_sym_name(Actual), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        FirstContext, Pieces).

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
:- pred check_for_unexpected_item_at_end(file_name::in, string::in, int::in,
    module_name::in, file_kind::in,
    maybe_lookahead::in, counter::in,
    read_module_errors::in, read_module_errors::out,
    line_context::in, line_posn::in) is det.

check_for_unexpected_item_at_end(SourceFileName, FileString, FileStringLen,
        ModuleName, FileKind, FinalLookAhead, SeqNumCounter0,
        !Errors, !.LineContext, !.LinePosn) :-
    get_next_item_or_marker(SourceFileName, FileString, FileStringLen,
        FinalLookAhead, ModuleName, IOMResult,
        SeqNumCounter0, _SeqNumCounter, !.LineContext, _, !.LinePosn, _),
    (
        IOMResult = read_iom_eof
    ;
        IOMResult = read_iom_parse_term_error(ItemSpec),
        add_nonfatal_error(rme_could_not_read_term, [ItemSpec], !Errors)
    ;
        IOMResult = read_iom_parse_item_errors(_VarSet, Term, ItemSpecs),
        add_nonfatal_error(rme_could_not_parse_item, ItemSpecs, !Errors),
        report_unexpected_term_at_end(FileKind, Term, !Errors)
    ;
        IOMResult = read_iom_ok(_IOMVarSet, IOMTerm, _IOM),
        report_unexpected_term_at_end(FileKind, IOMTerm, !Errors)
    ).

:- pred report_unexpected_term_at_end(file_kind::in, term::in,
    read_module_errors::in, read_module_errors::out) is det.

report_unexpected_term_at_end(FileKind, Term, !Errors) :-
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
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        Context, Pieces),
    add_nonfatal_error(Error, [Spec], !Errors).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_module.
%---------------------------------------------------------------------------%
