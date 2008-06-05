%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: analysis.file.m
% Main author: stayl, wangp.
%
% This module deals with the on-disk representations of the analysis registry
% and associated files.
%
%-----------------------------------------------------------------------------%

:- module analysis.file.
:- interface.

    % read_module_overall_status(Compiler, ModuleName, MaybeModuleStatus, !IO)
    %
    % Read the overall status of a module from its `.analysis_status' file.
    % If the module has outstanding requests, then an overall status of
    % `optimal' is downgraded to `suboptimal'.
    %
:- pred read_module_overall_status(Compiler::in, module_name::in,
    analysis_status::out, io::di, io::uo) is det <= compiler(Compiler).

    % write_module_overall_status(AnalysisInfo, ModuleName, ModuleStatus, !IO)
    %
    % Write the status of a module to its `.analysis_status' file.
    %
:- pred write_module_overall_status(analysis_info::in, module_name::in,
    analysis_status::in, io::di, io::uo) is det.

    % read_module_analysis_results(AnalysisInfo, ModuleName, AnalysisResults,
    %   !IO)
    %
    % Read the analysis results from a `.analysis' file.
    %
:- pred read_module_analysis_results(analysis_info::in, module_name::in,
    module_analysis_map(some_analysis_result)::out, io::di, io::uo) is det.

    % write_module_analysis_results(AnalysisInfo, ModuleName, AnalysisResults,
    %   !IO)
    %
    % Write the analysis results for a module to its `.analysis' file.
    %
:- pred write_module_analysis_results(analysis_info::in,
    module_name::in, module_analysis_map(some_analysis_result)::in,
    io::di, io::uo) is det.

    % read_module_analysis_requests(AnalysisInfo, ModuleName, ModuleRequests,
    %   !IO)
    %
    % Read outstanding analysis requests to a module from disk.
    %
:- pred read_module_analysis_requests(analysis_info::in,
    module_name::in, module_analysis_map(analysis_request)::out,
    io::di, io::uo) is det.

    % write_module_analysis_requests(AnalysisInfo, ModuleName, ModuleRequests,
    %   !IO)
    %
    % Write outstanding analysis requests for a module to disk.
    %
:- pred write_module_analysis_requests(analysis_info::in,
    module_name::in, module_analysis_map(analysis_request)::in,
    io::di, io::uo) is det.

    % read_module_imdg(AnalysisInfo, ModuleName, ModuleEntries, !IO)
    %
    % Read the intermodule dependencies graph entries for a module from disk.
    %
:- pred read_module_imdg(analysis_info::in, module_name::in,
    module_analysis_map(imdg_arc)::out, io::di, io::uo) is det.

    % write_module_imdg(AnalysisInfo, ModuleName, ModuleEntries, !IO)
    %
    % Write the intermodule dependencies graph entries for a module
    % to disk.
    %
:- pred write_module_imdg(analysis_info::in, module_name::in,
    module_analysis_map(imdg_arc)::in, io::di, io::uo) is det.

    % empty_request_file(AnalysisInfo, ModuleName, !IO)
    %
    % Delete the file containing outstanding analysis requests for a module.
    % This means all the analysis requests should have been satisfied already.
    %
:- pred empty_request_file(analysis_info::in, module_name::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.            % XXX unwanted dependency
:- import_module parse_tree.modules.    % XXX unwanted dependency

:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module parser.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

% The format of an analysis result file is:
%
% version_number.
% analysis_name(analysis_version, func_id, call_pattern, answer_pattern,
%   result_status).

% An .analysis_status file contains a single line, which is one of:
%
% optimal.  
% suboptimal.
% invalid.
%
% A missing file is equivalent to `optimal'.

% The format of an IMDG file is:
%
% version_number.
% calling_module -> analysis_name(analysis_version, func_id, call_pattern).

% The format of an analysis request file is:
%
% version_number.
% calling_module -> analysis_name(analysis_version, func_id, call_pattern).

:- type invalid_analysis_file
    --->    invalid_analysis_file(string).

:- func version_number = int.

version_number = 5.

:- func analysis_registry_suffix = string.

analysis_registry_suffix = ".analysis".

:- func analysis_registry_status_suffix = string.

analysis_registry_status_suffix = ".analysis_status".

:- func imdg_suffix = string.

imdg_suffix = ".imdg".

:- func request_suffix = string.

request_suffix = ".request".

%-----------------------------------------------------------------------------%

read_module_overall_status(Compiler, ModuleName, ModuleStatus, !IO) :-
    module_name_to_read_file_name(Compiler, ModuleName,
        analysis_registry_status_suffix, MaybeFileName, !IO),
    (
        MaybeFileName = ok(FileName),
        read_module_overall_status_2(FileName, ModuleStatus0, !IO)
    ;
        MaybeFileName = error(_),
        % Missing file means optimal.  We don't install `.analysis_status'
        % files when installing libraries, for example.
        ModuleStatus0 = optimal
    ),
    (
        ModuleStatus0 = optimal,
        module_name_to_read_file_name(Compiler, ModuleName, request_suffix,
            MaybeRequestFileName, !IO),
        (
            % There are outstanding requests for this module.
            MaybeRequestFileName = ok(_),
            ModuleStatus = suboptimal
        ;
            MaybeRequestFileName = error(_),
            ModuleStatus = ModuleStatus0
        )
    ;
        ( ModuleStatus0 = suboptimal
        ; ModuleStatus0 = invalid
        ),
        ModuleStatus = ModuleStatus0
    ).

:- pred read_module_overall_status_2(string::in, analysis_status::out,
    io::di, io::uo) is det.

read_module_overall_status_2(FileName, ModuleStatus, !IO) :-
    io.open_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read_line_as_string(Stream, ReadResult, !IO),
        io.close_input(Stream, !IO),
        (
            ReadResult = ok(String),
            ( string.prefix(String, "optimal.") ->
                ModuleStatus = optimal
            ; string.prefix(String, "suboptimal.") ->
                ModuleStatus = suboptimal
            ; string.prefix(String, "invalid.") ->
                ModuleStatus = invalid
            ;
                unexpected(this_file,
                    "read_module_overall_status_2: unexpected line")
            )
        ;
            ReadResult = eof,
            unexpected(this_file,
                "read_module_overall_status_2: unexpected eof")
        ;
            ReadResult = error(IOError),
            unexpected(this_file,
                "read_module_overall_status_2: " ++ io.error_message(IOError))
        )
    ;
        OpenResult = error(IOError),
        unexpected(this_file,
            "read_module_overall_status_2: " ++ io.error_message(IOError))
    ).

write_module_overall_status(Info, ModuleName, Status, !IO) :-
    module_name_to_write_file_name(Info ^ compiler, ModuleName,
        analysis_registry_status_suffix, FileName, !IO),
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        (
            Status = optimal,
            io.write_string(Stream, "optimal.\n", !IO)
        ;
            Status = suboptimal,
            io.write_string(Stream, "suboptimal.\n", !IO)
        ;
            Status = invalid,
            io.write_string(Stream, "invalid.\n", !IO)
        ),
        io.close_output(Stream, !IO)
    ;
        OpenResult = error(IOError),
        unexpected(this_file,
            "write_module_overall_status: " ++ io.error_message(IOError))
    ).

%-----------------------------------------------------------------------------%

read_module_analysis_results(Info, ModuleName, ModuleResults, !IO) :-
    % If the module's overall status is `invalid' then at least one of its
    % results is invalid.  However, we can't just discard the results as we
    % want to know which results change after we reanalyse the module.
    Compiler = Info ^ compiler,
    module_name_to_read_file_name(Compiler, ModuleName,
        analysis_registry_suffix, MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),
        read_module_analysis_results_2(Compiler, AnalysisFileName,
            ModuleResults, !IO)
    ;
        MaybeAnalysisFileName = error(_),
        ModuleResults = map.init
    ).

:- pred read_module_analysis_results_2(Compiler::in, string::in,
    module_analysis_map(some_analysis_result)::out, io::di, io::uo) is det
    <= compiler(Compiler).

read_module_analysis_results_2(Compiler, AnalysisFileName, ModuleResults,
        !IO) :-
    ModuleResults0 = map.init,
    io.open_input(AnalysisFileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Reading analysis registry file ", !IO),
            io.write_string(AnalysisFileName, !IO),
            io.nl(!IO)
        ), !IO),
        io.set_input_stream(Stream, OldStream, !IO),

        check_analysis_file_version_number(!IO),
        promise_only_solution_io(
            (pred(Results2::out, !.IO::di, !:IO::uo) is cc_multi :-
                try_io((pred(Results1::out, !.IO::di, !:IO::uo) is det :-
                    read_analysis_file_2(parse_result_entry(Compiler),
                        ModuleResults0, Results1, !IO)
                ), Results2, !IO)
            ), Results, !IO),
        (
            Results = succeeded(ModuleResults)
        ;
            Results = failed,
            ModuleResults = ModuleResults0
        ;
            Results = exception(_),
            % XXX Report error.
            ModuleResults = ModuleResults0
        ),
        io.set_input_stream(OldStream, _, !IO),
        io.close_input(Stream, !IO)
    ;
        OpenResult = error(_),
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Error reading analysis registry file: ", !IO),
            io.write_string(AnalysisFileName, !IO),
            io.nl(!IO)
        ), !IO),
        ModuleResults = ModuleResults0
    ).

:- pred analysis_status_to_string(analysis_status, string).
:- mode analysis_status_to_string(in, out) is det.
:- mode analysis_status_to_string(out, in) is semidet.

analysis_status_to_string(invalid, "invalid").
analysis_status_to_string(suboptimal, "suboptimal").
analysis_status_to_string(optimal, "optimal").

:- pred parse_result_entry(Compiler::in)
    `with_type` parse_entry(module_analysis_map(some_analysis_result))
    `with_inst` parse_entry <= compiler(Compiler).

parse_result_entry(Compiler, Term, Results0, Results) :-
    (
        Term = term.functor(term.atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm,
            CallPatternTerm, AnswerPatternTerm, StatusTerm], _),
        term_to_type(FuncIdTerm, FuncId),
        CallPatternTerm = term.functor(
            term.string(CallPatternString), [], _),
        AnswerPatternTerm = term.functor(
            term.string(AnswerPatternString), [], _),
        StatusTerm = term.functor(term.string(StatusString), [], _),
        analysis_type(_ : unit(Call), _ : unit(Answer)) =
            analyses(Compiler, AnalysisName),

        CallPattern = from_string(CallPatternString) : Call,
        AnswerPattern = from_string(AnswerPatternString) : Answer,
        analysis_status_to_string(Status, StatusString)
    ->
        (
            VersionNumber = analysis_version_number(_ : Call, _ : Answer),
            VersionNumberTerm = term.functor(
                term.integer(VersionNumber), [], _)
        ->
            Result = 'new some_analysis_result'(CallPattern, AnswerPattern,
                Status),
            ( AnalysisResults0 = map.search(Results0, AnalysisName) ->
                AnalysisResults1 = AnalysisResults0
            ;
                AnalysisResults1 = map.init
            ),
            ( map.search(AnalysisResults1, FuncId, FuncResults0) ->
                FuncResults = [Result | FuncResults0]
            ;
                FuncResults = [Result]
            ),
            map.set(AnalysisResults1, FuncId, FuncResults, AnalysisResults),
            map.set(Results0, AnalysisName, AnalysisResults, Results)
        ;
            % Ignore results with an out-of-date version number.
            Results = Results0
        )
    ;
        Msg = "failed to parse result entry: " ++ string(Term),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%

read_module_analysis_requests(Info, ModuleName, ModuleRequests, !IO) :-
    read_analysis_file(Info ^ compiler, ModuleName, request_suffix,
        parse_request_entry(Info ^ compiler),
        map.init, ModuleRequests, !IO).

:- pred parse_request_entry(Compiler::in)
    `with_type` parse_entry(module_analysis_map(analysis_request))
    `with_inst` parse_entry <= compiler(Compiler).

parse_request_entry(Compiler, Term, Requests0, Requests) :-
    (
        Term = term.functor(atom("->"), [CallerModuleTerm, RHS], _),
        RHS = term.functor(atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
        term_to_type(CallerModuleTerm, CallerModule),
        term_to_type(FuncIdTerm, FuncId),
        CallPatternTerm = term.functor(
            term.string(CallPatternString), [], _),
        analysis_type(_ : unit(Call), _ : unit(Answer)) =
            analyses(Compiler, AnalysisName),
        CallPattern = from_string(CallPatternString) : Call
    ->
        (
            VersionNumber = analysis_version_number(_ : Call, _ : Answer),
            VersionNumberTerm = term.functor(
                term.integer(VersionNumber), [], _)
        ->
            Result = 'new analysis_request'(CallPattern, CallerModule),
            ( AnalysisRequests0 = map.search(Requests0, AnalysisName) ->
                AnalysisRequests1 = AnalysisRequests0
            ;
                AnalysisRequests1 = map.init
            ),
            ( map.search(AnalysisRequests1, FuncId, FuncRequests0) ->
                FuncRequests = [Result | FuncRequests0]
            ;
                FuncRequests = [Result]
            ),
            map.set(AnalysisRequests1, FuncId, FuncRequests, AnalysisRequests),
            map.set(Requests0, AnalysisName, AnalysisRequests, Requests)
        ;
            % Ignore requests with an out-of-date version number.
            Requests = Requests0
        )
    ;
        Msg = "failed to parse request entry: " ++ string(Term),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%

read_module_imdg(Info, ModuleName, ModuleEntries, !IO) :-
    read_analysis_file(Info ^ compiler, ModuleName, imdg_suffix,
        parse_imdg_arc(Info ^ compiler),
        map.init, ModuleEntries, !IO).

:- pred parse_imdg_arc(Compiler::in)
    `with_type` parse_entry(module_analysis_map(imdg_arc))
    `with_inst` parse_entry <= compiler(Compiler).

parse_imdg_arc(Compiler, Term, Arcs0, Arcs) :-
    (
        Term = term.functor(atom("->"), [DependentModuleTerm, ResultTerm], _),
        term_to_type(DependentModuleTerm, DependentModule),
        ResultTerm = functor(atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
        term_to_type(FuncIdTerm, FuncId),
        CallPatternTerm = functor(string(CallPatternString), [], _),
        analysis_type(_ : unit(Call), _ : unit(Answer))
            = analyses(Compiler, AnalysisName),
        CallPattern = from_string(CallPatternString) : Call
    ->
        (
            VersionNumber = analysis_version_number(_ : Call, _ : Answer),
            VersionNumberTerm = term.functor(
                term.integer(VersionNumber), [], _)
        ->
            Arc = 'new imdg_arc'(CallPattern, DependentModule),
            ( AnalysisArcs0 = map.search(Arcs0, AnalysisName) ->
                AnalysisArcs1 = AnalysisArcs0
            ;
                AnalysisArcs1 = map.init
            ),
            ( map.search(AnalysisArcs1, FuncId, FuncArcs0) ->
                FuncArcs = [Arc | FuncArcs0]
            ;
                FuncArcs = [Arc]
            ),
            map.set(AnalysisArcs1, FuncId, FuncArcs, AnalysisArcs),
            map.set(Arcs0, AnalysisName, AnalysisArcs, Arcs)
        ;
            % Ignore results with an out-of-date version number.
            % XXX: is that the right thing to do?
            % do we really need a version number for the IMDG?
            Arcs = Arcs0
        )
    ;
        Msg = "failed to parse IMDG arc: " ++ string(Term),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%

:- type parse_entry(T) == pred(term, T, T).
:- inst parse_entry == (pred(in, in, out) is det).

:- pred read_analysis_file(Compiler::in, module_name::in, string::in,
    parse_entry(T)::in(parse_entry), T::in, T::out, io::di, io::uo) is det
    <= compiler(Compiler).

read_analysis_file(Compiler, ModuleName, Suffix, ParseEntry,
        ModuleResults0, ModuleResults, !IO) :-
    module_name_to_read_file_name(Compiler, ModuleName, Suffix,
        MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),
        read_analysis_file(AnalysisFileName, ParseEntry,
            ModuleResults0, ModuleResults, !IO)
    ;
        MaybeAnalysisFileName = error(Message),
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Couldn't open ", !IO),
            io.write_string(Suffix, !IO),
            io.write_string(" for module ", !IO),
            io.write(ModuleName, !IO),
            io.write_string(": ", !IO),
            io.write_string(Message, !IO),
            io.nl(!IO)
        ), !IO),
        ModuleResults = ModuleResults0
    ).

:- pred read_analysis_file(string::in, parse_entry(T)::in(parse_entry),
    T::in, T::out, io::di, io::uo) is det.

read_analysis_file(AnalysisFileName, ParseEntry, ModuleResults0, ModuleResults,
        !IO) :-
    io.open_input(AnalysisFileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Reading analysis file ", !IO),
            io.write_string(AnalysisFileName, !IO),
            io.nl(!IO)
        ), !IO),
        io.set_input_stream(Stream, OldStream, !IO),

        promise_only_solution_io(
            (pred(R::out, di, uo) is cc_multi -->
                try_io((pred(Results1::out, di, uo) is det -->
                    check_analysis_file_version_number,
                    read_analysis_file_2(ParseEntry, ModuleResults0, Results1)
                ), R)
            ), Result, !IO),
        (
            Result = succeeded(ModuleResults)
        ;
            Result = failed,
            ModuleResults = ModuleResults0
        ;
            Result = exception(_),
            % XXX Report error.
            ModuleResults = ModuleResults0
        ),
        io.set_input_stream(OldStream, _, !IO),
        io.close_input(Stream, !IO)
    ;
        OpenResult = error(_),
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Error reading analysis file: ", !IO),
            io.write_string(AnalysisFileName, !IO),
            io.nl(!IO)
        ), !IO),
        ModuleResults = ModuleResults0
    ).

:- pred check_analysis_file_version_number(io::di, io::uo) is det.

check_analysis_file_version_number(!IO) :-
    parser.read_term(TermResult : read_term, !IO),
    (
        TermResult = term(_, term.functor(term.integer(version_number), [], _))
    ->
        true
    ;
        Msg = "bad analysis file version: " ++ string(TermResult),
        throw(invalid_analysis_file(Msg))
    ).

:- pred read_analysis_file_2(parse_entry(T)::in(parse_entry), T::in, T::out,
    io::di, io::uo) is det.

read_analysis_file_2(ParseEntry, Results0, Results, !IO) :-
    parser.read_term(TermResult, !IO),
    (
        TermResult = term(_, Term) : read_term,
        ParseEntry(Term, Results0, Results1),
        read_analysis_file_2(ParseEntry, Results1, Results, !IO)
    ;
        TermResult = eof,
        Results = Results0
    ;
        TermResult = error(Msg, _),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

write_module_analysis_results(Info, ModuleName, ModuleResults, !IO) :-
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
        io.write_string("% Writing module analysis results for ", !IO),
        io.write(ModuleName, !IO),
        io.nl(!IO)
    ), !IO),
    ToTmp = yes,
    write_analysis_file(Info ^ compiler, ModuleName, analysis_registry_suffix,
        ToTmp, write_result_entry, ModuleResults, FileName, !IO),
    update_interface(FileName, !IO).

:- pred write_result_entry
    `with_type` write_entry(some_analysis_result)
    `with_inst` write_entry.

write_result_entry(AnalysisName, FuncId, Result, !IO) :-
    Result = some_analysis_result(Call, Answer, Status),
    VersionNumber = analysis_version_number(Call, Answer),
    analysis_status_to_string(Status, StatusString),
    term_io.write_term_nl(varset.init : varset,
        functor(atom(AnalysisName), [
            functor(integer(VersionNumber), [], context_init),
            type_to_term(FuncId),
            functor(string(to_string(Call)), [], context_init),
            functor(string(to_string(Answer)), [], context_init),
            functor(string(StatusString), [], context_init)
        ], context_init), !IO).

%-----------------------------------------------------------------------------%

write_module_analysis_requests(Info, ModuleName, ModuleRequests, !IO) :-
    Compiler = Info ^ compiler,
    module_name_to_write_file_name(Compiler, ModuleName, request_suffix,
        AnalysisFileName, !IO),
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
        io.write_string("% Writing module analysis requests to ", !IO),
        io.write_string(AnalysisFileName, !IO),
        io.nl(!IO)
    ), !IO),
    io.open_input(AnalysisFileName, InputResult, !IO),
    (
        InputResult = ok(InputStream),
        % Request file already exists.  Check it has the right version
        % number, then append the new requests to the end.

        io.set_input_stream(InputStream, OldInputStream, !IO),
        parser.read_term(VersionResult : read_term, !IO),
        io.set_input_stream(OldInputStream, _, !IO),
        io.close_input(InputStream, !IO),
        (
            VersionResult = term(_, term.functor(
                term.integer(version_number), [], _))
        ->
            io.open_append(AnalysisFileName, AppendResult, !IO),
            (
                AppendResult = ok(AppendStream),
                io.set_output_stream(AppendStream, OldOutputStream, !IO),
                write_analysis_file_2(write_request_entry(Compiler),
                    ModuleRequests, !IO),
                io.set_output_stream(OldOutputStream, _, !IO),
                io.close_output(AppendStream, !IO),
                Appended = yes
            ;
                AppendResult = error(_),
                Appended = no
            )
        ;
            Appended = no
        )
    ;
        InputResult = error(_),
        Appended = no
    ),
    (
        Appended = no,
        write_analysis_file(AnalysisFileName, write_request_entry(Compiler),
            ModuleRequests, !IO)
    ;
        Appended = yes
    ).

:- pred write_request_entry(Compiler::in)
    `with_type` write_entry(analysis_request)
    `with_inst` write_entry <= compiler(Compiler).

write_request_entry(Compiler, AnalysisName, FuncId, Request, !IO) :-
    Request = analysis_request(Call, CallerModule),
    (
        analysis_type(_ : unit(Call), _ : unit(Answer)) =
            analyses(Compiler, AnalysisName)
    ->
        VersionNumber = analysis_version_number(_ : Call, _ :  Answer)
    ;
        unexpected(this_file,
            "write_request_entry: unknown analysis type")
    ),
    term_io.write_term_nl(varset.init : varset,
        functor(atom("->"), [
            type_to_term(CallerModule),
            CallTerm
        ], context_init), !IO),
    CallTerm =
        functor(atom(AnalysisName), [
            functor(integer(VersionNumber), [], context_init),
            type_to_term(FuncId),
            functor(string(to_string(Call)), [], context_init)
        ], context_init).

%-----------------------------------------------------------------------------%

write_module_imdg(Info, ModuleName, ModuleEntries, !IO) :-
    ToTmp = no,
    write_analysis_file(Info ^ compiler, ModuleName, imdg_suffix, ToTmp,
        write_imdg_arc(Info ^ compiler), ModuleEntries, _FileName, !IO).

:- pred write_imdg_arc(Compiler::in)
    `with_type` write_entry(imdg_arc)
    `with_inst` write_entry <= compiler(Compiler).

write_imdg_arc(Compiler, AnalysisName, FuncId, imdg_arc(Call, DependentModule),
        !IO) :-
    (
        analysis_type(_ : unit(Call), _ : unit(Answer))
            = analyses(Compiler, AnalysisName)
    ->
        VersionNumber = analysis_version_number(_ : Call, _ : Answer)
    ;
        unexpected(this_file,
            "write_imdg_arc: unknown analysis type")
    ),
    term_io.write_term_nl(varset.init : varset,
        functor(atom("->"), [
            type_to_term(DependentModule),
            ResultTerm
        ], context_init), !IO),
    ResultTerm = functor(atom(AnalysisName), [
        functor(integer(VersionNumber), [], context_init),
        type_to_term(FuncId),
        functor(string(to_string(Call)), [], context_init)
    ], context_init).

%-----------------------------------------------------------------------------%

:- type write_entry(T) == pred(analysis_name, func_id, T, io, io).
:- inst write_entry == (pred(in, in, in, di, uo) is det).

:- pred write_analysis_file(Compiler::in, module_name::in, string::in,
    bool::in, write_entry(T)::in(write_entry), module_analysis_map(T)::in,
    string::out, io::di, io::uo) is det <= compiler(Compiler).

write_analysis_file(Compiler, ModuleName, Suffix, ToTmp, WriteEntry,
        ModuleResults, FileName, !IO) :-
    module_name_to_write_file_name(Compiler, ModuleName, Suffix, FileName,
        !IO),
    (
        ToTmp = yes,
        WriteFileName = FileName ++ ".tmp"
    ;
        ToTmp = no,
        WriteFileName = FileName
    ),
    write_analysis_file(WriteFileName, WriteEntry, ModuleResults, !IO).

:- pred write_analysis_file(string::in, write_entry(T)::in(write_entry),
    module_analysis_map(T)::in, io::di, io::uo) is det.

write_analysis_file(FileName, WriteEntry, ModuleResults, !IO) :-
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.set_output_stream(Stream, OldOutput, !IO),
        io.write_int(version_number, !IO),
        io.write_string(".\n", !IO),
        write_analysis_file_2(WriteEntry, ModuleResults, !IO),
        io.set_output_stream(OldOutput, _, !IO),
        io.close_output(Stream, !IO)
    ;
        OpenResult = error(IOError),
        unexpected(this_file, "write_analysis_file: error opening `" ++
            FileName ++ "' for output: " ++ io.error_message(IOError))
    ).

:- pred write_analysis_file_2(write_entry(T)::in(write_entry),
    module_analysis_map(T)::in, io::di, io::uo) is det.

write_analysis_file_2(WriteEntry, ModuleResults, !IO) :-
    map.foldl(write_analysis_file_3(WriteEntry), ModuleResults, !IO).

:- pred write_analysis_file_3(write_entry(T)::in(write_entry), string::in,
    func_analysis_map(T)::in, io::di, io::uo) is det.

write_analysis_file_3(WriteEntry, AnalysisName, FuncResults, !IO) :-
    map.foldl(write_analysis_file_4(WriteEntry, AnalysisName),
        FuncResults, !IO).

:- pred write_analysis_file_4(write_entry(T)::in(write_entry), string::in,
    func_id::in, list(T)::in, io::di, io::uo) is det.

write_analysis_file_4(WriteEntry, AnalysisName, FuncId, FuncResultList, !IO) :-
    list.sort(FuncResultList, FuncResultListSorted),
    list.foldl(
        (pred(FuncResult::in, !.IO::di, !:IO::uo) is det :-
            WriteEntry(AnalysisName, FuncId, FuncResult, !IO)
        ), FuncResultListSorted, !IO).

%-----------------------------------------------------------------------------%

empty_request_file(Info, ModuleName, !IO) :-
    module_name_to_write_file_name(Info ^ compiler, ModuleName, request_suffix,
        RequestFileName, !IO),
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
        io.write_string("% Removing request file ", !IO),
        io.write_string(RequestFileName, !IO),
        io.nl(!IO)
    ), !IO),
    io.remove_file(RequestFileName, _, !IO).

%-----------------------------------------------------------------------------%
:- end_module analysis.file.
%-----------------------------------------------------------------------------%
