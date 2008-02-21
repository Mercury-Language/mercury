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
% An analysis file contains analysis results for a single module.
%
%-----------------------------------------------------------------------------%

:- module analysis.file.

:- interface.

    % read_module_overall_status(Compiler, ModuleId, MaybeModuleStatus, !IO)
    %
    % Attempt to read the overall status from a module `.analysis' file.
    % If the module has outstanding requests, then an overall status of
    % `optimal' is downgraded to `suboptimal'.
    %
:- pred read_module_overall_status(Compiler::in, module_id::in,
    maybe(analysis_status)::out, io::di, io::uo) is det <= compiler(Compiler).

    % read_module_analysis_results(AnalysisInfo, ModuleId,
    %   OverallStatus, AnalysisResults, ExtraInfo, !IO)
    %
    % Read the overall module status, analysis results and any extra info
    % from a `.analysis' file.
    %
:- pred read_module_analysis_results(analysis_info::in, module_id::in,
    analysis_status::out, module_analysis_map(some_analysis_result)::out,
    module_extra_info_map::out, io::di, io::uo) is det.

    % write_module_analysis_results(AnalysisInfo, ModuleId,
    %   OverallStatus, AnalysisResults, ExtraInfo, !IO)
    %
    % Write the overall module status, analysis results and extra info
    % to a `.analysis' file.
    %
:- pred write_module_analysis_results(analysis_info::in,
    module_id::in, analysis_status::in,
    module_analysis_map(some_analysis_result)::in,
    module_extra_info_map::in, io::di, io::uo) is det.

    % read_module_analysis_requests(AnalysisInfo, ModuleId, ModuleRequests,
    %   !IO)
    %
    % Read outstanding analysis requests to a module from disk.
    %
:- pred read_module_analysis_requests(analysis_info::in,
    module_id::in, module_analysis_map(analysis_request)::out,
    io::di, io::uo) is det.

    % write_module_analysis_requests(AnalysisInfo, ModuleId, ModuleRequests,
    %   !IO)
    %
    % Write outstanding analysis requests for a module to disk.
    %
:- pred write_module_analysis_requests(analysis_info::in,
    module_id::in, module_analysis_map(analysis_request)::in,
    io::di, io::uo) is det.

    % read_module_imdg(AnalysisInfo, ModuleId, ModuleEntries, !IO)
    %
    % Read the intermodule dependencies graph entries for a module from disk.
    %
:- pred read_module_imdg(analysis_info::in, module_id::in,
    module_analysis_map(imdg_arc)::out, io::di, io::uo) is det.

    % write_module_imdg(AnalysisInfo, ModuleId, ModuleEntries, !IO)
    %
    % Write the intermodule dependencies graph entries for a module
    % to disk.
    %
:- pred write_module_imdg(analysis_info::in, module_id::in,
    module_analysis_map(imdg_arc)::in, io::di, io::uo) is det.

    % empty_request_file(AnalysisInfo, ModuleId, !IO)
    %
    % Delete the file containing outstanding analysis requests for a module.
    % This means all the analysis requests should have been satisfied already.
    %
:- pred empty_request_file(analysis_info::in, module_id::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module parser.
:- import_module term.
:- import_module term_io.
:- import_module varset.

:- import_module libs.compiler_util.

%-----------------------------------------------------------------------------%

% The format of an analysis result file is:
%
% version_number.
% module_status.
% extra_info(key, extra_info).
% analysis_name(analysis_version, func_id, call_pattern, answer_pattern,
%   result_status).
%
% All extra_infos, if any, must come before the analysis results.

% The format of an IMDG file is:
%
% version_number.
% calling_module -> analysis_name(analysis_version, func_id, call_pattern).

% The format of an analysis request file is:
%
% version_number.
% analysis_name(analysis_version, func_id, call_pattern).

:- type invalid_analysis_file
    --->    invalid_analysis_file.

:- func version_number = int.

version_number = 2.

:- func analysis_registry_suffix = string.

analysis_registry_suffix = ".analysis".

:- func imdg_suffix = string.

imdg_suffix = ".imdg".

:- func request_suffix = string.

request_suffix = ".request".

%-----------------------------------------------------------------------------%

read_module_overall_status(Compiler, ModuleId, MaybeModuleStatus, !IO) :-
    module_id_to_read_file_name(Compiler, ModuleId, analysis_registry_suffix,
        MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),
        read_module_overall_status_2(AnalysisFileName, MaybeModuleStatus0,
            !IO),
        ( MaybeModuleStatus0 = yes(optimal) ->
            module_id_to_read_file_name(Compiler, ModuleId, request_suffix,
                MaybeRequestFileName, !IO),
            (
                % There are outstanding requests for this module.
                MaybeRequestFileName = ok(_),
                MaybeModuleStatus = yes(suboptimal)
            ;
                MaybeRequestFileName = error(_),
                MaybeModuleStatus = MaybeModuleStatus0
            )
        ;
            MaybeModuleStatus = MaybeModuleStatus0
        )
    ;
        MaybeAnalysisFileName = error(_),
        MaybeModuleStatus = no
    ).

:- pred read_module_overall_status_2(string::in, maybe(analysis_status)::out,
    io::di, io::uo) is det.

read_module_overall_status_2(AnalysisFileName, MaybeModuleStatus, !IO) :-
    io.open_input(AnalysisFileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.set_input_stream(Stream, OldStream, !IO),

        promise_only_solution_io(
            (pred(Status::out, !.IO::di, !:IO::uo) is cc_multi :-
                try_io((pred(Status0::out, !.IO::di, !:IO::uo) is det :-
                    check_analysis_file_version_number(!IO),
                    read_module_status(Status0, !IO)
                ), Status, !IO)
            ), Result, !IO),
        (
            Result = succeeded(ModuleStatus),
            MaybeModuleStatus = yes(ModuleStatus)
        ;
            Result = failed,
            MaybeModuleStatus = no
        ;
            Result = exception(_),
            % XXX Report error.
            MaybeModuleStatus = no
        ),
        io.set_input_stream(OldStream, _, !IO),
        io.close_input(Stream, !IO)
    ;
        OpenResult = error(_),
        MaybeModuleStatus = no
    ).

%-----------------------------------------------------------------------------%

read_module_analysis_results(Info, ModuleId, ModuleStatus, ModuleResults,
        ExtraInfo, !IO) :-
    % If the module's overall status is `invalid' then at least one of its
    % results is invalid.  However, we can't just discard the results as we
    % want to know which results change after we reanalyse the module.
    Compiler = Info ^ compiler,
    module_id_to_read_file_name(Compiler, ModuleId, analysis_registry_suffix,
        MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),
        read_module_analysis_results_2(Compiler, AnalysisFileName,
            ModuleStatus, ModuleResults, ExtraInfo, !IO)
    ;
        MaybeAnalysisFileName = error(_),
        ModuleStatus = optimal,
        ModuleResults = map.init,
        ExtraInfo = map.init
    ).

:- pred read_module_analysis_results_2(Compiler::in, string::in,
    analysis_status::out, module_analysis_map(some_analysis_result)::out,
    module_extra_info_map::out, io::di, io::uo) is det <= compiler(Compiler).

read_module_analysis_results_2(Compiler, AnalysisFileName,
    ModuleStatus, ModuleResults, ExtraInfo, !IO) :-
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
        read_module_status(ModuleStatus, !IO),
        read_module_extra_infos(map.init, ExtraInfo, MaybeFirstResultEntry,
            !IO),
        (
            MaybeFirstResultEntry = yes(FirstResultEntry),
            ParseEntry = parse_result_entry(Compiler),
            promise_only_solution_io(
                (pred(Results3::out, !.IO::di, !:IO::uo) is cc_multi :-
                    try_io((pred(Results2::out, !.IO::di, !:IO::uo) is det :-
                        ParseEntry(FirstResultEntry, ModuleResults0, Results1),
                        read_analysis_file_2(ParseEntry, Results1, Results2,
                            !IO)
                    ), Results3, !IO)
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
            )
        ;
            MaybeFirstResultEntry = no,
            ModuleResults = map.init
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
        ModuleStatus = optimal,
        ModuleResults = ModuleResults0,
        ExtraInfo = map.init
    ).

:- pred read_module_status(analysis_status::out, io::di, io::uo) is det.

read_module_status(Status, !IO) :-
    parser.read_term(TermResult : read_term, !IO),
    ( TermResult = term(_, term.functor(term.atom(String), [], _)) ->
        ( analysis_status_to_string(Status0, String) ->
            Status = Status0
        ;
            throw(invalid_analysis_file)
        )
    ;
        throw(invalid_analysis_file)
    ).

:- pred analysis_status_to_string(analysis_status, string).
:- mode analysis_status_to_string(in, out) is det.
:- mode analysis_status_to_string(out, in) is semidet.

analysis_status_to_string(invalid, "invalid").
analysis_status_to_string(suboptimal, "suboptimal").
analysis_status_to_string(optimal, "optimal").

:- pred read_module_extra_infos(module_extra_info_map::in,
    module_extra_info_map::out, maybe(term)::out, io::di, io::uo) is det.

read_module_extra_infos(ExtraInfo0, ExtraInfo, MaybeFirstResultEntry, !IO) :-
    parser.read_term(TermResult, !IO),
    (
        TermResult = eof,
        ExtraInfo = ExtraInfo0,
        MaybeFirstResultEntry = no
    ;
        TermResult = error(_, _),
        throw(invalid_analysis_file)
    ;
        TermResult = term(_, Term),
        ( Term = term.functor(atom("extra_info"), Args, _) ->
            (
                Args = [KeyTerm, ValueTerm],
                KeyTerm = term.functor(string(Key), [], _),
                ValueTerm = term.functor(string(Value), [], _)
            ->
                map.det_insert(ExtraInfo0, Key, Value, ExtraInfo1),
                read_module_extra_infos(ExtraInfo1, ExtraInfo,
                    MaybeFirstResultEntry, !IO)
            ;
                throw(invalid_analysis_file)
            )
        ;
            ExtraInfo = ExtraInfo0,
            MaybeFirstResultEntry = yes(Term)
        )
    ).

:- pred parse_result_entry(Compiler::in)
    `with_type` parse_entry(module_analysis_map(some_analysis_result))
    `with_inst` parse_entry <= compiler(Compiler).

parse_result_entry(Compiler, Term, Results0, Results) :-
    (
        Term = term.functor(term.atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm,
            CallPatternTerm, AnswerPatternTerm, StatusTerm], _),
        FuncIdTerm = term.functor(term.string(FuncId), [], _),
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
            ( FuncResults0 = map.search(AnalysisResults1, FuncId) ->
                FuncResults = [Result | FuncResults0]
            ;
                FuncResults = [Result]
            ),
            Results = map.set(Results0, AnalysisName,
                map.set(AnalysisResults1, FuncId, FuncResults))
            ;
            % Ignore results with an out-of-date version number.
            Results = Results0
        )
    ;
        throw(invalid_analysis_file)
    ).

%-----------------------------------------------------------------------------%

read_module_analysis_requests(Info, ModuleId, ModuleRequests, !IO) :-
    read_analysis_file(Info ^ compiler, ModuleId, request_suffix,
        parse_request_entry(Info ^ compiler),
        map.init, ModuleRequests, !IO).

:- pred parse_request_entry(Compiler::in)
    `with_type` parse_entry(module_analysis_map(analysis_request))
    `with_inst` parse_entry <= compiler(Compiler).

parse_request_entry(Compiler, Term, Requests0, Requests) :-
    (
        Term = term.functor(term.atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
        FuncIdTerm = term.functor(term.string(FuncId), [], _),
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
            Result = 'new analysis_request'(CallPattern),
            ( AnalysisRequests0 = map.search(Requests0, AnalysisName) ->
                AnalysisRequests1 = AnalysisRequests0
            ;
                AnalysisRequests1 = map.init
            ),
            ( FuncRequests0 = map.search(AnalysisRequests1, FuncId) ->
                FuncRequests = [Result | FuncRequests0]
            ;
                FuncRequests = [Result]
            ),
            Requests = map.set(Requests0, AnalysisName,
                map.set(AnalysisRequests1, FuncId, FuncRequests))
        ;
            % Ignore requests with an out-of-date version number.
            Requests = Requests0
        )
    ;
        throw(invalid_analysis_file)
    ).

%-----------------------------------------------------------------------------%

read_module_imdg(Info, ModuleId, ModuleEntries, !IO) :-
    read_analysis_file(Info ^ compiler, ModuleId, imdg_suffix,
        parse_imdg_arc(Info ^ compiler),
        map.init, ModuleEntries, !IO).

:- pred parse_imdg_arc(Compiler::in)
    `with_type` parse_entry(module_analysis_map(imdg_arc))
    `with_inst` parse_entry <= compiler(Compiler).

parse_imdg_arc(Compiler, Term, Arcs0, Arcs) :-
    (
        Term = term.functor(atom("->"),
            [term.functor(string(DependentModule), [], _), ResultTerm], _),
        ResultTerm = functor(atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
        FuncIdTerm = term.functor(term.string(FuncId), [], _),
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
            ( FuncArcs0 = map.search(AnalysisArcs1, FuncId) ->
                FuncArcs = [Arc | FuncArcs0]
            ;
                FuncArcs = [Arc]
            ),
            Arcs = map.set(Arcs0, AnalysisName,
            map.set(AnalysisArcs1, FuncId, FuncArcs))
        ;
            % Ignore results with an out-of-date version number.
            % XXX: is that the right thing to do?
            % do we really need a version number for the IMDG?
            Arcs = Arcs0
        )
    ;
        throw(invalid_analysis_file)
    ).

%-----------------------------------------------------------------------------%

:- type read_analysis_header(T) == pred(T, io, io).
:- inst read_analysis_header == (pred(out, di, uo) is det).

:- type parse_entry(T) == pred(term, T, T).
:- inst parse_entry == (pred(in, in, out) is det).

:- pred read_analysis_file(Compiler::in, module_id::in, string::in,
    parse_entry(T)::in(parse_entry), T::in, T::out, io::di, io::uo) is det
    <= compiler(Compiler).

read_analysis_file(Compiler, ModuleId, Suffix, ParseEntry,
        ModuleResults0, ModuleResults, !IO) :-
    module_id_to_read_file_name(Compiler, ModuleId, Suffix,
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
            io.write_string(ModuleId, !IO),
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
        throw(invalid_analysis_file)
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
        TermResult = error(_, _),
        throw(invalid_analysis_file)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

write_module_analysis_results(Info, ModuleId, ModuleStatus, ModuleResults,
        ExtraInfo, !IO) :-
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
        io.write_string("% Writing module analysis results for ", !IO),
        io.write_string(ModuleId, !IO),
        io.nl(!IO)
    ), !IO),
    WriteHeader = write_module_status_and_extra_info(ModuleStatus, ExtraInfo),
    write_analysis_file(Info ^ compiler,
        ModuleId, analysis_registry_suffix,
        WriteHeader, write_result_entry, ModuleResults, !IO).

:- pred write_module_status_and_extra_info(analysis_status::in,
    module_extra_info_map::in, io::di, io::uo) is det.

write_module_status_and_extra_info(Status, ExtraInfo, !IO) :-
    write_module_status(Status, !IO),
    map.foldl(write_extra_info, ExtraInfo, !IO).

:- pred write_module_status(analysis_status::in, io::di, io::uo) is det.

write_module_status(Status, !IO) :-
    term_io.write_term_nl(init:varset, Term, !IO),
    Term = functor(atom(String), [], context_init),
    analysis_status_to_string(Status, String).

:- pred write_extra_info(extra_info_key::in, string::in,
    io::di, io::uo) is det.

write_extra_info(Key, Value, !IO) :-
    term_io.write_term_nl(varset.init : varset, Term, !IO),
    Term = functor(atom("extra_info"), [KeyTerm, ValueTerm], context_init),
    KeyTerm = functor(string(Key), [], context_init),
    ValueTerm = functor(string(Value), [], context_init).

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
                functor(string(FuncId), [], context_init),
            functor(string(to_string(Call)), [], context_init),
            functor(string(to_string(Answer)), [], context_init),
            functor(string(StatusString), [], context_init)
        ], context_init), !IO).

%-----------------------------------------------------------------------------%

write_module_analysis_requests(Info, ModuleId, ModuleRequests, !IO) :-
    Compiler = Info ^ compiler,
    module_id_to_write_file_name(Compiler, ModuleId, request_suffix,
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
                write_analysis_entries(write_request_entry(Compiler),
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
        write_analysis_file(AnalysisFileName, nop,
            write_request_entry(Compiler), ModuleRequests, !IO)
    ;
        Appended = yes
    ).

:- pred write_request_entry(Compiler::in)
    `with_type` write_entry(analysis_request)
    `with_inst` write_entry <= compiler(Compiler).

write_request_entry(Compiler, AnalysisName, FuncId, analysis_request(Call),
        !IO) :-
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
        functor(atom(AnalysisName), [
            functor(integer(VersionNumber), [], context_init),
                functor(string(FuncId), [], context_init),
            functor(string(to_string(Call)), [], context_init)
        ], context_init), !IO).

%-----------------------------------------------------------------------------%

write_module_imdg(Info, ModuleId, ModuleEntries, !IO) :-
    write_analysis_file(Info ^ compiler, ModuleId, imdg_suffix, nop,
        write_imdg_arc(Info ^ compiler), ModuleEntries, !IO).

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
            functor(string(DependentModule), [], context_init),
            ResultTerm
        ], context_init), !IO),
    ResultTerm = functor(atom(AnalysisName), [
        functor(integer(VersionNumber), [], context_init),
        functor(string(FuncId), [], context_init),
        functor(string(to_string(Call)), [], context_init)
        ], context_init).

%-----------------------------------------------------------------------------%

:- type write_header == pred(io, io).
:- inst write_header == (pred(di, uo) is det).

:- type write_entry(T) == pred(analysis_name, func_id, T, io, io).
:- inst write_entry == (pred(in, in, in, di, uo) is det).

:- pred write_analysis_file(Compiler::in, module_id::in, string::in,
    write_header::in(write_header),
    write_entry(T)::in(write_entry), module_analysis_map(T)::in,
    io::di, io::uo) is det <= compiler(Compiler).

write_analysis_file(Compiler, ModuleId, Suffix, WriteHeader, WriteEntry,
        ModuleResults, !IO) :-
    module_id_to_write_file_name(Compiler, ModuleId, Suffix,
        AnalysisFileName, !IO),
    write_analysis_file(AnalysisFileName, WriteHeader, WriteEntry,
        ModuleResults, !IO).

:- pred write_analysis_file(string::in, write_header::in(write_header),
    write_entry(T)::in(write_entry), module_analysis_map(T)::in,
    io::di, io::uo) is det.

write_analysis_file(AnalysisFileName, WriteHeader, WriteEntry, ModuleResults,
        !IO) :-
    io.open_output(AnalysisFileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.set_output_stream(Stream, OldOutput, !IO),
        io.write_int(version_number, !IO),
        io.write_string(".\n", !IO),
        WriteHeader(!IO),
        write_analysis_entries(WriteEntry, ModuleResults, !IO),
        io.set_output_stream(OldOutput, _, !IO),
        io.close_output(Stream, !IO)
    ;
        OpenResult = error(Msg),
        io.write_string("Error opening ", !IO),
        io.write_string(AnalysisFileName, !IO),
        io.write_string(" for output: ", !IO),
        io.write_string(io.error_message(Msg), !IO),
        io.nl(!IO)
    ).

:- pred write_analysis_entries(write_entry(T)::in(write_entry),
    module_analysis_map(T)::in, io::di, io::uo) is det.

write_analysis_entries(WriteEntry, ModuleResults, !IO) :-
    map.foldl(
        (pred(AnalysisName::in, FuncResults::in, di, uo) is det -->
            map.foldl(
                (pred(FuncId::in, FuncResultList::in, di, uo) is det -->
                    list.foldl(
                        (pred(FuncResult::in, di, uo) is det -->
                            WriteEntry(AnalysisName, FuncId, FuncResult)
                        ), FuncResultList)
                ),
                FuncResults)
        ),
        ModuleResults, !IO).

%-----------------------------------------------------------------------------%

empty_request_file(Info, ModuleId, !IO) :-
    module_id_to_write_file_name(Info ^ compiler, ModuleId, request_suffix,
        RequestFileName, !IO),
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
        io.write_string("% Removing request file ", !IO),
        io.write_string(RequestFileName, !IO),
        io.nl(!IO)
    ), !IO),
    io.remove_file(RequestFileName, _, !IO).

%-----------------------------------------------------------------------------%

:- pred nop(io::di, io::uo) is det.

nop(!IO).

%-----------------------------------------------------------------------------%
:- end_module analysis.file.
%-----------------------------------------------------------------------------%
