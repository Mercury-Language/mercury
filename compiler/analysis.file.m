%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2011 The University of Melbourne.
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

:- import_module libs.
:- import_module libs.globals.

    % read_module_overall_status(Compiler, Globals, ModuleName,
    %   MaybeModuleStatus, !IO)
    %
    % Read the overall status of a module from its `.analysis_status' file.
    % If the module has outstanding requests, then an overall status of
    % `optimal' is downgraded to `suboptimal'.
    %
:- pred read_module_overall_status(Compiler::in, globals::in, module_name::in,
    analysis_status::out, io::di, io::uo) is det <= compiler(Compiler).

    % write_module_overall_status(AnalysisInfo, Globals, ModuleName,
    %   ModuleStatus, !IO)
    %
    % Write the status of a module to its `.analysis_status' file.
    %
:- pred write_module_overall_status(analysis_info::in, globals::in,
    module_name::in, analysis_status::in, io::di, io::uo) is det.

    % read_module_analysis_results(AnalysisInfo, Globals, ModuleName,
    %   AnalysisResults, !IO)
    %
    % Read the analysis results from a `.analysis' file,
    % or from the analysis file cache (if enabled, and the cache file is
    % up-to-date).
    %
:- pred read_module_analysis_results(analysis_info::in, globals::in,
    module_name::in, module_analysis_map(some_analysis_result)::out,
    io::di, io::uo) is det.

    % write_module_analysis_results(AnalysisInfo, Globals, ModuleName,
    %   AnalysisResults, !IO)
    %
    % Write the analysis results for a module to its `.analysis' file.
    % Optionally, also write the cache copy of the analysis file.
    %
:- pred write_module_analysis_results(analysis_info::in, globals::in,
    module_name::in, module_analysis_map(some_analysis_result)::in,
    io::di, io::uo) is det.

    % read_module_analysis_requests(AnalysisInfo, Globals, ModuleName,
    %   ModuleRequests, !IO)
    %
    % Read outstanding analysis requests to a module from disk.
    %
:- pred read_module_analysis_requests(analysis_info::in, globals::in,
    module_name::in, module_analysis_map(analysis_request)::out,
    io::di, io::uo) is det.

    % write_module_analysis_requests(AnalysisInfo, Globals, ModuleName,
    %   ModuleRequests, !IO)
    %
    % Write outstanding analysis requests for a module to disk.
    %
:- pred write_module_analysis_requests(analysis_info::in, globals::in,
    module_name::in, module_analysis_map(analysis_request)::in,
    io::di, io::uo) is det.

    % read_module_imdg(AnalysisInfo, Globals, ModuleName, ModuleEntries, !IO)
    %
    % Read the intermodule dependencies graph entries for a module from disk.
    %
:- pred read_module_imdg(analysis_info::in, globals::in, module_name::in,
    module_analysis_map(imdg_arc)::out, io::di, io::uo) is det.

    % write_module_imdg(AnalysisInfo, Globals, ModuleName, ModuleEntries, !IO)
    %
    % Write the intermodule dependencies graph entries for a module to disk.
    %
:- pred write_module_imdg(analysis_info::in, globals::in, module_name::in,
    module_analysis_map(imdg_arc)::in, io::di, io::uo) is det.

    % empty_request_file(AnalysisInfo, Globals, ModuleName, !IO)
    %
    % Delete the file containing outstanding analysis requests for a module.
    % This means all the analysis requests should have been satisfied already.
    %
:- pred empty_request_file(analysis_info::in, globals::in, module_name::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.pickle.
:- import_module parse_tree.
:- import_module parse_tree.module_cmds.        % XXX unwanted dependency
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module exception.
:- import_module parser.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module type_desc.
:- import_module univ.
:- import_module varset.

%-----------------------------------------------------------------------------%

% The format of an analysis result file is:
%
% version_number.
% analysis_name(analysis_version, func_id, call_pattern, answer_pattern,
%   result_status).
%
% where func_id = p(name, arity, mode_id).
%   or  func_id = f(name, arity, mode_id).

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

version_number = 6.

:- func analysis_registry_ext = ext.

analysis_registry_ext = ext(".analysis").

:- func analysis_registry_status_ext = ext.

analysis_registry_status_ext = ext(".analysis_status").

:- func imdg_ext = ext.

imdg_ext = ext(".imdg").

:- func request_ext = ext.

request_ext = ext(".request").

%-----------------------------------------------------------------------------%

:- pred analysis_status_to_string(analysis_status, string).
:- mode analysis_status_to_string(in, out) is det.
:- mode analysis_status_to_string(out, in) is semidet.

analysis_status_to_string(invalid, "invalid").
analysis_status_to_string(suboptimal, "suboptimal").
analysis_status_to_string(optimal, "optimal").

%-----------------------------------------------------------------------------%
%
% Reading.
%

:- type parse_entry(T) == pred(term, T, T).
:- inst parse_entry == (pred(in, in, out) is det).

read_module_overall_status(Compiler, Globals, ModuleName, ModuleStatus, !IO) :-
    module_name_to_read_file_name(Compiler, Globals, ModuleName,
        analysis_registry_status_ext, MaybeFileName, !IO),
    (
        MaybeFileName = ok(FileName),
        read_module_overall_status_2(FileName, ModuleStatus0, !IO)
    ;
        MaybeFileName = error(_),
        % Missing file means optimal. We don't install `.analysis_status' files
        % when installing libraries, for example.
        ModuleStatus0 = optimal
    ),
    (
        ModuleStatus0 = optimal,
        module_name_to_read_file_name(Compiler, Globals, ModuleName,
            request_ext, MaybeRequestFileName, !IO),
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
            ( if string.prefix(String, "optimal.") then
                ModuleStatus = optimal
            else if string.prefix(String, "suboptimal.") then
                ModuleStatus = suboptimal
            else if string.prefix(String, "invalid.") then
                ModuleStatus = invalid
            else
                unexpected($pred, "unexpected line")
            )
        ;
            ReadResult = eof,
            unexpected($pred, "unexpected eof")
        ;
            ReadResult = error(IOError),
            unexpected($pred, io.error_message(IOError))
        )
    ;
        OpenResult = error(IOError),
        unexpected($pred, io.error_message(IOError))
    ).

%-----------------------------------------------------------------------------%

read_module_analysis_results(Info, Globals, ModuleName, ModuleResults, !IO) :-
    % If the module's overall status is `invalid', then at least one of its
    % results is invalid. However, we can't just discard the results,
    % as we want to know which results change after we reanalyse the module.
    Compiler = Info ^ compiler,
    module_name_to_read_file_name(Compiler, Globals, ModuleName,
        analysis_registry_ext, MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),

        % If analysis file caching is enabled, and the cache file exists
        % and is up-to-date, then read from the cache instead.
        globals.lookup_string_option(Globals, analysis_file_cache_dir,
            CacheDir),
        ( if CacheDir = "" then
            read_module_analysis_results_2(Compiler, AnalysisFileName,
                ModuleResults, !IO)
        else
            CacheFileName = make_cache_filename(CacheDir, AnalysisFileName),
            io.file_modification_time(AnalysisFileName, AnalysisTimeResult,
                !IO),
            io.file_modification_time(CacheFileName, CacheTimeResult, !IO),
            ( if
                AnalysisTimeResult = ok(AnalysisTime),
                CacheTimeResult = ok(CacheTime),
                CacheTime @>= AnalysisTime
            then
                Unpicklers = init_analysis_unpicklers(Compiler),
                unpickle_from_file(Unpicklers, CacheFileName, UnpickleResult,
                    !IO),
                (
                    UnpickleResult = ok(ModuleResults)
                ;
                    UnpickleResult = error(Error),
                    io.write_string("Error reading ", !IO),
                    io.write_string(CacheFileName, !IO),
                    io.write_string(": ", !IO),
                    io.write_string(io.error_message(Error), !IO),
                    io.nl(!IO),
                    read_module_analysis_results_2(Compiler, AnalysisFileName,
                        ModuleResults, !IO),
                    write_analysis_cache_file(CacheFileName, ModuleResults,
                        !IO)
                )
            else
                read_module_analysis_results_2(Compiler, AnalysisFileName,
                    ModuleResults, !IO),
                write_analysis_cache_file(CacheFileName, ModuleResults, !IO)
            )
        )
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
        debug_msg(
            ( pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("% Reading analysis registry file ", !IO),
                io.write_string(AnalysisFileName, !IO),
                io.nl(!IO)
            ), !IO),

        check_analysis_file_version_number(Stream, !IO),
        promise_equivalent_solutions [Results, !:IO] (
            try_io(read_analysis_file_2(Stream, parse_result_entry(Compiler),
                ModuleResults0), Results, !IO)
        ),
        io.close_input(Stream, !IO),
        (
            Results = succeeded(ModuleResults)
        ;
            Results = exception(_),
            rethrow(Results)
        )
    ;
        OpenResult = error(_),
        debug_msg(
            ( pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("% Error reading analysis registry file: ",
                    !IO),
                io.write_string(AnalysisFileName, !IO),
                io.nl(!IO)
            ), !IO),
        ModuleResults = ModuleResults0
    ).

:- pred parse_result_entry(Compiler::in, term::in,
    module_analysis_map(some_analysis_result)::in,
    module_analysis_map(some_analysis_result)::out) is det
    <= compiler(Compiler).

parse_result_entry(Compiler, Term, !Results) :-
    ( if
        Term = term.functor(term.atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm,
            CallPatternTerm, AnswerPatternTerm, StatusTerm], _),
        StatusTerm = term.functor(term.atom(StatusString), [], _),
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis,

        parse_func_id(FuncIdTerm, FuncId),
        from_term(CallPatternTerm, CallPattern : Call),
        from_term(AnswerPatternTerm, AnswerPattern : Answer),
        analysis_status_to_string(Status, StatusString)
    then
        ( if
            VersionNumber = analysis_version_number(_ : Call, _ : Answer),
            decimal_term_to_int(VersionNumberTerm, VersionNumber)
        then
            Result = 'new some_analysis_result'(CallPattern, AnswerPattern,
                Status),
            ( if map.search(!.Results, AnalysisName, AnalysisResults0) then
                AnalysisResults1 = AnalysisResults0
            else
                AnalysisResults1 = map.init
            ),
            ( if map.search(AnalysisResults1, FuncId, FuncResults0) then
                FuncResults = [Result | FuncResults0]
            else
                FuncResults = [Result]
            ),
            map.set(FuncId, FuncResults, AnalysisResults1, AnalysisResults),
            map.set(AnalysisName, AnalysisResults, !Results)
        else
            % Ignore results with an out-of-date version number.
            true
        )
    else
        Msg = "failed to parse result entry: " ++ string(Term),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%

read_module_analysis_requests(Info, Globals, ModuleName, ModuleRequests,
        !IO) :-
    find_and_read_analysis_file(Info ^ compiler, Globals, ModuleName,
        request_ext, parse_request_entry(Info ^ compiler),
        map.init, ModuleRequests, !IO).

:- pred parse_request_entry(Compiler::in, term::in,
    module_analysis_map(analysis_request)::in,
    module_analysis_map(analysis_request)::out) is det
    <= compiler(Compiler).

parse_request_entry(Compiler, Term, !Requests) :-
    ( if
        Term = term.functor(atom("->"), [CallerModuleTerm, RHS], _),
        RHS = term.functor(atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis,

        try_parse_module_name(CallerModuleTerm, CallerModule),
        parse_func_id(FuncIdTerm, FuncId),
        from_term(CallPatternTerm, CallPattern : Call)
    then
        ( if
            VersionNumber = analysis_version_number(_ : Call, _ : Answer),
            decimal_term_to_int(VersionNumberTerm, VersionNumber)
        then
            Result = 'new analysis_request'(CallPattern, CallerModule),
            ( if map.search(!.Requests, AnalysisName, AnalysisRequests0) then
                AnalysisRequests1 = AnalysisRequests0
            else
                AnalysisRequests1 = map.init
            ),
            ( if map.search(AnalysisRequests1, FuncId, FuncRequests0) then
                FuncRequests = [Result | FuncRequests0]
            else
                FuncRequests = [Result]
            ),
            map.set(FuncId, FuncRequests, AnalysisRequests1, AnalysisRequests),
            map.set(AnalysisName, AnalysisRequests, !Requests)
        else
            % Ignore requests with an out-of-date version number.
            true
        )
    else
        Msg = "failed to parse request entry: " ++ string(Term),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%

read_module_imdg(Info, Globals, ModuleName, ModuleEntries, !IO) :-
    find_and_read_analysis_file(Info ^ compiler, Globals, ModuleName,
        imdg_ext, parse_imdg_arc(Info ^ compiler),
        map.init, ModuleEntries, !IO).

:- pred parse_imdg_arc(Compiler::in, term::in,
    module_analysis_map(imdg_arc)::in, module_analysis_map(imdg_arc)::out)
    is det <= compiler(Compiler).

parse_imdg_arc(Compiler, Term, !Arcs) :-
    ( if
        Term = term.functor(atom("->"), [DependentModuleTerm, ResultTerm], _),
        ResultTerm = functor(atom(AnalysisName),
            [VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis,

        try_parse_module_name(DependentModuleTerm, DependentModule),
        parse_func_id(FuncIdTerm, FuncId),
        from_term(CallPatternTerm, CallPattern : Call)
    then
        ( if
            VersionNumber = analysis_version_number(_ : Call, _ : Answer),
            decimal_term_to_int(VersionNumberTerm, VersionNumber)
        then
            Arc = 'new imdg_arc'(CallPattern, DependentModule),
            ( if map.search(!.Arcs, AnalysisName, AnalysisArcs0) then
                AnalysisArcs1 = AnalysisArcs0
            else
                AnalysisArcs1 = map.init
            ),
            ( if map.search(AnalysisArcs1, FuncId, FuncArcs0) then
                FuncArcs = [Arc | FuncArcs0]
            else
                FuncArcs = [Arc]
            ),
            map.set(FuncId, FuncArcs, AnalysisArcs1, AnalysisArcs),
            map.set(AnalysisName, AnalysisArcs, !Arcs)
        else
            % Ignore results with an out-of-date version number.
            % XXX: is that the right thing to do?
            % do we really need a version number for the IMDG?
            true
        )
    else
        Msg = "failed to parse IMDG arc: " ++ string(Term),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%

:- pred parse_func_id(term::in, func_id::out) is semidet.

parse_func_id(Term, FuncId) :-
    Term = functor(atom(PF), [NameTerm, ArityTerm, ProcTerm], _),
    (
        PF = "p",
        PredOrFunc = pf_predicate
    ;
        PF = "f",
        PredOrFunc = pf_function
    ),
    NameTerm = functor(atom(Name), [], _),
    decimal_term_to_int(ArityTerm, Arity),
    decimal_term_to_int(ProcTerm, ProcInt),
    proc_id_to_int(ProcId, ProcInt),
    FuncId = func_id(PredOrFunc, Name, Arity, ProcId).

:- pred try_parse_module_name(term::in, module_name::out) is semidet.

try_parse_module_name(Term, ModuleName) :-
    try_parse_sym_name_and_no_args(Term, ModuleName).

%-----------------------------------------------------------------------------%

:- pred find_and_read_analysis_file(Compiler::in, globals::in, module_name::in,
    ext::in, parse_entry(T)::in(parse_entry), T::in, T::out,
    io::di, io::uo) is det <= compiler(Compiler).

find_and_read_analysis_file(Compiler, Globals, ModuleName, Ext, ParseEntry,
        ModuleResults0, ModuleResults, !IO) :-
    module_name_to_read_file_name(Compiler, Globals, ModuleName, Ext,
        MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),
        read_analysis_file(AnalysisFileName, ParseEntry,
            ModuleResults0, ModuleResults, !IO)
    ;
        MaybeAnalysisFileName = error(Message),
        debug_msg(
            ( pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("Couldn't open ", !IO),
                io.write_string(extension_to_string(Ext), !IO),
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
        debug_msg(
            ( pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("% Reading analysis file ", !IO),
                io.write_string(AnalysisFileName, !IO),
                io.nl(!IO)
            ), !IO),

        promise_equivalent_solutions [Result, !:IO] (
            try_io(
                ( pred(Results1::out, !.IO::di, !:IO::uo) is det :-
                    check_analysis_file_version_number(Stream, !IO),
                    read_analysis_file_2(Stream, ParseEntry,
                        ModuleResults0, Results1, !IO)
                ), Result, !IO)
        ),
        io.close_input(Stream, !IO),
        (
            Result = succeeded(ModuleResults)
        ;
            Result = exception(_),
            rethrow(Result)
        )
    ;
        OpenResult = error(_),
        debug_msg(
            ( pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("Error reading analysis file: ", !IO),
                io.write_string(AnalysisFileName, !IO),
                io.nl(!IO)
            ), !IO),
        ModuleResults = ModuleResults0
    ).

:- pred check_analysis_file_version_number(io.text_input_stream::in,
    io::di, io::uo) is det.

check_analysis_file_version_number(Stream, !IO) :-
    parser.read_term(Stream, TermResult : read_term, !IO),
    ( if
        TermResult  = term(_, NumberTerm),
        decimal_term_to_int(NumberTerm, version_number)
    then
        true
    else
        Msg = "bad analysis file version: " ++ string(TermResult),
        throw(invalid_analysis_file(Msg))
    ).

:- pred read_analysis_file_2(io.text_input_stream::in,
    parse_entry(T)::in(parse_entry), T::in, T::out, io::di, io::uo) is det.

read_analysis_file_2(Stream, ParseEntry, Results0, Results, !IO) :-
    parser.read_term(Stream, TermResult : read_term, !IO),
    (
        TermResult = term(_, Term),
        ParseEntry(Term, Results0, Results1),
        read_analysis_file_2(Stream, ParseEntry, Results1, Results, !IO)
    ;
        TermResult = eof,
        Results = Results0
    ;
        TermResult = error(Msg, _),
        throw(invalid_analysis_file(Msg))
    ).

%-----------------------------------------------------------------------------%
%
% Writing.
%

:- type write_entry(T) == pred(analysis_name, func_id, T, io, io).
:- inst write_entry == (pred(in, in, in, di, uo) is det).

write_module_overall_status(Info, Globals, ModuleName, Status, !IO) :-
    module_name_to_write_file_name(Info ^ compiler, Globals, ModuleName,
        analysis_registry_status_ext, FileName, !IO),
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
        unexpected($pred, io.error_message(IOError))
    ).

%-----------------------------------------------------------------------------%

write_module_analysis_results(Info, Globals, ModuleName, ModuleResults, !IO) :-
    debug_msg(
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Writing module analysis results for ", !IO),
            io.write(ModuleName, !IO),
            io.nl(!IO)
        ), !IO),
    find_and_write_analysis_file(Info ^ compiler, Globals, ModuleName,
        analysis_registry_ext, add_dot_temp, write_result_entry,
        ModuleResults, FileName, !IO),
    update_interface_return_changed(Globals, FileName, Result, !IO),

    % If analysis file caching is turned on, write the internal represention of
    % the module results to disk right now.
    globals.lookup_string_option(Globals, analysis_file_cache_dir, CacheDir),
    ( if
        CacheDir \= "",
        Result = interface_new_or_changed
    then
        CacheFileName = make_cache_filename(CacheDir, FileName),
        write_analysis_cache_file(CacheFileName, ModuleResults, !IO)
    else
        true
    ).

:- pred write_result_entry(analysis_name::in, func_id::in,
    some_analysis_result::in, io::di, io::uo) is det.

write_result_entry(AnalysisName, FuncId, Result, !IO) :-
    Result = some_analysis_result(Call, Answer, Status),
    VersionNumber = analysis_version_number(Call, Answer),
    analysis_status_to_string(Status, StatusString),

    io.write_string(AnalysisName, !IO),
    io.write_char('(', !IO),
    io.write_int(VersionNumber, !IO),
    io.write_string(", ", !IO),
    write_func_id(FuncId, !IO),
    io.write_string(", ", !IO),
    term_io.write_term(varset.init, to_term(Call), !IO),
    io.write_string(", ", !IO),
    term_io.write_term(varset.init, to_term(Answer), !IO),
    io.write_string(", ", !IO),
    io.write_string(StatusString, !IO),
    io.write_string(").\n", !IO).

%-----------------------------------------------------------------------------%

write_module_analysis_requests(Info, Globals, ModuleName, ModuleRequests,
        !IO) :-
    Compiler = Info ^ compiler,
    module_name_to_write_file_name(Compiler, Globals, ModuleName,
        request_ext, AnalysisFileName, !IO),
    debug_msg(
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Writing module analysis requests to ", !IO),
            io.write_string(AnalysisFileName, !IO),
            io.nl(!IO)
        ), !IO),
    io.open_input(AnalysisFileName, InputResult, !IO),
    (
        InputResult = ok(InputStream),
        % Request file already exists. Check it has the right version number,
        % then append the new requests to the end.

        parser.read_term(InputStream, VersionResult : read_term, !IO),
        io.close_input(InputStream, !IO),
        ( if
            VersionResult = term(_, NumberTerm),
            decimal_term_to_int(NumberTerm, version_number)
        then
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
        else
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

:- pred write_request_entry(Compiler::in, analysis_name::in, func_id::in,
    analysis_request::in, io::di, io::uo) is det <= compiler(Compiler).

write_request_entry(Compiler, AnalysisName, FuncId, Request, !IO) :-
    Request = analysis_request(Call, CallerModule),
    ( if
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis
    then
        VersionNumber = analysis_version_number(_ : Call, _ :  Answer)
    else
        unexpected($pred, "unknown analysis type")
    ),

    write_quoted_module_name(CallerModule, !IO),
    io.write_string(" -> ", !IO),
    io.write_string(AnalysisName, !IO),
    io.write_string("(", !IO),
    io.write_int(VersionNumber, !IO),
    io.write_string(", ", !IO),
    write_func_id(FuncId, !IO),
    io.write_string(", ", !IO),
    term_io.write_term(varset.init, to_term(Call), !IO),
    io.write_string(").\n", !IO).

%-----------------------------------------------------------------------------%

write_module_imdg(Info, Globals, ModuleName, ModuleEntries, !IO) :-
    find_and_write_analysis_file(Info ^ compiler, Globals, ModuleName,
        imdg_ext, do_not_add_dot_temp, write_imdg_arc(Info ^ compiler),
        ModuleEntries, _FileName, !IO).

:- pred write_imdg_arc(Compiler::in, analysis_name::in, func_id::in,
    imdg_arc::in, io::di, io::uo) is det <= compiler(Compiler).

write_imdg_arc(Compiler, AnalysisName, FuncId, Arc, !IO) :-
    Arc = imdg_arc(Call, DependentModule),
    ( if
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis
    then
        VersionNumber = analysis_version_number(_ : Call, _ : Answer)
    else
        unexpected($pred, "unknown analysis type")
    ),

    write_quoted_module_name(DependentModule, !IO),
    io.write_string(" -> ", !IO),
    io.write_string(AnalysisName, !IO),
    io.write_char('(', !IO),
    io.write_int(VersionNumber, !IO),
    io.write_string(", ", !IO),
    write_func_id(FuncId, !IO),
    io.write_string(", ", !IO),
    term_io.write_term(varset.init, to_term(Call), !IO),
    io.write_string(").\n", !IO).

%-----------------------------------------------------------------------------%

:- pred write_func_id(func_id::in, io::di, io::uo) is det.

write_func_id(func_id(PredOrFunc, Name, Arity, ProcId), !IO) :-
    (
        PredOrFunc = pf_predicate,
        io.write_string("p(", !IO)
    ;
        PredOrFunc = pf_function,
        io.write_string("f(", !IO)
    ),
    term_io.quote_atom(Name, !IO),
    io.write_string(", ", !IO),
    io.write_int(Arity, !IO),
    io.write_string(", ", !IO),
    io.write_int(proc_id_to_int(ProcId), !IO),
    io.write_char(')', !IO).

:- pred write_quoted_module_name(module_name::in, io::di, io::uo) is det.

write_quoted_module_name(ModuleName, !IO) :-
    write_quoted_sym_name(ModuleName, !IO).

%-----------------------------------------------------------------------------%

:- type maybe_add_dot_temp
    --->    do_not_add_dot_temp
    ;       add_dot_temp.

:- pred find_and_write_analysis_file(Compiler::in, globals::in,
    module_name::in, ext::in, maybe_add_dot_temp::in,
    write_entry(T)::in(write_entry), module_analysis_map(T)::in, string::out,
    io::di, io::uo) is det <= compiler(Compiler).

find_and_write_analysis_file(Compiler, Globals, ModuleName, Ext, ToTmp,
        WriteEntry, ModuleResults, FileName, !IO) :-
    module_name_to_write_file_name(Compiler, Globals, ModuleName, Ext,
        FileName, !IO),
    (
        ToTmp = add_dot_temp,
        WriteFileName = FileName ++ ".tmp"
    ;
        ToTmp = do_not_add_dot_temp,
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
        unexpected($pred,
            "error opening `" ++ FileName ++ "' for output: " ++
            io.error_message(IOError))
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
        ( pred(FuncResult::in, !.IO::di, !:IO::uo) is det :-
            WriteEntry(AnalysisName, FuncId, FuncResult, !IO)
        ), FuncResultListSorted, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

empty_request_file(Info, Globals, ModuleName, !IO) :-
    module_name_to_write_file_name(Info ^ compiler, Globals, ModuleName,
        request_ext, RequestFileName, !IO),
    debug_msg(
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Removing request file ", !IO),
            io.write_string(RequestFileName, !IO),
            io.nl(!IO)
        ), !IO),
    io.remove_file(RequestFileName, _, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Analysis file caching.
%
% An analysis cache file stores a binary representation of the parsed
% information in the corresponding .analysis file. In some cases,
% the binary format can be faster to read than the usual representation.
% The textual analysis files are portable, they more stable (they don't depend
% on compiler internals) and are easier to debug, which is why we don't
% use binary files exclusively.
%

:- func make_cache_filename(string, string) = string.

make_cache_filename(Dir, FileName) = CacheFileName :-
    Components = string.split_at_separator(dir_sep, FileName),
    EscFileName = string.join_list(":", Components),
    CacheFileName = Dir / EscFileName.

:- pred dir_sep(char::in) is semidet.

dir_sep(Char) :-
    dir.is_directory_separator(Char).

:- pred write_analysis_cache_file(string::in,
    module_analysis_map(some_analysis_result)::in, io::di, io::uo) is det.

write_analysis_cache_file(CacheFileName, ModuleResults, !IO) :-
    % Write to a temporary file first, and only move it into place
    % once it is complete.
    TmpFileName = CacheFileName ++ ".tmp",
    io.tell_binary(TmpFileName, TellRes, !IO),
    (
        TellRes = ok,
        pickle(init_analysis_picklers, ModuleResults, !IO),
        io.told_binary(!IO),
        io.rename_file(TmpFileName, CacheFileName, RenameRes, !IO),
        (
            RenameRes = ok
        ;
            RenameRes = error(Error),
            io.write_string("Error renaming ", !IO),
            io.write_string(CacheFileName, !IO),
            io.write_string(": ", !IO),
            io.write_string(io.error_message(Error), !IO),
            io.nl(!IO),
            io.remove_file(TmpFileName, _, !IO)
        )
    ;
        TellRes = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

:- func init_analysis_picklers = picklers.

init_analysis_picklers = Pickles :-
    some [!Pickles] (
        !:Pickles = init_picklers,
        Dummy = 'new some_analysis_result'(any_call, dummy_answer, optimal),
        Type = type_ctor(type_of(Dummy)),
        register_pickler(Type, pickle_analysis_result, !Pickles),
        Pickles = !.Pickles
    ).

:- pred pickle_analysis_result(picklers::in, univ::in, io::di, io::uo) is det.

pickle_analysis_result(Pickles, Univ, !IO) :-
    det_univ_to_type(Univ, some_analysis_result(Call, Answer, Status)),
    Name = analysis_name(Call, Answer),
    pickle(Pickles, Name, !IO),
    pickle(Pickles, Call, !IO),
    pickle(Pickles, Answer, !IO),
    pickle(Pickles, Status, !IO).

:- func init_analysis_unpicklers(Compiler) = unpicklers
    <= compiler(Compiler).

init_analysis_unpicklers(Compiler) = Unpicklers :-
    some [!Unpicklers] (
        !:Unpicklers = init_unpicklers,
        Dummy = 'new some_analysis_result'(any_call, dummy_answer, optimal),
        Type = type_ctor(type_of(Dummy)),
        register_unpickler(Type, unpickle_analysis_result(Compiler),
            !Unpicklers),
        Unpicklers = !.Unpicklers
    ).

:- pred unpickle_analysis_result(Compiler::in, unpicklers::in,
    unpickle_handle::in, type_desc::in, univ::out,
    unpickle_state::di, unpickle_state::uo) is det
    <= compiler(Compiler).

unpickle_analysis_result(Compiler, Unpicklers, Handle, _Type, Univ, !State) :-
    unpickle(Unpicklers, Handle, AnalysisName : string, !State),
    ( if
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis
    then
        unpickle(Unpicklers, Handle, Call : Call, !State),
        unpickle(Unpicklers, Handle, Answer : Answer, !State),
        unpickle(Unpicklers, Handle, Status, !State),
        Result = 'new some_analysis_result'(Call, Answer, Status),
        type_to_univ(Result, Univ)
    else
        unexpected($pred, AnalysisName)
    ).

% This is only needed so we can get the type_ctor_desc of
% `some_analysis_result' without referring to a real analysis.

:- type dummy_answer
    --->    dummy_answer.

:- instance answer_pattern(no_func_info, dummy_answer) where [].
:- instance partial_order(no_func_info, dummy_answer) where [
    ( more_precise_than(no_func_info, _, _) :-
        semidet_fail
    ),
    equivalent(no_func_info, dummy_answer, dummy_answer)
].
:- instance to_term(dummy_answer) where [
    ( to_term(dummy_answer) = Term :-
        Term = term.functor(atom("dummy"), [], context_init)
    ),
    ( from_term(Term, dummy_answer) :-
        Term = term.functor(atom("dummy"), [], _)
    )
].

:- instance analysis(no_func_info, any_call, dummy_answer) where [
    analysis_name(_, _) = "dummy",
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = greatest_fixpoint,
    bottom(_, _) = dummy_answer,
    top(_, _) = dummy_answer,
    get_func_info(_, _, _, _, _, no_func_info)
].

%-----------------------------------------------------------------------------%
:- end_module analysis.file.
%-----------------------------------------------------------------------------%
