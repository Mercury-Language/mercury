%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: analysis.file.m
% Main author: stayl, wangp.
%
% This module deals with the on-disk representations of the analysis registry
% and associated files.
%
%---------------------------------------------------------------------------%

:- module analysis.file.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

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

%---------------------%

    % read_module_analysis_results(ProgressStream, AnalysisInfo, Globals,
    %   ModuleName, AnalysisResults, !IO)
    %
    % Read the analysis results from a `.analysis' file,
    % or from the analysis file cache (if enabled, and the cache file is
    % up-to-date).
    %
:- pred read_module_analysis_results(io.text_output_stream::in,
    analysis_info::in, globals::in,
    module_name::in, module_analysis_map(some_analysis_result)::out,
    list(error_spec)::out, io::di, io::uo) is det.

    % write_module_analysis_results(ProgressStream, AnalysisInfo, Globals,
    %   ModuleName, AnalysisResults, !IO)
    %
    % Write the analysis results for a module to its `.analysis' file.
    % Optionally, also write the cache copy of the analysis file.
    %
:- pred write_module_analysis_results(io.text_output_stream::in,
    analysis_info::in, globals::in,
    module_name::in, module_analysis_map(some_analysis_result)::in,
    io::di, io::uo) is det.

%---------------------%

    % read_module_analysis_requests(AnalysisInfo, Globals, ModuleName,
    %   ModuleRequests, !IO)
    %
    % Read outstanding analysis requests to a module from disk.
    %
:- pred read_module_analysis_requests(analysis_info::in, globals::in,
    module_name::in, module_analysis_map(analysis_request)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

    % write_module_analysis_requests(AnalysisInfo, Globals, ModuleName,
    %   ModuleRequests, !IO)
    %
    % Write outstanding analysis requests for a module to disk.
    %
:- pred write_module_analysis_requests(analysis_info::in, globals::in,
    module_name::in, module_analysis_map(analysis_request)::in,
    io::di, io::uo) is det.

%---------------------%

    % read_module_imdg(AnalysisInfo, Globals, ModuleName, ModuleEntries, !IO)
    %
    % Read the intermodule dependencies graph entries for a module from disk.
    %
:- pred read_module_imdg(analysis_info::in, globals::in, module_name::in,
    module_analysis_map(imdg_arc)::out, list(error_spec)::out,
    io::di, io::uo) is det.

    % write_module_imdg(AnalysisInfo, Globals, ModuleName, ModuleEntries, !IO)
    %
    % Write the intermodule dependencies graph entries for a module to disk.
    %
:- pred write_module_imdg(analysis_info::in, globals::in, module_name::in,
    module_analysis_map(imdg_arc)::in, io::di, io::uo) is det.

%---------------------%

    % empty_request_file(AnalysisInfo, Globals, ModuleName, !IO)
    %
    % Delete the file containing outstanding analysis requests for a module.
    % This means all the analysis requests should have been satisfied already.
    %
:- pred empty_request_file(analysis_info::in, globals::in, module_name::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.pickle.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module io.file.
:- import_module mercury_term_lexer.
:- import_module mercury_term_parser.
:- import_module require.
:- import_module string.
:- import_module term_context.
:- import_module term_int.
:- import_module term_io.
:- import_module type_desc.
:- import_module univ.
:- import_module varset.

%---------------------------------------------------------------------------%

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

:- func version_number = int.

version_number = 6.

%---------------------------------------------------------------------------%

:- pred analysis_status_to_string(analysis_status, string).
:- mode analysis_status_to_string(in, out) is det.
:- mode analysis_status_to_string(out, in) is semidet.

analysis_status_to_string(invalid, "invalid").
analysis_status_to_string(suboptimal, "suboptimal").
analysis_status_to_string(optimal, "optimal").

%---------------------------------------------------------------------------%
%
% Reading and writing overall status.
%

:- type parse_entry(T) ==
    pred(varset, term, T, T, list(error_spec), list(error_spec)).
:- inst parse_entry ==
    (pred(in, in, in, out, in, out) is det).

read_module_overall_status(Compiler, Globals, ModuleName, ModuleStatus, !IO) :-
    module_name_to_read_file_name(Compiler, Globals,
        ext_cur_ngs_gs(ext_cur_ngs_gs_an_ds_status),
        ModuleName, MaybeFileName, !IO),
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
        module_name_to_read_file_name(Compiler, Globals,
            ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_request),
            ModuleName, MaybeRequestFileName, !IO),
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

%---------------------%

:- type write_entry(T) ==
    pred(io.text_output_stream, analysis_name, func_id, T, io, io).
:- inst write_entry ==
    (pred(in, in, in, in, di, uo) is det).

write_module_overall_status(Info, Globals, ModuleName, Status, !IO) :-
    module_name_to_write_file_name(Info ^ compiler, Globals,
        ext_cur_ngs_gs(ext_cur_ngs_gs_an_ds_status),
        ModuleName, FileName, !IO),
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

%---------------------------------------------------------------------------%
%
% Reading and writing analysis results.
%

read_module_analysis_results(ProgressStream, Info, Globals,
        ModuleName, ModuleResults, Specs, !IO) :-
    % If the module's overall status is `invalid', then at least one of its
    % results is invalid. However, we can't just discard the results,
    % as we want to know which results change after we reanalyse the module.
    Compiler = Info ^ compiler,
    module_name_to_read_file_name(Compiler, Globals,
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
        ModuleName, MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),

        % If analysis file caching is enabled, and the cache file exists
        % and is up-to-date, then read from the cache instead.
        globals.lookup_string_option(Globals, analysis_file_cache_dir,
            CacheDir),
        ( if CacheDir = "" then
            do_read_module_analysis_results(Compiler, AnalysisFileName,
                ModuleResults, Specs, !IO)
        else
            CacheFileName = make_cache_filename(CacheDir, AnalysisFileName),
            io.file.file_modification_time(AnalysisFileName,
                AnalysisTimeResult, !IO),
            io.file.file_modification_time(CacheFileName,
                CacheTimeResult, !IO),
            ( if
                AnalysisTimeResult = ok(AnalysisTime),
                CacheTimeResult = ok(CacheTime),
                CacheTime @>= AnalysisTime
            then
                Unpicklers = init_analysis_unpicklers(Compiler),
                unpickle_from_file(Unpicklers, CacheFileName, UnpickleResult,
                    !IO),
                (
                    UnpickleResult = ok(ModuleResults),
                    Specs = []
                ;
                    UnpickleResult = error(Error),
                    io.error_message(Error, ErrorMsg),
                    io.format(ProgressStream, "Error reading %s: %s\n",
                        [s(CacheFileName), s(ErrorMsg)], !IO),
                    do_read_module_analysis_results(Compiler, AnalysisFileName,
                        ModuleResults, Specs, !IO),
                    maybe_write_analysis_cache_file(CacheFileName,
                        ModuleResults, Specs, !IO)
                )
            else
                do_read_module_analysis_results(Compiler, AnalysisFileName,
                    ModuleResults, Specs, !IO),
                maybe_write_analysis_cache_file(CacheFileName,
                    ModuleResults, Specs, !IO)
            )
        )
    ;
        MaybeAnalysisFileName = error(_),
        ModuleResults = map.init,
        % XXX Why is a failed open of an input file *less* fatal than
        % not being able to parse the contents of the file?
        Specs = []
    ).

:- pred do_read_module_analysis_results(Compiler::in, string::in,
    module_analysis_map(some_analysis_result)::out, list(error_spec)::out,
    io::di, io::uo) is det <= compiler(Compiler).

do_read_module_analysis_results(Compiler, AnalysisFileName, !:ModuleResults,
        !:Specs, !IO) :-
    !:ModuleResults = map.init,
    !:Specs = [],
    io.read_named_file_as_string(AnalysisFileName, FileResult, !IO),
    (
        FileResult = ok(FileStr),
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            io.format(DebugStream, "%% Reading analysis registry file %s\n",
                [s(AnalysisFileName)], !IO)
        ),

        string.length(FileStr, MaxOffset),
        LineContext0 = line_context(1, 0),
        LinePosn0 = line_posn(0),
        check_analysis_file_version_number(AnalysisFileName,
            FileStr, MaxOffset, LineContext0, LineContext1,
            LinePosn0, LinePosn1, !Specs),
        parse_analysis_file_entries(AnalysisFileName, FileStr, MaxOffset,
            parse_result_entry(Compiler), LineContext1, LinePosn1,
            !ModuleResults, !Specs)
    ;
        FileResult = error(_),
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            io.format(DebugStream,
                "%% Error reading analysis registry file: %s\n",
                [s(AnalysisFileName)], !IO)
        )
        % XXX Why is a failed open of an input file *less* fatal than
        % not being able to parse the contents of the file?
    ).

:- pred parse_result_entry(Compiler::in, varset::in, term::in,
    module_analysis_map(some_analysis_result)::in,
    module_analysis_map(some_analysis_result)::out,
    list(error_spec)::in, list(error_spec)::out) is det <= compiler(Compiler).

parse_result_entry(Compiler, VarSet, Term, !Results, !Specs) :-
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
            term_int.decimal_term_to_int(VersionNumberTerm, VersionNumber)
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
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a result entry, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

write_module_analysis_results(ProgressStream, Info, Globals,
        ModuleName, ModuleResults, !IO) :-
    get_debug_analysis_stream(MaybeDebugStream, !IO),
    (
        MaybeDebugStream = no
    ;
        MaybeDebugStream = yes(DebugStream),
        io.format(DebugStream, "%%s Writing module analysis results for %s\n",
            [s(sym_name_to_string(ModuleName))], !IO)
    ),
    find_and_write_analysis_file(Info ^ compiler, Globals,
        add_dot_temp, write_result_entry,
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
        ModuleName, ModuleResults, FileName, !IO),
    copy_dot_tmp_to_base_file_return_changed(ProgressStream, Globals, FileName,
        UpdateResult, !IO),

    % If analysis file caching is turned on, write the internal represention
    % of the module results to disk right now.
    globals.lookup_string_option(Globals, analysis_file_cache_dir, CacheDir),
    ( if
        CacheDir \= "",
        UpdateResult = base_file_new_or_changed
    then
        CacheFileName = make_cache_filename(CacheDir, FileName),
        write_analysis_cache_file(CacheFileName, ModuleResults, !IO)
    else
        true
    ).

:- pred write_result_entry(io.text_output_stream::in,
    analysis_name::in, func_id::in, some_analysis_result::in,
    io::di, io::uo) is det.

write_result_entry(OutStream, AnalysisName, FuncId, Result, !IO) :-
    Result = some_analysis_result(Call, Answer, Status),
    VersionNumber = analysis_version_number(Call, Answer),
    analysis_status_to_string(Status, StatusString),

    FuncIdStr = func_id_to_string(FuncId),
    io.format(OutStream, "%s(%d, %s, ",
        [s(AnalysisName), i(VersionNumber), s(FuncIdStr)], !IO),
    term_io.write_term(OutStream, varset.init, to_term(Call), !IO),
    io.write_string(OutStream, ", ", !IO),
    term_io.write_term(OutStream, varset.init, to_term(Answer), !IO),
    io.format(OutStream, ", %s).\n",
        [s(StatusString)], !IO).

%---------------------------------------------------------------------------%
%
% Reading and writing analysis requests.
%

read_module_analysis_requests(Info, Globals, ModuleName, ModuleRequests,
        !Specs, !IO) :-
    find_and_read_analysis_file(Info ^ compiler, Globals,
        parse_request_entry(Info ^ compiler),
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_request), ModuleName,
        map.init, ModuleRequests, !Specs, !IO).

:- pred parse_request_entry(Compiler::in, varset::in, term::in,
    module_analysis_map(analysis_request)::in,
    module_analysis_map(analysis_request)::out,
    list(error_spec)::in, list(error_spec)::out) is det <= compiler(Compiler).

parse_request_entry(Compiler, VarSet, Term, !Requests, !Specs) :-
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
            term_int.decimal_term_to_int(VersionNumberTerm, VersionNumber)
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
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a request entry, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

write_module_analysis_requests(Info, Globals, ModuleName, ModuleRequests,
        !IO) :-
    Compiler = Info ^ compiler,
    module_name_to_write_file_name(Compiler, Globals,
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_request),
        ModuleName, AnalysisFileName, !IO),
    get_debug_analysis_stream(MaybeDebugStream, !IO),
    (
        MaybeDebugStream = no
    ;
        MaybeDebugStream = yes(DebugStream),
        io.format(DebugStream, "%% Writing module analysis requests to %s\n",
            [s(AnalysisFileName)], !IO)
    ),
    io.read_named_file_as_string(AnalysisFileName, FileResult, !IO),
    (
        FileResult = ok(FileStr),
        % Request file already exists. Check it has the right version number,
        % then append the new requests to the end.
        % XXX Check whether FileStr is well_formed.

        string.length(FileStr, MaxOffset),
        LineContext0 = line_context(1, 0),
        LinePosn0 = line_posn(0),
        mercury_term_parser.read_term_from_linestr(AnalysisFileName,
            FileStr, MaxOffset, LineContext0, _LineContext1,
            LinePosn0, _LinePosn1, VersionResult : read_term),
        ( if
            VersionResult = term(_, NumberTerm),
            term_int.decimal_term_to_int(NumberTerm, version_number)
        then
            io.open_append(AnalysisFileName, AppendResult, !IO),
            (
                AppendResult = ok(AppendStream),
                write_module_analysis_map(AppendStream,
                    write_request_entry(Compiler), ModuleRequests, !IO),
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
        FileResult = error(_),
        Appended = no
    ),
    (
        Appended = no,
        write_analysis_file(AnalysisFileName, write_request_entry(Compiler),
            ModuleRequests, !IO)
    ;
        Appended = yes
    ).

:- pred write_request_entry(Compiler::in, io.text_output_stream::in,
    analysis_name::in, func_id::in, analysis_request::in, io::di, io::uo)
    is det <= compiler(Compiler).

write_request_entry(Compiler, OutStream, AnalysisName, FuncId, Request, !IO) :-
    Request = analysis_request(Call, CallerModule),
    ( if
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis
    then
        VersionNumber = analysis_version_number(_ : Call, _ :  Answer)
    else
        unexpected($pred, "unknown analysis type")
    ),

    FuncIdStr = func_id_to_string(FuncId),
    CallerModuleStr = escaped_sym_name_to_string(CallerModule),
    CallTermStr = term_io.term_to_string(varset.init, to_term(Call)),
    io.format(OutStream, "'%s' -> %s(%d, %s, %s).\n",
        [s(CallerModuleStr), s(AnalysisName),
        i(VersionNumber), s(FuncIdStr), s(CallTermStr)], !IO).

%---------------------------------------------------------------------------%
%
% Reading and writing imdgs.
%

read_module_imdg(Info, Globals, ModuleName, ModuleEntries, Specs, !IO) :-
    find_and_read_analysis_file(Info ^ compiler, Globals,
        parse_imdg_arc(Info ^ compiler),
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_imdg),
        ModuleName, map.init, ModuleEntries, [], Specs, !IO).

:- pred parse_imdg_arc(Compiler::in, varset::in, term::in,
    module_analysis_map(imdg_arc)::in, module_analysis_map(imdg_arc)::out,
    list(error_spec)::in, list(error_spec)::out) is det <= compiler(Compiler).

parse_imdg_arc(Compiler, VarSet, Term, !Arcs, !Specs) :-
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
            term_int.decimal_term_to_int(VersionNumberTerm, VersionNumber)
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
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected an imdb arc, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

write_module_imdg(Info, Globals, ModuleName, ModuleEntries, !IO) :-
    find_and_write_analysis_file(Info ^ compiler, Globals,
        do_not_add_dot_temp, write_imdg_arc(Info ^ compiler),
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_imdg), ModuleName,
        ModuleEntries, _FileName, !IO).

:- pred write_imdg_arc(Compiler::in, io.text_output_stream::in,
    analysis_name::in, func_id::in, imdg_arc::in, io::di, io::uo) is det
    <= compiler(Compiler).

write_imdg_arc(Compiler, OutStream, AnalysisName, FuncId, Arc, !IO) :-
    Arc = imdg_arc(Call, DependentModule),
    ( if
        analyses(Compiler, AnalysisName, Analysis),
        analysis_type(_ : unit(Call), _ : unit(Answer)) = Analysis
    then
        VersionNumber = analysis_version_number(_ : Call, _ : Answer)
    else
        unexpected($pred, "unknown analysis type")
    ),

    FuncIdStr = func_id_to_string(FuncId),
    DependentModuleStr = escaped_sym_name_to_string(DependentModule),
    CallTermStr = term_io.term_to_string(varset.init, to_term(Call)),
    io.format(OutStream, "'%s' -> %s(%i, %s, %s).\n",
        [s(DependentModuleStr), s(AnalysisName),
        i(VersionNumber), s(FuncIdStr), s(CallTermStr)], !IO).

%---------------------------------------------------------------------------%
%
% Common code for reading.
%

:- pred parse_func_id(term::in, func_id::out) is semidet.

parse_func_id(Term, FuncId) :-
    Term = term.functor(term.atom(PF), [NameTerm, ArityTerm, ProcTerm], _),
    (
        PF = "p",
        PredOrFunc = pf_predicate
    ;
        PF = "f",
        PredOrFunc = pf_function
    ),
    NameTerm = term.functor(term.atom(Name), [], _),
    term_int.decimal_term_to_int(ArityTerm, Arity),
    term_int.decimal_term_to_int(ProcTerm, ProcIdInt),
    proc_id_to_int(ProcId, ProcIdInt),
    FuncId = func_id(PredOrFunc, Name, pred_form_arity(Arity), ProcId).

%---------------------%

:- pred try_parse_module_name(term::in, module_name::out) is semidet.

try_parse_module_name(Term, ModuleName) :-
    try_parse_sym_name_and_no_args(Term, ModuleName).

%---------------------%

:- pred find_and_read_analysis_file(Compiler::in, globals::in,
    parse_entry(T)::in(parse_entry), ext::in, module_name::in,
    T::in, T::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det <= compiler(Compiler).

find_and_read_analysis_file(Compiler, Globals, ParseEntry,
        Ext, ModuleName, !ModuleResults, !Specs, !IO) :-
    module_name_to_read_file_name(Compiler, Globals,
        Ext, ModuleName, MaybeAnalysisFileName, !IO),
    (
        MaybeAnalysisFileName = ok(AnalysisFileName),
        read_analysis_file(AnalysisFileName, ParseEntry,
            !ModuleResults, !Specs, !IO)
    ;
        MaybeAnalysisFileName = error(Message),
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            ExtStr = extension_to_string(Globals, Ext),
            io.format(DebugStream,
                "Couldn't find %s file for module %s: %s\n",
                [s(ExtStr), s(sym_name_to_string(ModuleName)),
                s(Message)], !IO)
        )
        % XXX Why is not being able to find a file *less* fatal than
        % not being able to parse the contents of the file?
    ).

:- pred read_analysis_file(string::in, parse_entry(T)::in(parse_entry),
    T::in, T::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

read_analysis_file(AnalysisFileName, ParseEntry,
        !ModuleResults, !Specs, !IO) :-
    io.read_named_file_as_string(AnalysisFileName, FileResult, !IO),
    (
        FileResult = ok(FileStr),
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            io.format(DebugStream, "%% Reading analysis file %s\n",
                [s(AnalysisFileName)], !IO)
        ),

        string.length(FileStr, MaxOffset),
        LineContext0 = line_context(1, 0),
        LinePosn0 = line_posn(0),
        check_analysis_file_version_number(AnalysisFileName,
            FileStr, MaxOffset, LineContext0, LineContext1,
            LinePosn0, LinePosn1, !Specs),
        parse_analysis_file_entries(AnalysisFileName, FileStr, MaxOffset,
            ParseEntry, LineContext1, LinePosn1, !ModuleResults, !Specs)
    ;
        FileResult = error(_),
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            io.format(DebugStream, "Error reading analysis file: %s\n",
                [s(AnalysisFileName)], !IO)
        )
        % XXX Why is a failed open of an input file *less* fatal than
        % not being able to parse the contents of the file?
    ).

:- pred check_analysis_file_version_number(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_analysis_file_version_number(FileName, FileStr, MaxOffset,
        !LineContext, !LinePosn, !Specs) :-
    LineContext0 = !.LineContext,
    read_term_from_linestr(FileName, FileStr, MaxOffset,
        !LineContext, !LinePosn, TermResult : read_term),
    ( if
        TermResult = term(_VarSet, NumberTerm),
        term_int.decimal_term_to_int(NumberTerm, Number)
    then
        ( if Number = version_number then
            true
        else
            Pieces = [words("Error: version number mismatch."),
                words("Expected"), int_fixed(version_number), suffix(","),
                words("got"), int_fixed(Number), suffix("."), nl],
            LineContext0 = line_context(LineNumber, _),
            Context = context(FileName, LineNumber),
            Spec = simplest_spec($pred, severity_error, phase_read_files,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        Pieces = [words("Error: this file"),
            words("should start with a version number"),
            words("(specifically,"), int_fixed(version_number), suffix("),"),
            words("but it does not."), nl],
        LineContext0 = line_context(LineNumber, _),
        Context = context(FileName, LineNumber),
        Spec = simplest_spec($pred, severity_error, phase_read_files,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_analysis_file_entries(string::in, string::in, int::in,
    parse_entry(T)::in(parse_entry),
    line_context::in, line_posn::in,
    T::in, T::out, list(error_spec)::in, list(error_spec)::out) is det.

parse_analysis_file_entries(FileName, FileStr, MaxOffset, ParseEntry,
        !.LineContext, !.LinePosn, !Results, !Specs) :-
    LineContext0 = !.LineContext,
    read_term_from_linestr(FileName, FileStr, MaxOffset,
        !LineContext, !LinePosn, TermResult : read_term),
    (
        TermResult = term(VarSet, Term),
        ParseEntry(VarSet, Term, !Results, !Specs),
        parse_analysis_file_entries(FileName, FileStr, MaxOffset, ParseEntry,
            !.LineContext, !.LinePosn, !Results, !Specs)
    ;
        TermResult = eof
    ;
        TermResult = error(Msg, _),
        Pieces = [words(Msg), nl],
        LineContext0 = line_context(LineNumber, _),
        Context = context(FileName, LineNumber),
        Spec = simplest_spec($pred, severity_error, phase_read_files,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%
%
% Common code for reading.
%

:- func func_id_to_string(func_id) = string.

func_id_to_string(FuncId) = String :-
    FuncId = func_id(PredOrFunc, Name, PredFormArity, ProcId),
    (
        PredOrFunc = pf_predicate,
        PFStr = "p"
    ;
        PredOrFunc = pf_function,
        PFStr = "f"
    ),
    PredFormArity = pred_form_arity(Arity),
    NameStr = term_io.quoted_atom(Name),
    string.format("%s(%s, %d, %d)",
        [s(PFStr), s(NameStr), i(Arity), i(proc_id_to_int(ProcId))], String).

%---------------------%

:- type maybe_add_dot_temp
    --->    do_not_add_dot_temp
    ;       add_dot_temp.

:- pred find_and_write_analysis_file(Compiler::in, globals::in,
    maybe_add_dot_temp::in, write_entry(T)::in(write_entry),
    ext::in, module_name::in,
    module_analysis_map(T)::in, string::out, io::di, io::uo) is det
    <= compiler(Compiler).

find_and_write_analysis_file(Compiler, Globals, ToTmp, WriteEntry,
        Ext, ModuleName, ModuleResults, FileName, !IO) :-
    module_name_to_write_file_name(Compiler, Globals, Ext,
        ModuleName, FileName, !IO),
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
        OpenResult = ok(FileStream),
        io.format(FileStream, "%d.\n", [i(version_number)], !IO),
        write_module_analysis_map(FileStream, WriteEntry, ModuleResults, !IO),
        io.close_output(FileStream, !IO)
    ;
        OpenResult = error(IOError),
        string.format("error opening `%s' for output: %s\n",
            [s(FileName), s(io.error_message(IOError))], IOErrorMsg),
        unexpected($pred, IOErrorMsg)
    ).

:- pred write_module_analysis_map(io.text_output_stream::in,
    write_entry(T)::in(write_entry), module_analysis_map(T)::in,
    io::di, io::uo) is det.

write_module_analysis_map(OutStream, WriteEntry, ModuleResults, !IO) :-
    map.foldl(write_module_analysis_map_entry(OutStream, WriteEntry),
        ModuleResults, !IO).

:- pred write_module_analysis_map_entry(io.text_output_stream::in,
    write_entry(T)::in(write_entry), string::in, func_analysis_map(T)::in,
    io::di, io::uo) is det.

write_module_analysis_map_entry(OutStream, WriteEntry, AnalysisName,
        FuncResults, !IO) :-
    map.foldl(write_module_analysis_func(OutStream, WriteEntry, AnalysisName),
        FuncResults, !IO).

:- pred write_module_analysis_func(io.text_output_stream::in,
    write_entry(T)::in(write_entry), string::in, func_id::in, list(T)::in,
    io::di, io::uo) is det.

write_module_analysis_func(OutStream, WriteEntry, AnalysisName, FuncId,
        FuncResultList, !IO) :-
    list.sort(FuncResultList, FuncResultListSorted),
    list.foldl(
        ( pred(FuncResult::in, !.IO::di, !:IO::uo) is det :-
            WriteEntry(OutStream, AnalysisName, FuncId, FuncResult, !IO)
        ), FuncResultListSorted, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

empty_request_file(Info, Globals, ModuleName, !IO) :-
    module_name_to_write_file_name(Info ^ compiler, Globals,
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_request),
        ModuleName, RequestFileName, !IO),
    get_debug_analysis_stream(MaybeDebugStream, !IO),
    (
        MaybeDebugStream = no
    ;
        MaybeDebugStream = yes(DebugStream),
        io.format(DebugStream, "%% Removing request file %s\n",
            [s(RequestFileName)], !IO)
    ),
    io.file.remove_file(RequestFileName, _, !IO).

%---------------------------------------------------------------------------%
%
% Analysis file caching.
%
% An analysis cache file stores a binary representation of the parsed
% information in the corresponding .analysis file. In some cases,
% the binary format can be faster to read than the usual representation.
% The textual analysis files are portable, they are more stable (they don't
% depend on compiler internals) and are easier to debug, which is why
% we don't use binary files exclusively.
%

:- func make_cache_filename(string, string) = string.

make_cache_filename(Dir, FileName) = CacheFileName :-
    Components = string.split_at_separator(dir_sep, FileName),
    EscFileName = string.join_list(":", Components),
    CacheFileName = Dir / EscFileName.

:- pred dir_sep(char::in) is semidet.

dir_sep(Char) :-
    dir.is_directory_separator(Char).

:- pred maybe_write_analysis_cache_file(string::in,
    module_analysis_map(some_analysis_result)::in, list(error_spec)::in,
    io::di, io::uo) is det.

maybe_write_analysis_cache_file(CacheFileName, ModuleResults, Specs, !IO) :-
    (
        Specs = [],
        write_analysis_cache_file(CacheFileName, ModuleResults, !IO)
    ;
        Specs = [_ | _]
    ).

:- pred write_analysis_cache_file(string::in,
    module_analysis_map(some_analysis_result)::in, io::di, io::uo) is det.

write_analysis_cache_file(CacheFileName, ModuleResults, !IO) :-
    % Write to a temporary file first, and only move it into place
    % once it is complete.
    TmpFileName = CacheFileName ++ ".tmp",
    io.open_binary_output(TmpFileName, TmpFileResult, !IO),
    (
        TmpFileResult = ok(TmpFileStream),
        pickle(TmpFileStream, init_analysis_picklers, ModuleResults, !IO),
        io.close_binary_output(TmpFileStream, !IO),
        io.file.rename_file(TmpFileName, CacheFileName, RenameResult, !IO),
        (
            RenameResult = ok
        ;
            RenameResult = error(Error),
            % XXX Our caller should tell us what stream we should print
            % any error messages to.
            io.stderr_stream(StdErrStream, !IO),
            io.format(StdErrStream, "Error renaming %s: %s\n",
                [s(CacheFileName), s(io.error_message(Error))], !IO),
            io.file.remove_file(TmpFileName, _, !IO)
        )
    ;
        TmpFileResult = error(Error),
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

:- pred pickle_analysis_result(io.binary_output_stream::in, picklers::in,
    univ::in, io::di, io::uo) is det.

pickle_analysis_result(OutputStream, Pickles, Univ, !IO) :-
    det_univ_to_type(Univ, some_analysis_result(Call, Answer, Status)),
    Name = analysis_name(Call, Answer),
    pickle(OutputStream, Pickles, Name, !IO),
    pickle(OutputStream, Pickles, Call, !IO),
    pickle(OutputStream, Pickles, Answer, !IO),
    pickle(OutputStream, Pickles, Status, !IO).

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
        Term = term.functor(atom("dummy"), [], dummy_context)
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

%---------------------------------------------------------------------------%
:- end_module analysis.file.
%---------------------------------------------------------------------------%
