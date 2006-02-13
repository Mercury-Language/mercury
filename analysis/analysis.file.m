%-----------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: analysis.file.m
% Main author: stayl
%
% An analysis file contains analysis results for a single module.
%-----------------------------------------------------------------------------%

:- module analysis__file.

:- interface.

	% read_module_overall_status(Compiler, ModuleId, MaybeModuleStatus,
	%   !IO)
	% Attempt to read the overall status from a module `.analysis' file.
	%
:- pred read_module_overall_status(Compiler::in, module_id::in,
	maybe(analysis_status)::out, io::di, io::uo) is det
	<= compiler(Compiler).

:- pred read_module_analysis_results(analysis_info::in, module_id::in,
	analysis_status::out, module_analysis_map(analysis_result)::out,
	io__state::di, io__state::uo) is det.

:- pred write_module_analysis_results(analysis_info::in,
	module_id::in, analysis_status::in,
	module_analysis_map(analysis_result)::in,
	io__state::di, io__state::uo) is det.

:- pred read_module_analysis_requests(analysis_info::in,
	module_id::in, module_analysis_map(analysis_request)::out,
	io__state::di, io__state::uo) is det.

:- pred write_module_analysis_requests(analysis_info::in,
	module_id::in, module_analysis_map(analysis_request)::in,
	io__state::di, io__state::uo) is det.

:- pred read_module_imdg(analysis_info::in, module_id::in,
	module_analysis_map(imdg_arc)::out, io::di, io::uo) is det.

:- pred write_module_imdg(analysis_info::in, module_id::in,
	module_analysis_map(imdg_arc)::in, io::di, io::uo) is det.

:- pred empty_request_file(analysis_info::in, module_id::in,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

% The format of an analysis result file is:
%
% version_number.
% module_status.
% analysis_name(analysis_version, func_id, call_pattern, answer_pattern,
%   result_status).

% The format of an IMDG file is:
%
% version_number.
% calling_module -> analysis_name(analysis_version, func_id, call_pattern).

% The format of an analysis request file is:
%
% version_number.
% analysis_name(analysis_version, func_id, call_pattern).

:- import_module bool, exception, parser, term, term_io, varset.

:- type invalid_analysis_file ---> invalid_analysis_file.

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
	read_module_overall_status_2(AnalysisFileName, MaybeModuleStatus, !IO)
    ;
	MaybeAnalysisFileName = error(_),
	MaybeModuleStatus = no
    ).

:- pred read_module_overall_status_2(string::in, maybe(analysis_status)::out,
    io::di, io::uo) is det.

read_module_overall_status_2(AnalysisFileName, MaybeModuleStatus, !IO) :-
    io__open_input(AnalysisFileName, OpenResult, !IO),
    (
	OpenResult = ok(Stream),
	io__set_input_stream(Stream, OldStream, !IO),

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
	io__set_input_stream(OldStream, _, !IO),
	io__close_input(Stream, !IO)
    ;
	OpenResult = error(_),
	MaybeModuleStatus = no
    ).

read_module_analysis_results(Info, ModuleId, ModuleStatus, ModuleResults,
		!IO) :-
	% If the module's overall status is `invalid' then at least one of its
	% results is invalid.  However, we can't just discard the results as we
	% want to know which results change after we reanalyse the module.
	Compiler = Info ^ compiler,
	read_analysis_file(Compiler, ModuleId, analysis_registry_suffix,
		read_module_status, optimal, ModuleStatus,
		parse_result_entry(Compiler),
		map__init, ModuleResults, !IO).

:- pred read_module_status(analysis_status::out, io::di, io::uo) is det.

read_module_status(Status, !IO) :-
	parser__read_term(TermResult `with_type` read_term, !IO),
	( TermResult = term(_, term__functor(term__atom(String), [], _)) ->
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

:- pred parse_result_entry(Compiler::in)
		`with_type` parse_entry(module_analysis_map(analysis_result))
		`with_inst` parse_entry <= compiler(Compiler).

parse_result_entry(Compiler, Term, Results0, Results) :-
    (	
	Term = term__functor(term__atom(AnalysisName),
			[VersionNumberTerm, FuncIdTerm,
			CallPatternTerm, AnswerPatternTerm, StatusTerm], _),
	FuncIdTerm = term__functor(term__string(FuncId), [], _),
	CallPatternTerm = term__functor(
			term__string(CallPatternString), [], _),
	AnswerPatternTerm = term__functor(
			term__string(AnswerPatternString), [], _),
	StatusTerm = term__functor(term__string(StatusString), [], _),
	analysis_type(_ `with_type` unit(Call),
			_ `with_type` unit(Answer)) =
			analyses(Compiler, AnalysisName),

	CallPattern = from_string(CallPatternString) `with_type` Call,
	AnswerPattern = from_string(AnswerPatternString) `with_type` Answer,
	analysis_status_to_string(Status, StatusString)
    ->
	(
		VersionNumber = analysis_version_number(
				_ `with_type` Call,
				_ `with_type` Answer),
		VersionNumberTerm = term__functor(
				term__integer(VersionNumber), [], _)
	->
		Result = 'new analysis_result'(CallPattern, AnswerPattern,
			Status),
		( AnalysisResults0 = map__search(Results0, AnalysisName) ->
			AnalysisResults1 = AnalysisResults0
		;
			AnalysisResults1 = map__init
		),
		(
			FuncResults0 = map__search(AnalysisResults1,
				FuncId)
		->
			FuncResults = [Result | FuncResults0]
		;
			FuncResults = [Result]
		),
		Results = map__set(Results0, AnalysisName, 
			map__set(AnalysisResults1,
				FuncId, FuncResults))
    	;
		% Ignore results with an out-of-date version number.
		Results = Results0
	)
    ;
	throw(invalid_analysis_file)
    ).

read_module_analysis_requests(Info, ModuleId, ModuleRequests, !IO) :-
	read_analysis_file(Info ^ compiler, ModuleId, request_suffix,
		nop, unit, _NoHeader,
		parse_request_entry(Info ^ compiler),
		map__init, ModuleRequests, !IO).

:- pred parse_request_entry(Compiler::in)
		`with_type` parse_entry(module_analysis_map(analysis_request))
		`with_inst` parse_entry <= compiler(Compiler).

parse_request_entry(Compiler, Term, Requests0, Requests) :-
    (	
	Term = term__functor(term__atom(AnalysisName),
			[VersionNumberTerm, FuncIdTerm, CallPatternTerm], _),
	FuncIdTerm = term__functor(term__string(FuncId), [], _),
	CallPatternTerm = term__functor(
		term__string(CallPatternString), [], _),
	analysis_type(_ `with_type` unit(Call), _ `with_type` unit(Answer)) =
		analyses(Compiler, AnalysisName),
	CallPattern = from_string(CallPatternString) `with_type` Call
    ->
	(
		VersionNumber = analysis_version_number(
				_ `with_type` Call,
				_ `with_type` Answer),
		VersionNumberTerm = term__functor(
				term__integer(VersionNumber), [], _)
	->
		Result = 'new analysis_request'(CallPattern),
		(
			AnalysisRequests0 = map__search(Requests0,
				AnalysisName)
		->
			AnalysisRequests1 = AnalysisRequests0
		;
			AnalysisRequests1 = map__init
		),
		(
			FuncRequests0 = map__search(AnalysisRequests1,
				FuncId)
		->
			FuncRequests = [Result | FuncRequests0]
		;
			FuncRequests = [Result]
		),
		Requests = map__set(Requests0, AnalysisName, 
			map__set(AnalysisRequests1,
				FuncId, FuncRequests))
    	;
		% Ignore requests with an out-of-date version number.
		Requests = Requests0
	)
    ;
	throw(invalid_analysis_file)
    ).

read_module_imdg(Info, ModuleId, ModuleEntries, !IO) :-
    read_analysis_file(Info ^ compiler, ModuleId, imdg_suffix,
	nop, unit, _NoHeader,
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
	    %	   do we really need a version number for the IMDG?
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
		read_analysis_header(Header)::in(read_analysis_header),
		Header::in, Header::out,
		parse_entry(T)::in(parse_entry), T::in, T::out,
		io__state::di, io__state::uo) is det <= compiler(Compiler).

read_analysis_file(Compiler, ModuleId, Suffix,
		ReadHeader, DefaultHeader, Header,
		ParseEntry, ModuleResults0, ModuleResults, !IO) :-
	module_id_to_read_file_name(Compiler, ModuleId,
		Suffix, MaybeAnalysisFileName, !IO),
	(
		MaybeAnalysisFileName = ok(AnalysisFileName),
		read_analysis_file(AnalysisFileName,
			ReadHeader, DefaultHeader, Header,
			ParseEntry, ModuleResults0, ModuleResults, !IO)
	;
		MaybeAnalysisFileName = error(_),
		Header = DefaultHeader,
		ModuleResults = ModuleResults0
	).

:- pred read_analysis_file(string::in,
		read_analysis_header(Header)::in(read_analysis_header),
		Header::in, Header::out,
		parse_entry(T)::in(parse_entry), T::in, T::out,
		io__state::di, io__state::uo) is det.

read_analysis_file(AnalysisFileName,
		ReadHeader, DefaultHeader, Header,
		ParseEntry, ModuleResults0, ModuleResults, !IO) :-
	io__open_input(AnalysisFileName, OpenResult, !IO),
	(
		OpenResult = ok(Stream),
		debug_msg((pred(!.IO::di, !:IO::uo) is det :-
			io.print("Reading analysis file ", !IO),
			io.print(AnalysisFileName, !IO),
			io.nl(!IO)
		), !IO),
		io__set_input_stream(Stream, OldStream, !IO),

		promise_only_solution_io(
		    (pred(HR::out, di, uo) is cc_multi -->
			try_io((pred({Header1, Results1}::out, di, uo)
				    is det -->
			    check_analysis_file_version_number,
			    ReadHeader(Header1),
			    read_analysis_file_2(ParseEntry,
			    		ModuleResults0, Results1)
			), HR)
		    ), Result, !IO),
		(
			Result = succeeded({Header, ModuleResults})
		;
			Result = failed,
			Header = DefaultHeader,
			ModuleResults = ModuleResults0
		;
			Result = exception(_),
			% XXX Report error.
			Header = DefaultHeader,
			ModuleResults = ModuleResults0
		),
		io__set_input_stream(OldStream, _, !IO),
		io__close_input(Stream, !IO)
	;
		OpenResult = error(_),
		debug_msg((pred(!.IO::di, !:IO::uo) is det :-
			io.print("Error reading analysis file: ", !IO),
			io.print(AnalysisFileName, !IO),
			io.nl(!IO)
		), !IO),
		Header = DefaultHeader,
		ModuleResults = ModuleResults0
	).

:- pred check_analysis_file_version_number(io::di, io::uo) is det.

check_analysis_file_version_number(!IO) :-
	parser__read_term(TermResult `with_type` read_term, !IO),
	(
		TermResult = term(_, term__functor(
				term__integer(version_number), [], _))
	->
		true
	;
		throw(invalid_analysis_file)
	).

:- pred read_analysis_file_2(parse_entry(T)::in(parse_entry),
	T::in, T::out, io__state::di, io__state::uo) is det.

read_analysis_file_2(ParseEntry, Results0, Results, !IO) :-
	parser__read_term(TermResult, !IO),
	(
		TermResult = term(_, Term) `with_type` read_term,
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

write_module_analysis_results(Info, ModuleId, ModuleStatus, ModuleResults,
		!IO) :-
	debug_msg((pred(!.IO::di, !:IO::uo) is det :-
		io.print("Writing module analysis results for ", !IO),
		io.print(ModuleId, !IO),
		io.nl(!IO)
	), !IO),
	WriteHeader = write_module_status(ModuleStatus),
	write_analysis_file(Info ^ compiler,
		ModuleId, analysis_registry_suffix,
		WriteHeader, write_result_entry, ModuleResults, !IO).

:- pred write_module_status(analysis_status::in, io::di, io::uo) is det.

write_module_status(Status, !IO) :-
    term_io.write_term_nl(init:varset, Term, !IO),
    Term = functor(atom(String), [], context_init),
    analysis_status_to_string(Status, String).

write_module_analysis_requests(Info, ModuleId, ModuleRequests, !IO) :-
	Compiler = Info ^ compiler,
	module_id_to_write_file_name(Compiler, ModuleId, request_suffix,
		AnalysisFileName, !IO),
	debug_msg((pred(!.IO::di, !:IO::uo) is det :-
		io.print("Writing module analysis requests to ", !IO),
		io.print(AnalysisFileName, !IO),
		io.nl(!IO)
	), !IO),
	io__open_input(AnalysisFileName, InputResult, !IO),
	( InputResult = ok(InputStream) ->
		%
		% Request file already exists.  Check it has the right version
		% number, then append the new requests to the end.
		%
		io__set_input_stream(InputStream, OldInputStream, !IO),
		parser__read_term(VersionResult `with_type` read_term, !IO),
		io__set_input_stream(OldInputStream, _, !IO),
		io__close_input(InputStream, !IO),
		(
			VersionResult = term(_, term__functor(
				term__integer(version_number), [], _))
		->
			io__open_append(AnalysisFileName, AppendResult, !IO),
			( AppendResult = ok(AppendStream) ->
				io__set_output_stream(AppendStream,
					OldOutputStream, !IO),
				write_analysis_entries(
					write_request_entry(Compiler),
					ModuleRequests, !IO),
				io__set_output_stream(OldOutputStream, _, !IO),
				io__close_output(AppendStream, !IO),
				Appended = yes
			;
				Appended = no
			)
		;
			Appended = no
		)
	;
		Appended = no
	),
	( Appended = no ->
		write_analysis_file(AnalysisFileName,
			nop, write_request_entry(Compiler),
			ModuleRequests, !IO)
	;
		true
	).

:- pred write_result_entry `with_type` write_entry(analysis_result)
		`with_inst` write_entry.

write_result_entry(AnalysisName, FuncId, Result, !IO) :-
	Result = analysis_result(Call, Answer, Status),
	VersionNumber = analysis_version_number(Call, Answer), 
	analysis_status_to_string(Status, StatusString),
	term_io__write_term_nl(varset__init `with_type` varset,
		functor(atom(AnalysisName), [
			functor(integer(VersionNumber), [], context_init),
		    	functor(string(FuncId), [], context_init),
			functor(string(to_string(Call)), [], context_init),
			functor(string(to_string(Answer)), [], context_init),
			functor(string(StatusString), [], context_init)
		], context_init), !IO).

:- pred write_request_entry(Compiler::in)
		`with_type` write_entry(analysis_request)
		`with_inst` write_entry <= compiler(Compiler).

write_request_entry(Compiler, AnalysisName, FuncId,
		analysis_request(Call), !IO) :-
	(
		analysis_type(
			_ `with_type` unit(Call),
			_ `with_type` unit(Answer)) =
			analyses(Compiler, AnalysisName)
	->
		VersionNumber = analysis_version_number(
			_ `with_type` Call,
			_ `with_type`  Answer)
	;
		error("write_request_entry: unknown analysis type")

	),
	term_io__write_term_nl(varset__init `with_type` varset,
		functor(atom(AnalysisName), [
			functor(integer(VersionNumber), [], context_init),
		    	functor(string(FuncId), [], context_init),
			functor(string(to_string(Call)), [], context_init)
		], context_init), !IO).

write_module_imdg(Info, ModuleId, ModuleEntries, !IO) :-
    write_analysis_file(Info ^ compiler, ModuleId, imdg_suffix,
	nop, write_imdg_arc(Info ^ compiler), ModuleEntries, !IO).

:- pred write_imdg_arc(Compiler::in)
	    `with_type` write_entry(imdg_arc)
	    `with_inst` write_entry <= compiler(Compiler).

write_imdg_arc(Compiler, AnalysisName, FuncId,
	imdg_arc(Call, DependentModule), !IO) :-
    (
	analysis_type(_ : unit(Call), _ : unit(Answer))
	    = analyses(Compiler, AnalysisName)
    ->
	VersionNumber = analysis_version_number(_ : Call, _ : Answer)
    ;
	error("write_imdg_arc: unknown analysis type")
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

:- type write_entry(T) == pred(analysis_name, func_id, T, io__state, io__state).
:- inst write_entry == (pred(in, in, in, di, uo) is det).

:- pred write_analysis_file(Compiler::in, module_id::in, string::in,
	write_header::in(write_header),
	write_entry(T)::in(write_entry), module_analysis_map(T)::in,
	io__state::di, io__state::uo) is det <= compiler(Compiler).

write_analysis_file(Compiler, ModuleId, Suffix, WriteHeader, WriteEntry,
		ModuleResults, !IO) :-
	module_id_to_write_file_name(Compiler, ModuleId, Suffix,
		AnalysisFileName, !IO),
	write_analysis_file(AnalysisFileName, WriteHeader, WriteEntry,
		ModuleResults, !IO).
    
:- pred write_analysis_file(string::in, write_header::in(write_header),
	write_entry(T)::in(write_entry), module_analysis_map(T)::in,
	io__state::di, io__state::uo) is det.

write_analysis_file(AnalysisFileName, WriteHeader, WriteEntry,
		ModuleResults, !IO) :-
	io__open_output(AnalysisFileName, OpenResult, !IO),
	(
		OpenResult = ok(Stream),
		io__set_output_stream(Stream, OldOutput, !IO),
		io__write_int(version_number, !IO),
		io__write_string(".\n", !IO),
		WriteHeader(!IO),
		write_analysis_entries(WriteEntry, ModuleResults, !IO),
		io__set_output_stream(OldOutput, _, !IO),
		io__close_output(Stream, !IO)
	;
		OpenResult = error(Msg),
		io__write_string("Error opening ", !IO),
		io__write_string(AnalysisFileName, !IO),
		io__write_string(" for output: ", !IO),
		io__write_string(io__error_message(Msg), !IO),
		io__nl(!IO)
	).

:- pred write_analysis_entries(write_entry(T)::in(write_entry),
	module_analysis_map(T)::in, io__state::di, io__state::uo) is det.

write_analysis_entries(WriteEntry, ModuleResults, !IO) :-
	map__foldl(
	    (pred(AnalysisName::in, FuncResults::in, di, uo) is det -->
		map__foldl(
		    (pred(FuncId::in, FuncResultList::in, di, uo) is det -->
			list__foldl(
			    (pred(FuncResult::in, di, uo) is det -->
				WriteEntry(AnalysisName, FuncId, FuncResult)
		    	    ), FuncResultList)
		    ), FuncResults)
	    ), ModuleResults, !IO).

empty_request_file(Info, ModuleId, !IO) :-
	module_id_to_write_file_name(Info ^ compiler, ModuleId, request_suffix,
		RequestFileName, !IO),
	debug_msg((pred(!.IO::di, !:IO::uo) is det :-
		io.print("Removing request file ", !IO),
		io.print(RequestFileName, !IO),
		io.nl(!IO)
	), !IO),
	io__remove_file(RequestFileName, _, !IO).

:- pred nop(io::di, io::uo) is det.

nop(!IO).

:- pred nop(unit::out, io::di, io::uo) is det.

nop(unit, !IO).
