%-----------------------------------------------------------------------------%
% Copyright (C) 2003 University of Melbourne.
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

:- pred read_module_analysis_results(analysis_info::in,
	module_id::in, module_analysis_map(analysis_result)::out,
	io__state::di, io__state::uo) is det.

:- pred write_module_analysis_results(analysis_info::in,
	module_id::in, module_analysis_map(analysis_result)::in,
	io__state::di, io__state::uo) is det.

:- pred read_module_analysis_requests(analysis_info::in,
	module_id::in, module_analysis_map(analysis_request)::out,
	io__state::di, io__state::uo) is det.

:- pred write_module_analysis_requests(analysis_info::in,
	module_id::in, module_analysis_map(analysis_request)::in,
	io__state::di, io__state::uo) is det.

:- pred empty_request_file(analysis_info::in, module_id::in,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.
% The format of an analysis file is:
%
% version_number.
% analysis_name(analysis_version, func_id, call_pattern, answer_pattern).
%-----------------------------------------------------------------------------%
:- import_module bool, exception, parser, term, term_io, varset.

:- type invalid_analysis_file ---> invalid_analysis_file.

:- func version_number = int.
version_number = 1.

read_module_analysis_results(Info, ModuleId, ModuleResults, !IO) :-
	read_analysis_file(Info ^ compiler, ModuleId, ".analysis",
		parse_result_entry(Info ^ compiler),
		map__init, ModuleResults, !IO).

:- pred parse_result_entry(Compiler::in)
		`with_type` parse_entry(module_analysis_map(analysis_result))
		`with_inst` parse_entry <= compiler(Compiler).

parse_result_entry(Compiler, Term, Results0, Results) :-
    (	
	Term = term__functor(term__atom(AnalysisName),
			[VersionNumberTerm, FuncIdTerm,
			CallPatternTerm, AnswerPatternTerm], _),
	FuncIdTerm = term__functor(term__string(FuncId), [], _),
	CallPatternTerm = term__functor(
			term__string(CallPatternString), [], _),
	AnswerPatternTerm = term__functor(
			term__string(AnswerPatternString), [], _),
	analysis_type(_ `with_type` unit(FuncInfo), _ `with_type` unit(Call),
			_ `with_type` unit(Answer)) =
			analyses(Compiler, AnalysisName),

	CallPattern = from_string(CallPatternString) `with_type` Call,
	AnswerPattern = from_string(AnswerPatternString) `with_type` Answer
    ->
	(
		VersionNumber = analysis_version_number(
				_ `with_type` FuncInfo, _ `with_type` Call,
				_ `with_type` Answer),
		VersionNumberTerm = term__functor(
				term__integer(VersionNumber), [], _)
	->
		Result = 'new analysis_result'(
				unit1 `with_type` unit(FuncInfo),
				CallPattern, AnswerPattern),
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
	read_analysis_file(Info ^ compiler, ModuleId, ".request",
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
	analysis_type(_ `with_type` unit(FuncInfo),
		_ `with_type` unit(Call), _ `with_type` unit(Answer)) =
		analyses(Compiler, AnalysisName),
	CallPattern = from_string(CallPatternString) `with_type` Call
    ->
	(
		VersionNumber = analysis_version_number(
				_ `with_type` FuncInfo, _ `with_type` Call,
				_ `with_type` Answer),
		VersionNumberTerm = term__functor(
				term__integer(VersionNumber), [], _)
	->
		Result = 'new analysis_request'(
				unit1 `with_type` unit(FuncInfo),
				CallPattern),
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

:- type parse_entry(T) == pred(term, T, T).
:- inst parse_entry == (pred(in, in, out) is det).

:- pred read_analysis_file(Compiler::in, module_id::in, string::in,
		parse_entry(T)::in(parse_entry), T::in, T::out,
		io__state::di, io__state::uo) is det <= compiler(Compiler).

read_analysis_file(Compiler, ModuleId, Suffix, ParseEntry,
		ModuleResults0, ModuleResults, !IO) :-
	module_id_to_file_name(Compiler, ModuleId,
		Suffix, AnalysisFileName, !IO),
	io__open_input(AnalysisFileName, OpenResult, !IO),
	(
		OpenResult = ok(Stream),
		io__set_input_stream(Stream, OldStream, !IO),
		promise_only_solution_io(
		    (pred(R::out, di, uo) is cc_multi -->
			try_io((pred(Results1::out, di, uo) is det -->
			    read_analysis_file_2(ParseEntry,
			    		ModuleResults0, Results1)
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
		io__set_input_stream(OldStream, _, !IO),
		io__close_input(Stream, !IO)
	;
		OpenResult = error(_),
		ModuleResults = ModuleResults0
	).

:- pred read_analysis_file_2(parse_entry(T)::in(parse_entry),
	T::in, T::out, io__state::di, io__state::uo) is det.

read_analysis_file_2(ParseEntry, Results0, Results, !IO) :-
	parser__read_term(TermResult `with_type` read_term, !IO),
	(
		TermResult = term(_, term__functor(
				term__integer(version_number), [], _))
	->
		true
	;
		throw(invalid_analysis_file)
	),
	read_analysis_file_3(ParseEntry, Results0, Results, !IO).

:- pred read_analysis_file_3(parse_entry(T)::in(parse_entry),
	T::in, T::out, io__state::di, io__state::uo) is det.

read_analysis_file_3(ParseEntry, Results0, Results, !IO) :-
	parser__read_term(TermResult, !IO),
	(
		TermResult = term(_, Term) `with_type` read_term,
		ParseEntry(Term, Results0, Results)
	;
		TermResult = eof,
		Results = Results0
	;
		TermResult = error(_, _),
		throw(invalid_analysis_file)
	).

write_module_analysis_results(Info, ModuleId, ModuleResults, !IO) :-
	write_analysis_file(Info ^ compiler, ModuleId, ".analysis",
		write_result_entry, ModuleResults, !IO).

write_module_analysis_requests(Info, ModuleId, ModuleRequests, !IO) :-
	module_id_to_file_name(Info ^ compiler, ModuleId, ".request",
		AnalysisFileName, !IO),
	io__open_input(AnalysisFileName, InputResult, !IO),
	( InputResult = ok(InputStream) ->
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
					write_request_entry(Info ^ compiler),
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
		write_analysis_file(Info ^ compiler, ModuleId, ".request",
			write_request_entry(Info ^ compiler),
			ModuleRequests, !IO)
	;
		true
	).

:- pred write_result_entry `with_type` write_entry(analysis_result)
		`with_inst` write_entry.

write_result_entry(AnalysisName, FuncId,
		analysis_result(_ `with_type` unit(FuncInfo), Call, Answer),
		!IO) :-
	VersionNumber = analysis_version_number(_ `with_type` FuncInfo,
				Call, Answer), 
	term_io__write_term_nl(varset__init `with_type` varset,
		functor(atom(AnalysisName), [
			functor(integer(VersionNumber), [], context_init),
		    	functor(string(FuncId), [], context_init),
			functor(string(to_string(Call)), [], context_init),
			functor(string(to_string(Answer)), [], context_init)
		], context_init), !IO).

:- pred write_request_entry(Compiler::in)
		`with_type` write_entry(analysis_request)
		`with_inst` write_entry <= compiler(Compiler).

write_request_entry(Compiler, AnalysisName, FuncId,
		analysis_request(_, Call), !IO) :-
	(
		analysis_type(_ `with_type` unit(FuncInfo),
			_ `with_type` unit(Call),
			_ `with_type` unit(Answer)) =
			analyses(Compiler, AnalysisName)
	->
		VersionNumber = analysis_version_number(
			_ `with_type` FuncInfo, _ `with_type` Call,
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

:- type write_entry(T) == pred(analysis_name, func_id, T, io__state, io__state).
:- inst write_entry == (pred(in, in, in, di, uo) is det).

:- pred write_analysis_file(Compiler::in, module_id::in, string::in,
	write_entry(T)::in(write_entry), module_analysis_map(T)::in,
	io__state::di, io__state::uo) is det <= compiler(Compiler).

write_analysis_file(Compiler, ModuleId, Suffix,
		WriteEntry, ModuleResults, !IO) :-
	module_id_to_file_name(Compiler, ModuleId,
		Suffix, AnalysisFileName, !IO),
	io__open_output(AnalysisFileName, OpenResult, !IO),
	(
		OpenResult = ok(Stream),
		io__set_output_stream(Stream, OldOutput, !IO),
		io__write_int(version_number, !IO),
		io__write_string(".\n", !IO),
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
	module_id_to_file_name(Info ^ compiler, ModuleId, ".request", 
		RequestFileName, !IO),
	io__remove_file(RequestFileName, _, !IO).

