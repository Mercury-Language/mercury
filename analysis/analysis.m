%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: analysis.m
% Main author: stayl
%
% An inter-module analysis framework, as described in
%
%	Nicholas Nethercote. The Analysis Framework of HAL,
%	Chapter 7: Inter-module Analysis, Master's Thesis,
%	University of Melbourne, September 2001, revised April 2002.
%	<http://www.cl.cam.ac.uk/~njn25/pubs/masters2001.ps.gz>.
%
%-----------------------------------------------------------------------------%
:- module analysis.

:- interface.

:- import_module assoc_list, io, list, std_util.

	% The intention is that eventually any compiler can
	% use this library via .NET by defining an instance
	% of this class.
:- typeclass compiler(Compiler) where [
	func compiler_name(Compiler) = string,

	% Describe the analyses which can be performed by a compiler.
	func analyses(Compiler, analysis_name) = analysis_type is semidet,

	% module_id_to_file_name(Compiler, ModuleId, Ext, FileName)
	pred module_id_to_file_name(Compiler::in, module_id::in,
		string::in, string::out, io__state::di, io__state::uo) is det
].

:- type module_id == string.

:- type analysis_name == string.

:- type analysis_type
	---> some [FuncInfo, Call, Answer]
		analysis_type(unit(FuncInfo), unit(Call), unit(Answer))
		=> analysis(FuncInfo, Call, Answer).

	% An analysis is defined by a type describing call patterns,
	% a type defining answer patterns and a type giving information
	% about the function being analysed (e.g. arity) which should
	% be provided by the caller.
:- typeclass analysis(FuncInfo, Call, Answer) <=
		(call_pattern(FuncInfo, Call),
		answer_pattern(FuncInfo, Answer))
	where
[
	func analysis_name(FuncInfo::unused, Call::unused, Answer::unused) =
		(analysis_name::out) is det,

	% The version number should be changed when the Call or Answer
	% types are changed so that results which use the old types
	% can be discarded.
	func analysis_version_number(FuncInfo::unused, Call::unused,
		Answer::unused) = (int::out) is det,

	func preferred_fixpoint_type(FuncInfo::unused, Call::unused,
		Answer::unused) = (fixpoint_type::out) is det
].

:- type fixpoint_type
	--->
			% Start at `bottom'.
			% Must run to completion.
		least_fixpoint
	;
			% Start at `top'.
			% Can stop at any time.
		greatest_fixpoint.

:- typeclass call_pattern(FuncInfo, Call)
		<= (partial_order(FuncInfo, Call), to_string(Call)) where [].

:- typeclass answer_pattern(FuncInfo, Answer)
		<= (partial_order(FuncInfo, Answer), to_string(Answer)) where [
	func bottom(FuncInfo) = Answer,
	func top(FuncInfo) = Answer
].

:- typeclass partial_order(FuncInfo, Call) where [
	pred more_precise_than(FuncInfo::in, Call::in, Call::in) is semidet,
	pred equivalent(FuncInfo::in, Call::in, Call::in) is semidet
].

:- typeclass to_string(S) where [
	func to_string(S) = string,
	func from_string(string) = S is semidet
].

:- type any_call ---> any_call.
:- instance call_pattern(unit, any_call).
:- instance partial_order(unit, any_call).
:- instance to_string(any_call).

	% This will need to encode language specific details like
	% whether it is a predicate or a function, and the arity
	% and mode number.
:- type func_id == string.

	% Holds information used while analysing a module.
:- type analysis_info.

:- func init_analysis_info(Compiler) = analysis_info <= compiler(Compiler).

	% Look up all results for a given function.
:- pred lookup_results(module_id::in, func_id::in, FuncInfo::in,
	assoc_list(Call, Answer)::out, analysis_info::in, analysis_info::out,
	io__state::di, io__state::uo) is det
	<= analysis(FuncInfo, Call, Answer).

	% Look up the best result matching a given call.
:- pred lookup_best_result(module_id::in, func_id::in, FuncInfo::in,
	Call::in, maybe(pair(Call, Answer))::out, analysis_info::in,
	analysis_info::out, io__state::di, io__state::uo) is det
	<= analysis(FuncInfo, Call, Answer).

	% Record an analysis result for a (usually local) function.
:- pred record_result(module_id::in, func_id::in, FuncInfo::in, Call::in,
	Answer::in, analysis_info::in, analysis_info::out) is det
	<= analysis(FuncInfo, Call, Answer).

	% Lookup all the requests for a given (usually local) function.
:- pred lookup_requests(analysis_name::in, module_id::in, func_id::in,
	FuncInfo::in, list(Call)::out, analysis_info::in, analysis_info::out,
	io__state::di, io__state::uo) is det <= call_pattern(FuncInfo, Call).

	% Record a request for a local function.
:- pred record_request(analysis_name::in, module_id::in, func_id::in,
	FuncInfo::in, Call::in, analysis_info::in, analysis_info::out) is det
	<= call_pattern(FuncInfo, Call).

	% Should be called after all analysis is completed to write the
	% requests and results for the current compilation to the
	% analysis files.
:- pred write_analysis_files(module_id::in, analysis_info::in,
	io__state::di, io__state::uo) is det.

:- implementation.

:- include_module analysis__file.
:- import_module analysis__file.

:- import_module map, require, set.

:- type analysis_info
	---> some [Compiler] analysis_info(
		compiler :: Compiler,
		analysis_requests :: analysis_map(analysis_request),
		analysis_results :: analysis_map(analysis_result)
	) => compiler(Compiler).

:- type analysis_result
	--->	some [FuncInfo, Call, Answer] analysis_result(unit(FuncInfo),
			Call, Answer) => analysis(FuncInfo, Call, Answer).

:- type analysis_request
	---> some [FuncInfo, Call] analysis_request(unit(FuncInfo), Call)
			=> call_pattern(FuncInfo, Call).

:- type analysis_hash == int.

:- type analysis_map(T) == map(module_id, module_analysis_map(T)).
:- type module_analysis_map(T) == map(analysis_name, func_analysis_map(T)).
:- type func_analysis_map(T) == map(func_id, list(T)).

:- instance call_pattern(unit, any_call) where [].
:- instance partial_order(unit, any_call) where [
	more_precise_than(_, _, _) :- semidet_fail,
	equivalent(_, _, _) :- semidet_succeed
].
:- instance to_string(any_call) where [
	to_string(any_call) = "",
	from_string("") = any_call
].

init_analysis_info(Compiler) =
	'new analysis_info'(Compiler, map__init, map__init).

lookup_results(ModuleId, FuncId, _FuncInfo, ResultList, !Info, !IO) :-
	%io__write_string("looking up results for ", !IO),
	%io__write_string(FuncId, !IO),
	%io__nl(!IO),
	( map__search(!.Info ^ analysis_results, ModuleId, ModuleResults0) ->
		ModuleResults = ModuleResults0
	;
		read_module_analysis_results(!.Info, ModuleId,
			ModuleResults, !IO),
		!:Info = !.Info ^ analysis_results
				^ elem(ModuleId) := ModuleResults
	),
	AnalysisName = analysis_name(_ `with_type` FuncInfo,
				_ `with_type` Call, _ `with_type` Answer),
	(
		Results = ModuleResults ^ elem(AnalysisName) ^ elem(FuncId)
	->
		ResultList = list__map(
		    (func(Result) = ResultCall - ResultAnswer :-
			Result = analysis_result(_,
					ResultCall0, ResultAnswer0),
			det_univ_to_type(univ(ResultCall0), ResultCall),
		    	det_univ_to_type(univ(ResultAnswer0), ResultAnswer)
		    ), Results)
	;
		ResultList = []
	).

lookup_best_result(ModuleId, FuncId, FuncInfo, Call, MaybeBestResult,
		!Info, !IO) :-
	%io__write_string("looking up best result for ", !IO),
	%io__write_string(FuncId, !IO),
	%io__nl(!IO),
	lookup_results(ModuleId, FuncId, FuncInfo, ResultList, !Info, !IO),
	MatchingResults = list__filter(
		(pred((ResultCall - _)::in) is semidet :-
			( more_precise_than(FuncInfo, Call, ResultCall)
			; equivalent(FuncInfo, Call, ResultCall)
			)
		), ResultList),
	(
		MatchingResults = [],
		MaybeBestResult = no
	;
		MatchingResults = [FirstResult | MatchingResults1],
		MaybeBestResult = yes(list__foldl(
		    (func(ThisResult, BestResult) =
			(
				more_precise_than(FuncInfo,
					snd(ThisResult), snd(BestResult))
			->
				ThisResult
			;
				BestResult
			)
		    ), MatchingResults1, FirstResult))
	).

record_result(ModuleId, FuncId, FuncInfo, CallPattern, AnswerPattern, !Info) :-
	( ModuleResults0 = map__search(!.Info ^ analysis_results, ModuleId) ->
		ModuleResults1 = ModuleResults0
	;
		ModuleResults1 = map__init
	),
	AnalysisName = analysis_name(FuncInfo, CallPattern, AnswerPattern),
	( AnalysisResults0 = map__search(ModuleResults1, AnalysisName) ->
		AnalysisResults1 = AnalysisResults0
	;
		AnalysisResults1 = map__init
	),
	( FuncResults0 = map__search(AnalysisResults1, FuncId) ->
		FuncResults1 = FuncResults0
	;
		FuncResults1 = []
	),
	!:Info = !.Info ^ analysis_results :=
		map__set(!.Info ^ analysis_results, ModuleId,
		map__set(ModuleResults1, AnalysisName,
		map__set(AnalysisResults1, FuncId,
		['new analysis_result'(unit1 `with_type` unit(FuncInfo),
			CallPattern, AnswerPattern) | FuncResults1]))).

lookup_requests(AnalysisName, ModuleId, FuncId, _FuncInfo,
		CallPatterns, !Info, !IO) :-
	( map__search(!.Info ^ analysis_requests, ModuleId, ModuleRequests0) ->
		ModuleRequests = ModuleRequests0
	;
		read_module_analysis_requests(!.Info,
			ModuleId, ModuleRequests, !IO),
		!:Info = !.Info ^ analysis_requests
				^ elem(ModuleId) := ModuleRequests
	),
	( CallPatterns0 = ModuleRequests ^ elem(AnalysisName) ^ elem(FuncId) ->
		CallPatterns = list__filter_map(
		    (func(Call0) = Call is semidet :-
			univ(Call) = univ(Call0)
		    ), CallPatterns0)
	;
		CallPatterns = []
	).

record_request(AnalysisName, ModuleId, FuncId, _FuncInfo,
		CallPattern, !Info) :-
	( ModuleResults0 = map__search(!.Info ^ analysis_requests, ModuleId) ->
		ModuleResults1 = ModuleResults0
	;
		ModuleResults1 = map__init
	),
	( AnalysisResults0 = map__search(ModuleResults1, AnalysisName) ->
		AnalysisResults1 = AnalysisResults0
	;
		AnalysisResults1 = map__init
	),
	( FuncResults0 = map__search(AnalysisResults1, FuncId) ->
		FuncResults1 = FuncResults0
	;
		FuncResults1 = []
	),
	!:Info = !.Info ^ analysis_requests :=
		map__set(!.Info ^ analysis_requests, ModuleId,
		map__set(ModuleResults1, AnalysisName,
		map__set(AnalysisResults1, FuncId,
		['new analysis_request'(unit1 `with_type` unit(FuncInfo),
			CallPattern) | FuncResults1]))).

write_analysis_files(ModuleId, Info, !IO) :-

	%
	% Write the results for the current module.
	%
	( ModuleResults0 = map__search(Info ^ analysis_results, ModuleId) ->
		ModuleResults = ModuleResults0
	;
		ModuleResults = map__init
	),
	write_module_analysis_results(Info, ModuleId, ModuleResults, !IO),

	%
	% Write the requests for the imported modules.
	%
	map__foldl(write_module_analysis_requests(Info),
		Info ^ analysis_requests, !IO),

	empty_request_file(Info, ModuleId, !IO).
