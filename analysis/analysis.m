%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=8 noet
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: analysis.m
% Main authors: stayl, wangp
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

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module set.
:- import_module std_util.

	% The intention is that eventually any compiler can
	% use this library via .NET by defining an instance
	% of this class.
:- typeclass compiler(Compiler) where [
	func compiler_name(Compiler) = string,

	% Describe the analyses which can be performed by a compiler.
	func analyses(Compiler, analysis_name) = analysis_type is semidet,

	% module_id_to_read_file_name(Compiler, ModuleId, Ext, FileName)
	pred module_id_to_read_file_name(Compiler::in, module_id::in,
		string::in, maybe_error(string)::out, io::di, io::uo) is det,

	% module_id_to_write_file_name(Compiler, ModuleId, Ext, FileName)
	pred module_id_to_write_file_name(Compiler::in, module_id::in,
		string::in, string::out, io::di, io::uo) is det,

	% module_is_local(Compiler, ModuleId, IsLocal, !IO)
	%
	% IsLocal is `yes' if the module is not a "library" module, i.e. we are
	% able to reanalyse the module, not just use results that already
	% exist.
	% 
	pred module_is_local(Compiler::in, module_id::in, bool::out,
	    io::di, io::uo) is det
].

:- type module_id == string.

:- type analysis_name == string.

:- type analysis_type
	--->	some [Call, Answer]
		analysis_type(unit(Call), unit(Answer))
		=> analysis(Call, Answer).

	% An analysis is defined by a type describing call patterns and
	% a type defining answer patterns.  If the analysis needs to store
	% more information about the function being analysed (e.g. arity)
	% it should be stored as part of the type for call patterns.
	%
:- typeclass analysis(Call, Answer) <=
		(call_pattern(Call),
		answer_pattern(Answer))
	where
[
	func analysis_name(Call::unused, Answer::unused) =
		(analysis_name::out) is det,

	% The version number should be changed when the Call or Answer
	% types are changed so that results which use the old types
	% can be discarded.
	func analysis_version_number(Call::unused,
		Answer::unused) = (int::out) is det,

	func preferred_fixpoint_type(Call::unused,
		Answer::unused) = (fixpoint_type::out) is det,

	% `top' and `bottom' should not really depend on the call pattern.
	% However some analyses may choose to store extra information about
	% the function in their `Call' types that might be needed for the
	% answer pattern.
	%
 	func bottom(Call) = Answer,
 	func top(Call) = Answer
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

:- typeclass call_pattern(Call)
		<= (partial_order(Call), to_string(Call)) where [].

:- typeclass answer_pattern(Answer)
		<= (partial_order(Answer), to_string(Answer)) where [].

:- typeclass partial_order(T) where [
	pred more_precise_than(T::in, T::in) is semidet,
	pred equivalent(T::in, T::in) is semidet
].

:- typeclass to_string(S) where [
	func to_string(S) = string,
	func from_string(string) = S is semidet
].

	% A call pattern that can be used by analyses that do not need
	% finer granularity.
	%
:- type any_call ---> any_call.
:- instance call_pattern(any_call).
:- instance partial_order(any_call).
:- instance to_string(any_call).

	% The status of a module or a specific analysis result.
	%
:- type analysis_status
	--->	invalid
	;	suboptimal
	;	optimal.

	% Least upper bound of two analysis_status values.
	%
:- func lub(analysis_status, analysis_status) = analysis_status.

	% This will need to encode language specific details like
	% whether it is a predicate or a function, and the arity
	% and mode number.
:- type func_id == string.

	% Holds information used while analysing a module.
:- type analysis_info.

:- func init_analysis_info(Compiler) = analysis_info <= compiler(Compiler).

	% Look up all results for a given function.
	%
	% N.B. Newly recorded results will NOT be found.  This
	% is intended for looking up results from _other_ modules.
	%
:- pred lookup_results(module_id::in, func_id::in,
	list({Call, Answer, analysis_status})::out, 
	analysis_info::in, analysis_info::out, io::di, io::uo) is det
	<= analysis(Call, Answer).

	% Look up all results for a given function and call pattern CP such
	% that the results have call patterns CP' that are equivalent
	% to CP or less specific than CP.
	%
	% N.B. Newly recorded results will NOT be found.  This
	% is intended for looking up results from _other_ modules.
	%
:- pred lookup_matching_results(module_id::in, func_id::in, Call::in,
	list({Call, Answer, analysis_status})::out, 
	analysis_info::in, analysis_info::out, io::di, io::uo) is det
	<= analysis(Call, Answer).

	% Look up the best result matching a given call.
	%
	% N.B. Newly recorded results will NOT be found.  This
	% is intended for looking up results from _other_ modules.
	%
:- pred lookup_best_result(module_id::in, func_id::in, Call::in,
	maybe({Call, Answer, analysis_status})::out,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det
	<= analysis(Call, Answer).

	% Record an analysis result for a (usually local) function.
	% XXX at the moment the result is assumed to be for a function local to
	% the currently-compiled module and things will probably break if it
	% isn't.
	%
:- pred record_result(module_id::in, func_id::in, Call::in,
	Answer::in, analysis_status::in,
	analysis_info::in, analysis_info::out) is det
	<= analysis(Call, Answer).

	% Record the dependency of a module on the analysis result of another
	% module.
	%
:- pred record_dependency(module_id::in, analysis_name::in, module_id::in,
	func_id::in, Call::in, analysis_info::in, analysis_info::out) is det
	<= call_pattern(Call).

	% Lookup all the requests for a given (usually local) function.
:- pred lookup_requests(analysis_name::in, module_id::in, func_id::in,
	list(Call)::out, analysis_info::in, analysis_info::out,
	io::di, io::uo) is det <= call_pattern(Call).

	% Record a request for a local function.
:- pred record_request(analysis_name::in, module_id::in, func_id::in,
	Call::in, analysis_info::in, analysis_info::out) is det
	<= call_pattern(Call).

	% Should be called after all analysis is completed to write the
	% requests and results for the current compilation to the
	% analysis files.
	%
:- pred write_analysis_files(Compiler::in, module_id::in, set(module_id)::in, 
	analysis_info::in, analysis_info::out, io::di, io::uo) is det
	<= compiler(Compiler).

	% read_module_overall_status(Compiler, ModuleId, MaybeModuleStatus,
	%   !IO)
	% Attempt to read the overall status from a module `.analysis' file.
	%
:- pred read_module_overall_status(Compiler::in, module_id::in,
	maybe(analysis_status)::out, io::di, io::uo) is det
	<= compiler(Compiler).

:- pred enable_debug_messages(bool::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module analysis__file.
:- import_module analysis__file.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

:- type analysis_info
	---> some [Compiler] analysis_info(
		compiler :: Compiler,

			% Holds outstanding requests for more specialised
			% variants of procedures.  Requests are added to this
			% map as analyses proceed and written out to disk
			% at the end of the compilation of this module.
			%
		analysis_requests :: analysis_map(analysis_request),

			% The overall status of each module.
			%
		module_statuses	:: map(module_id, analysis_status),

			% The "old" map stores analysis results read in from
			% disk.  New results generated while analysing the
			% current module are added to the "new" map.  After
			% all the analyses the two maps are compared to
			% see which analysis results have changed.  Other
			% modules may need to be marked or invalidated as a
			% result.  Then "new" results are moved into the "old"
			% map, from where they can be written to disk.
			%
		old_analysis_results :: analysis_map(analysis_result),
		new_analysis_results :: analysis_map(analysis_result),

			% The Inter-module Dependency Graph records
			% dependencies of an entire module's analysis results
			% on another module's answer patterns. e.g. assume
			% module M1 contains function F1 that has an analysis
			% result that used the answer F2:CP2->AP2 from module
			% M2.  If AP2 changes then all of M1 will either be
			% marked `suboptimal' or `invalid'.  Finer-grained
			% dependency tracking would allow only F1 to be
			% recompiled, instead of all of M1, but we don't do
			% that.
			%
			% IMDGs are loaded from disk into the old map.
			% During analysis any dependences of the current module
			% on other modules is added into the new map.
			% At the end of analysis all the arcs which terminate
			% at the current module are cleared from the old map
			% and replaced by those in the new map.
			%
			% XXX: check if we really need two maps
			%
		old_imdg :: analysis_map(imdg_arc),
		new_imdg :: analysis_map(imdg_arc)
	) => compiler(Compiler).

	% An analysis result is a call pattern paired with an answer.
	% The result has a status associated with it.
	%
:- type analysis_result
	--->	some [Call, Answer]
		analysis_result(Call, Answer, analysis_status)
		=> analysis(Call, Answer).

:- type analysis_request
	--->	some [Call]
		analysis_request(Call)
		=> call_pattern(Call).

:- type imdg_arc
	--->	some [Call]
		imdg_arc(
			Call,	    % Call pattern of the analysis result
				    % being depended on.
			module_id   % The module that _depends on_ this
				    % function's result.
		) => call_pattern(Call).

:- type analysis_map(T)		== map(module_id, module_analysis_map(T)).
:- type module_analysis_map(T)	== map(analysis_name, func_analysis_map(T)).
:- type func_analysis_map(T)	== map(func_id, list(T)).

%-----------------------------------------------------------------------------%
%
% The "any" call pattern
%

:- instance call_pattern(any_call) where [].
:- instance partial_order(any_call) where [
	more_precise_than(_, _) :- semidet_fail,
	equivalent(_, _) :- semidet_succeed
].
:- instance to_string(any_call) where [
	to_string(any_call) = "",
	from_string("") = any_call
].

%-----------------------------------------------------------------------------%

init_analysis_info(Compiler) =
    'new analysis_info'(Compiler, map__init, map__init, map__init, map__init,
	map__init, map__init).

%-----------------------------------------------------------------------------%

lookup_results(ModuleId, FuncId, ResultList, !Info, !IO) :-
    lookup_results(no, ModuleId, FuncId, ResultList, !Info, !IO).

:- pred lookup_results(bool::in, module_id::in, func_id::in,
	list({Call, Answer, analysis_status})::out, 
	analysis_info::in, analysis_info::out, io::di, io::uo) is det
	<= analysis(Call, Answer).

lookup_results(AllowInvalidModules, ModuleId, FuncId, ResultList,
	!Info, !IO) :-
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
	io.write_string("Looking up analysis results for ", !IO),
	io.write_string(ModuleId, !IO),
	io.write_string(".", !IO),
	io.write_string(FuncId, !IO),
	io.nl(!IO)
    ), !IO),
    ensure_old_module_analysis_results_loaded(ModuleId, !Info, !IO),
    (if
	AllowInvalidModules = no,
	!.Info ^ module_statuses ^ det_elem(ModuleId) = invalid
    then
	ResultList = []
    else
	lookup_results_2(!.Info ^ old_analysis_results,
	    ModuleId, FuncId, ResultList),
	debug_msg((pred(!.IO::di, !:IO::uo) is det :-
	    io.write_string("Found these results: ", !IO),
	    io.print(ResultList, !IO),
	    io.nl(!IO)
	), !IO)
    ).

:- pred lookup_results_2(analysis_map(analysis_result)::in, module_id::in,
	func_id::in, list({Call, Answer, analysis_status})::out) is det
	<= analysis(Call, Answer).

lookup_results_2(Map, ModuleId, FuncId, ResultList) :-
    AnalysisName = analysis_name(_ : Call, _ : Answer),
    (if
	ModuleResults = Map ^ elem(ModuleId),
	Results = ModuleResults ^ elem(AnalysisName) ^ elem(FuncId)
    then
	% XXX we might have to discard results which are
	% `invalid' or `fixpoint_invalid' if they are written at all
	ResultList = list.map(
	    (func(Result) = {Call, Answer, Status} :-
		Result = analysis_result(Call0, Answer0, Status),
		det_univ_to_type(univ(Call0), Call),
		det_univ_to_type(univ(Answer0), Answer)
	    ), Results)
    else
	ResultList = []
    ).

lookup_matching_results(ModuleId, FuncId, Call, ResultList, !Info, !IO) :-
    lookup_results(ModuleId, FuncId, AllResultsList, !Info, !IO),
    ResultList = list.filter(
	(pred(({ResultCall, _, _})::in) is semidet :-
	    ( more_precise_than(Call, ResultCall)
	    ; equivalent(Call, ResultCall)
	    )
	), AllResultsList).

lookup_best_result(ModuleId, FuncId, Call, MaybeBestResult, !Info, !IO) :-
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
	io.write_string("Looking up best analysis result for ", !IO),
	io.write_string(ModuleId, !IO),
	io.write_string(".", !IO),
	io.write_string(FuncId, !IO),
	io.nl(!IO)
    ), !IO),
    lookup_matching_results(ModuleId, FuncId, Call, MatchingResults,
	!Info, !IO),
    (
	MatchingResults = [],
	MaybeBestResult = no
    ;
	MatchingResults = [_ | _],
	MaybeBestResult = yes(BestResult),
	most_precise_answer(MatchingResults, BestResult)
    ).

:- pred most_precise_answer(
	list({Call, Answer, analysis_status})::in(non_empty_list),
	{Call, Answer, analysis_status}::out) is det
	<= analysis(Call, Answer).

most_precise_answer([Result | Results], BestResult) :-
    list.foldl(more_precise_answer, Results, Result, BestResult).

:- pred more_precise_answer({Call, Answer, analysis_status}::in,
	{Call, Answer, analysis_status}::in, 
	{Call, Answer, analysis_status}::out) is det
	<= analysis(Call, Answer).

more_precise_answer(Result, Best0, Best) :-
    Result = {_, ResultAnswer, _},
    Best0  = {_, BestAnswer0, _},
    (if more_precise_than(ResultAnswer, BestAnswer0) then
	Best = Result
    else
	Best = Best0
    ).

:- pred lookup_exactly_matching_result_even_from_invalid_modules(module_id::in,
	func_id::in, Call::in, maybe({Call, Answer, analysis_status})::out,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det
	<= analysis(Call, Answer).

lookup_exactly_matching_result_even_from_invalid_modules(ModuleId,
	FuncId, Call, MaybeResult, !Info, !IO) :-
    lookup_results(yes, ModuleId, FuncId, AllResultsList, !Info, !IO),
    ResultList = list.filter(
        (pred(({ResultCall, _, _})::in) is semidet :-
                equivalent(Call, ResultCall)
        ), AllResultsList),
    (
        ResultList = [],
        MaybeResult = no
    ;
        ResultList = [Result],
        MaybeResult = yes(Result)
    ;
        ResultList = [_, _ | _],
        error("lookup_exactly_matching_result: zero or one " ++
                "exactly matching results expected")
    ).

%-----------------------------------------------------------------------------%

record_result(ModuleId, FuncId, CallPattern, AnswerPattern, Status, !Info) :-
    Map0 = !.Info ^ new_analysis_results,
    record_result_in_analysis_map(ModuleId, FuncId,
	CallPattern, AnswerPattern, Status, Map0, Map),
    !:Info = !.Info ^ new_analysis_results := Map.

:- pred record_result_in_analysis_map(module_id::in, func_id::in,
	Call::in, Answer::in, analysis_status::in,
	analysis_map(analysis_result)::in, 
	analysis_map(analysis_result)::out) is det
	<= analysis(Call, Answer).

record_result_in_analysis_map(ModuleId, FuncId,
        CallPattern, AnswerPattern, Status, !Map) :-
    ( ModuleResults0 = map.search(!.Map, ModuleId) ->
	ModuleResults1 = ModuleResults0
    ;
	ModuleResults1 = map.init
    ),
    AnalysisName = analysis_name(CallPattern, AnswerPattern),
    ( AnalysisResults0 = map.search(ModuleResults1, AnalysisName) ->
	AnalysisResults1 = AnalysisResults0
    ;
	AnalysisResults1 = map.init
    ),
    ( FuncResults0 = map.search(AnalysisResults1, FuncId) ->
	FuncResults1 = FuncResults0
    ;
	FuncResults1 = []
    ),
    !:Map = map.set(!.Map, ModuleId,
	map.set(ModuleResults1, AnalysisName,
	map.set(AnalysisResults1, FuncId,
	FuncResults))),
    FuncResults = [Result | FuncResults1],
    Result = 'new analysis_result'(CallPattern, AnswerPattern, Status).

%-----------------------------------------------------------------------------%

lookup_requests(AnalysisName, ModuleId, FuncId, CallPatterns, !Info, !IO) :-
    ( map__search(!.Info ^ analysis_requests, ModuleId, ModuleRequests0) ->
        ModuleRequests = ModuleRequests0
    ;
        read_module_analysis_requests(!.Info, ModuleId, ModuleRequests, !IO),
        !:Info = !.Info ^ analysis_requests ^ elem(ModuleId) := ModuleRequests
    ),
    ( CallPatterns0 = ModuleRequests ^ elem(AnalysisName) ^ elem(FuncId) ->
        CallPatterns = list__filter_map(
            (func(Call0) = Call is semidet :- univ(Call) = univ(Call0)),
            CallPatterns0)
    ;
        CallPatterns = []
    ).

record_request(AnalysisName, ModuleId, FuncId, CallPattern, !Info) :-
    ( ModuleResults0 = map.search(!.Info ^ analysis_requests, ModuleId) ->
        ModuleResults1 = ModuleResults0
    ;
        ModuleResults1 = map.init
    ),
    ( AnalysisResults0 = map.search(ModuleResults1, AnalysisName) ->
        AnalysisResults1 = AnalysisResults0
    ;
        AnalysisResults1 = map.init
    ),
    ( FuncResults0 = map.search(AnalysisResults1, FuncId) ->
        FuncResults1 = FuncResults0
    ;
        FuncResults1 = []
    ),
    !:Info = !.Info ^ analysis_requests :=
        map.set(!.Info ^ analysis_requests, ModuleId,
        map.set(ModuleResults1, AnalysisName,
        map.set(AnalysisResults1, FuncId,
        ['new analysis_request'(CallPattern) | FuncResults1]))).

%-----------------------------------------------------------------------------%

record_dependency(CallerModuleId, AnalysisName, CalleeModuleId, FuncId, Call,
	!Info) :-
    (if CallerModuleId = CalleeModuleId then
	% XXX this assertion breaks compiling the standard library with
	% --analyse-trail-usage at the moment
	%
	% error("record_dependency: " ++ CalleeModuleId ++ " and " ++
	%    CallerModuleId ++ " must be different")
	true
    else
	( Analyses0 = map.search(!.Info ^ new_imdg, CalleeModuleId) ->
	    Analyses1 = Analyses0
	;
	    Analyses1 = map.init
	),
	( Funcs0 = map.search(Analyses1, AnalysisName) ->
	    Funcs1 = Funcs0
	;
	    Funcs1 = map.init
	),
	( FuncArcs0 = map.search(Funcs1, FuncId) ->
	    FuncArcs1 = FuncArcs0
	;
	    FuncArcs1 = []
	),
	Dep = 'new imdg_arc'(Call, CallerModuleId),
	% XXX this should really be a set to begin with
	( Dep `list.member` FuncArcs1 ->
	    true
	;
	    !:Info = !.Info ^ new_imdg :=
		map.set(!.Info ^ new_imdg, CalleeModuleId,
		map.set(Analyses1, AnalysisName,
		map.set(Funcs1, FuncId, FuncArcs))),
	    FuncArcs = [Dep | FuncArcs1]
	)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % The algorithm is from Nick's thesis, pp. 108-9.
    % Or my corruption thereof.
    % See the `analysis/README' file for a reference.
    % 
    % For each new analysis result (P^M:DP --> Ans_new):
    %   Read in the registry of M if necessary
    %   If there is an existing analysis result (P^M:DP --> Ans_old):
    %	if Ans_new \= Ans_old:
    %	    Replace the entry in the registry with P^M:DP --> Ans_new
    %	    if Ans_new `more_precise_than` Ans_old
    %		Status = suboptimal
    %	    else
    %		Status = invalid
    %	    For each entry (Q^N:DQ --> P^M:DP) in the IMDG:
    %		% Mark Q^N:DQ --> _ (_) with Status
    %		Actually, we don't do that.  We only mark the
    %		module N's _overall_ status with the 
    %		least upper bound of its old status and Status.
    %   Else (P:DP --> Ans_old) did not exist:
    %	Insert result (P:DP --> Ans_new) into the registry.
    %
    % Finally, clear out the "new" analysis results map.  When we write
    % out the analysis files we will do it from the "old" results map.
    %
:- pred update_analysis_registry(analysis_info::in, analysis_info::out,
	io::di, io::uo) is det.
:- pred update_analysis_registry_2(module_id::in,
	module_analysis_map(analysis_result)::in,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.
:- pred update_analysis_registry_3(module_id::in, analysis_name::in, 
	func_analysis_map(analysis_result)::in,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.
:- pred update_analysis_registry_4(module_id::in, analysis_name::in,
	func_id::in, list(analysis_result)::in, 
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.
:- pred update_analysis_registry_5(module_id::in, analysis_name::in,
	func_id::in, analysis_result::in, 
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry(!Info, !IO) :-
    debug_msg(io.print("Updating analysis registry.\n"), !IO),
    map.foldl2(update_analysis_registry_2, !.Info ^ new_analysis_results,
	!Info, !IO),
    !:Info = !.Info ^ new_analysis_results := map.init.

update_analysis_registry_2(ModuleId, ModuleMap, !Info, !IO) :-
    ensure_old_module_analysis_results_loaded(ModuleId, !Info, !IO),
    ensure_old_imdg_loaded(ModuleId, !Info, !IO),
    map.foldl2(update_analysis_registry_3(ModuleId), ModuleMap, !Info, !IO).

update_analysis_registry_3(ModuleId, AnalysisName, FuncMap, !Info, !IO) :-
    map.foldl2(update_analysis_registry_4(ModuleId, AnalysisName),
	FuncMap, !Info, !IO).

update_analysis_registry_4(ModuleId, AnalysisName, FuncId, NewResults,
	!Info, !IO) :-
    % XXX Currently we do not prevent there being more than one recorded result
    % for a given call pattern.
    list.foldl2(update_analysis_registry_5(ModuleId, AnalysisName, FuncId),
	NewResults, !Info, !IO).

update_analysis_registry_5(ModuleId, AnalysisName, FuncId, NewResult,
	!Info, !IO) :-
    NewResult = analysis_result(Call, NewAnswer, NewStatus),
    lookup_exactly_matching_result_even_from_invalid_modules(ModuleId, FuncId,
	Call, MaybeResult, !Info, !IO),
    (
	% There was a previous answer for this call pattern.
	%
	MaybeResult = yes({_OldCall, OldAnswer, OldStatus}),
	(if equivalent(NewAnswer, OldAnswer) then
	    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
		io.print("No change in the result ", !IO),
		io.print(ModuleId, !IO),
		io.print(".", !IO),
		io.print(FuncId, !IO),
		io.print(":", !IO),
		io.print(Call, !IO),
		io.print(" --> ", !IO),
		io.print(NewAnswer, !IO),
		io.nl(!IO)
	    ), !IO),

	    (if NewStatus \= OldStatus then
		OldMap0 = !.Info ^ old_analysis_results,
		replace_result_in_analysis_map(ModuleId, FuncId,
		    Call, NewAnswer, NewStatus, OldMap0, OldMap),
		!:Info = !.Info ^ old_analysis_results := OldMap
	    else
		true
	    )
	else
	    % Answer has changed.
	    % Replace the old answer in the registry with the new answer.
	    %
	    OldMap0 = !.Info ^ old_analysis_results,
	    replace_result_in_analysis_map(ModuleId, FuncId,
                Call, NewAnswer, NewStatus, OldMap0, OldMap),
	    !:Info = !.Info ^ old_analysis_results := OldMap,

	    % If the answer is more precise than before then dependent
	    % modules should be marked suboptimal.  Otherwise the answer
	    % is less precise than it was before, so dependent modules
	    % should be invalidated.
	    %
	    (if NewAnswer `more_precise_than` OldAnswer then
		Status = suboptimal
	    else
		Status = invalid
	    ),
	    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
		io.print(OldAnswer, !IO),
		io.print(" changed to ", !IO),
		io.print(NewAnswer, !IO),
		io.nl(!IO),
		io.print("Mark dependent modules as ", !IO),
		io.print(Status, !IO),
		io.nl(!IO),
		io.print("The modules to mark are: ", !IO),
		io.print(DepModules, !IO),
		io.nl(!IO)
	    ), !IO),
	    DepModules = imdg_dependent_modules(
		!.Info ^ old_imdg ^ det_elem(ModuleId), AnalysisName,
		FuncId, Call),
	    set.fold2(taint_module_overall_status(Status), DepModules,
		!Info, !IO)
	)
    ;
	% There was no previous answer for this call pattern.
	% Just add this result to the registry.
	%
	MaybeResult = no,
	OldMap0 = !.Info ^ old_analysis_results,
	record_result_in_analysis_map(ModuleId, FuncId,
	    Call, NewAnswer, NewStatus, OldMap0, OldMap),
	!:Info = !.Info ^ old_analysis_results := OldMap
    ).

    % replace_result_in_analysis_map(ModuleId, FuncId,
    %	Call, Answer, Status, !Map)
    % 
    % Replace an analysis result for the given function/call pattern
    % with a new result.  A previous result _must_ already exist
    % in the map with exactly the same call pattern.
    %
:- pred replace_result_in_analysis_map(module_id::in, func_id::in,
	Call::in, Answer::in, analysis_status::in,
	analysis_map(analysis_result)::in, 
	analysis_map(analysis_result)::out) is det
	<= analysis(Call, Answer).

replace_result_in_analysis_map(ModuleId, FuncId,
	CallPattern, AnswerPattern, Status, Map0, Map) :-
    AnalysisName = analysis_name(CallPattern, AnswerPattern),
    ModuleResults0 = map.lookup(Map0, ModuleId),
    AnalysisResults0 = map.lookup(ModuleResults0, AnalysisName),
    FuncResults0 = map.lookup(AnalysisResults0, FuncId),
    replace_result_in_list(CallPattern, AnswerPattern, Status,
	FuncResults0, FuncResults),
    Map = map.det_update(Map0, ModuleId,
	map.det_update(ModuleResults0, AnalysisName,
	map.det_update(AnalysisResults0, FuncId, FuncResults))).

:- pred replace_result_in_list(Call::in, Answer::in, analysis_status::in, 
	list(analysis_result)::in, list(analysis_result)::out)
	is det <= analysis(Call, Answer).

replace_result_in_list(_Call, _Answer, _Status, [], _) :-
    error("replace_result_in_list/5: found no result to replace").
replace_result_in_list(Call, Answer, Status, [H0 | T0], [H | T]) :-
    H0 = analysis_result(HCall0, _, _),
    det_univ_to_type(univ(HCall0), HCall),
    (if equivalent(Call, HCall) then
	H = 'new analysis_result'(Call, Answer, Status),
	T = T0
    else
	H = H0,
	replace_result_in_list(Call, Answer, Status, T0, T)
    ).

:- func imdg_dependent_modules(module_analysis_map(imdg_arc), analysis_name,
	func_id, Call) = set(module_id) <= call_pattern(Call).

imdg_dependent_modules(ModuleMap, AnalysisName, FuncId, Call) =
    (if map.search(ModuleMap, AnalysisName, FuncAnalysisMap),
	map.search(FuncAnalysisMap, FuncId, IMDGEntries)
    then
	set.from_list(list.filter_map(arc_module_id(Call), IMDGEntries))
    else
	set.init
    ).

    % XXX: compiler aborts if the modes are removed
:- func arc_module_id(Call::in, imdg_arc::in) = (module_id::out) is semidet
    <= call_pattern(Call).

arc_module_id(CallA, imdg_arc(CallB0, ModuleId)) = ModuleId :-
    det_univ_to_type(univ(CallB0), CallB),
    equivalent(CallA, CallB).

:- pred taint_module_overall_status(analysis_status::in,
	module_id::in, analysis_info::in, analysis_info::out,
	io::di, io::uo) is det.

taint_module_overall_status(Status, ModuleId, !Info, !IO) :-
    (if Status = optimal then
	true
    else
	ensure_old_module_analysis_results_loaded(ModuleId, !Info, !IO),
	debug_msg((pred(!.IO::di, !:IO::uo) is det :-
	    io.print("Tainting the overall module status of ", !IO),
	    io.print(ModuleId, !IO),
	    io.print(" with ", !IO),
	    io.print(ModuleStatus, !IO),
	    io.nl(!IO)
	), !IO),
	ModuleStatus0 = !.Info ^ module_statuses ^ det_elem(ModuleId),
	ModuleStatus = lub(ModuleStatus0, Status),
	!:Info = !.Info ^ module_statuses ^ elem(ModuleId) := ModuleStatus
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % For each module N imported by M:
    %	Delete all entries leading to module M from N's IMDG:
    %	For each P^M:DP in S (call patterns to analyse):
    %	    add P^M:DP --> Q^N:DQ to N's IMDG
    %
:- pred update_intermodule_dependencies(module_id::in, set(module_id)::in,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_intermodule_dependencies(ModuleId, ImportedModules, !Info, !IO) :-
    set.fold2(update_intermodule_dependencies_2(ModuleId),
	ImportedModules, !Info, !IO).

:- pred update_intermodule_dependencies_2(module_id::in, module_id::in,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_intermodule_dependencies_2(ModuleId, ImportedModuleId, !Info, !IO) :-
    debug_msg((pred(!.IO::di, !:IO::uo) is det :-
	io.print("Clearing entries involving ", !IO),
	io.print(ModuleId, !IO),
	io.print(" from ", !IO),
	io.print(ImportedModuleId, !IO),
	io.print("'s IMDG.\n", !IO)
    ), !IO),
    ensure_old_imdg_loaded(ImportedModuleId, !Info, !IO),
    IMDG0 = !.Info ^ old_imdg ^ det_elem(ImportedModuleId),
    clear_imdg_entries_pointing_at(ModuleId, IMDG0, IMDG1),

    (if NewArcs = !.Info ^ new_imdg ^ elem(ImportedModuleId) then
	map.union(combine_func_imdg, IMDG1, NewArcs, IMDG)
    else
	IMDG = IMDG1
    ),
    !:Info = !.Info ^ old_imdg ^ elem(ImportedModuleId) := IMDG,
    !:Info = !.Info  ^ new_imdg :=
        map.delete(!.Info ^ new_imdg, ImportedModuleId).

:- pred clear_imdg_entries_pointing_at(module_id::in,
	module_analysis_map(imdg_arc)::in,
	module_analysis_map(imdg_arc)::out) is det.
:- pred clear_imdg_entries_pointing_at_2(module_id::in, analysis_name::in, 
	func_analysis_map(imdg_arc)::in,
	func_analysis_map(imdg_arc)::out) is det.
:- pred clear_imdg_entries_pointing_at_3(module_id::in, func_id::in,
	list(imdg_arc)::in, list(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at(ModuleId, Map0, Map) :-
    map.map_values(clear_imdg_entries_pointing_at_2(ModuleId), Map0, Map).
clear_imdg_entries_pointing_at_2(ModuleId, _, FuncMap0, FuncMap) :-
    map.map_values(clear_imdg_entries_pointing_at_3(ModuleId),
	FuncMap0, FuncMap).
clear_imdg_entries_pointing_at_3(ModuleId, _, Arcs0, Arcs) :-
    list.filter((pred(imdg_arc(_, ModId)::in) is semidet :- ModuleId \= ModId),
	Arcs0, Arcs).

:- pred combine_func_imdg(func_analysis_map(imdg_arc)::in,
	func_analysis_map(imdg_arc)::in, func_analysis_map(imdg_arc)::out)
	is det.

combine_func_imdg(FuncImdgA, FuncImdgB, FuncImdg) :-
    map.union(combine_imdg_lists, FuncImdgA, FuncImdgB, FuncImdg).

:- pred combine_imdg_lists(list(imdg_arc)::in, list(imdg_arc)::in,
	list(imdg_arc)::out) is det.

combine_imdg_lists(ArcsA, ArcsB, ArcsA ++ ArcsB).

%-----------------------------------------------------------------------------%

:- pred ensure_old_module_analysis_results_loaded(module_id::in,
	analysis_info::in, analysis_info::out, io::di, io::uo) is det.

ensure_old_module_analysis_results_loaded(ModuleId, !Info, !IO) :-
    (if map.search(!.Info ^ old_analysis_results, ModuleId, _Results) then
	% sanity check
	map.lookup(!.Info ^ module_statuses, ModuleId, _StatusMustExist)
    else
	read_module_analysis_results(!.Info, ModuleId,
	    ModuleStatus, ModuleResults, !IO),
	!:Info = (!.Info
		^ module_statuses ^ elem(ModuleId) := ModuleStatus)
		^ old_analysis_results ^ elem(ModuleId) := ModuleResults
    ).

:- pred ensure_old_imdg_loaded(module_id::in, analysis_info::in,
	analysis_info::out, io::di, io::uo) is det.

ensure_old_imdg_loaded(ModuleId, !Info, !IO) :-
    Map0 = !.Info ^ old_imdg,
    (if map.search(Map0, ModuleId, _) then
	% already loaded
	true
    else
	read_module_imdg(!.Info, ModuleId, IMDG, !IO),
	map.det_insert(Map0, ModuleId, IMDG, Map),
	!:Info = !.Info ^ old_imdg := Map
    ).

%-----------------------------------------------------------------------------%

    % In this procedure we have just finished compiling module ModuleId
    % and will write out data currently cached in the analysis_info
    % structure out to disk.
    % 
write_analysis_files(Compiler, ModuleId, ImportedModuleIds, !Info, !IO) :-
    % The current module was just compiled so we set its status to the
    % lub of all the new analysis results generated.
    (if NewResults = !.Info ^ new_analysis_results ^ elem(ModuleId) then
	ModuleStatus = lub_result_statuses(NewResults)
    else
	ModuleStatus = optimal,
	% Force an `.analysis' file to be written out for this module,
	% even though there are no results recorded for it.
	!:Info = !.Info ^ new_analysis_results ^ elem(ModuleId) := map.init
    ),

    update_analysis_registry(!Info, !IO),

    !:Info = !.Info ^ module_statuses ^ elem(ModuleId) := ModuleStatus,

    update_intermodule_dependencies(ModuleId, ImportedModuleIds,
	!Info, !IO),
    (if map.is_empty(!.Info ^ new_analysis_results) then
	true
    else
	io.print("Warning: new_analysis_results is not empty\n", !IO),
	io.print(!.Info ^ new_analysis_results, !IO),
	io.nl(!IO)
    ),

    % Write the results for all the modules we know of.  For the
    % module being compiled, the analysis results may have changed.
    % For other modules, their overall statuses may have changed.
    write_local_modules(!.Info, write_module_analysis_results,
	!.Info ^ old_analysis_results, !IO),

    % Write the requests for the imported modules.
    write_local_modules(!.Info, write_module_analysis_requests,
	!.Info ^ analysis_requests, !IO),

    % Remove the requests for the current module since we (should have)
    % fulfilled them in this pass.
    empty_request_file(!.Info, ModuleId, !IO),

    % Write the intermodule dependency graphs.
    write_local_modules(!.Info, write_module_imdg,
	!.Info ^ old_imdg, !IO),
    
    % Touch a timestamp file to indicate the last time that this module was
    % analysed.
    module_id_to_write_file_name(Compiler, ModuleId, ".analysis_date",
	TimestampFileName, !IO),
    io.open_output(TimestampFileName, Result, !IO),
    (
        Result = ok(OutputStream),
        io.write_string(OutputStream, "\n", !IO),
        io.close_output(OutputStream, !IO)
    ;
        Result = error(IOError),
	error(io.error_message(IOError))
    ).

:- type write_module_analysis_map(T) ==
    (pred(analysis_info, module_id, module_analysis_map(T), io, io)).
:- mode write_module_analysis_map == in(pred(in, in, in, di, uo) is det).

:- pred write_local_modules(analysis_info::in,
    write_module_analysis_map(T)::write_module_analysis_map,
    analysis_map(T)::in, io::di, io::uo) is det.
:- pred write_local_modules_2(analysis_info::in,
    write_module_analysis_map(T)::write_module_analysis_map,
    module_id::in, module_analysis_map(T)::in, io::di, io::uo) is det.

write_local_modules(Info, Write, AnalysisMap, !IO) :-
    map.foldl(write_local_modules_2(Info, Write), AnalysisMap, !IO).

write_local_modules_2(Info, Write, ModuleId, ModuleResults, !IO) :-
    module_is_local(Info ^ compiler, ModuleId, IsLocal, !IO),
    (
	IsLocal = yes,
	Write(Info, ModuleId, ModuleResults, !IO)
    ;
	IsLocal = no,
	debug_msg((pred(!.IO::di, !:IO::uo) is det :-
	    io.write_string("Not writing file for non-local module ", !IO),
	    io.write_string(ModuleId, !IO),
	    io.nl(!IO)
	), !IO)
    ).

:- pred write_module_analysis_results(analysis_info::in, module_id::in,
	module_analysis_map(analysis_result)::in, io::di, io::uo) is det.

write_module_analysis_results(Info, ModuleId, ModuleResults, !IO) :-
    ModuleStatus = Info ^ module_statuses ^ det_elem(ModuleId),
    write_module_analysis_results(Info, ModuleId,
	ModuleStatus, ModuleResults, !IO).

%-----------------------------------------------------------------------------%

read_module_overall_status(Compiler, ModuleId, MaybeModuleStatus, !IO) :-
    analysis.file.read_module_overall_status(Compiler, ModuleId,
	MaybeModuleStatus, !IO).

%-----------------------------------------------------------------------------%

lub(StatusA, StatusB) = Status :-
    compare(Cmp, StatusA, StatusB),
    (
	Cmp = (=),
	Status = StatusA
    ;
	Cmp = (<),
	Status = StatusA
    ;
	Cmp = (>),
	Status = StatusB
    ).

:- func lub_result_statuses(module_analysis_map(analysis_result))
	= analysis_status.
:- func lub_result_statuses_2(analysis_name,
	func_analysis_map(analysis_result), analysis_status) = analysis_status.
:- func lub_result_statuses_3(func_id, list(analysis_result), analysis_status)
	= analysis_status.
:- func lub_result_statuses_4(analysis_result, analysis_status)
	= analysis_status.

lub_result_statuses(ModuleMap) =
    map.foldl(lub_result_statuses_2, ModuleMap, optimal).
lub_result_statuses_2(_AnalysisName, FuncMap, Acc) =
    map.foldl(lub_result_statuses_3, FuncMap, Acc).
lub_result_statuses_3(_FuncId, Results, Acc) =
    list.foldl(lub_result_statuses_4, Results, Acc).
lub_result_statuses_4(analysis_result(_, _, Status), Acc) =
    lub(Status, Acc).

%-----------------------------------------------------------------------------%

:- mutable(debug_analysis, bool, no, ground, [untrailed, attach_to_io_state]).

enable_debug_messages(Debug, !IO) :-
    set_debug_analysis(Debug, !IO).

:- pred debug_msg(pred(io, io)::in(pred(di, uo) is det), io::di, io::uo)
    is det.

debug_msg(P, !IO) :-
    get_debug_analysis(Debug, !IO),
    (
	Debug = yes,
	P(!IO)
    ;
	Debug = no
    ).
