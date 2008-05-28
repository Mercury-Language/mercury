%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004, 2006-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: analysis.m.
% Main authors: stayl, wangp.
%
% An inter-module analysis framework, as described in
%
%   Nicholas Nethercote. The Analysis Framework of HAL,
%   Chapter 7: Inter-module Analysis, Master's Thesis,
%   University of Melbourne, September 2001, revised April 2002.
%   <http://www.cl.cam.ac.uk/~njn25/pubs/masters2001.ps.gz>.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module unit.

%-----------------------------------------------------------------------------%

    % The intention is that eventually any compiler can use this library
    % via .NET by defining an instance of this type class.
:- typeclass compiler(Compiler) where [
    func compiler_name(Compiler) = string,

    % Describe the analyses which can be performed by a compiler.
    %
    func analyses(Compiler, analysis_name) = analysis_type is semidet,

    % module_name_to_read_file_name(Compiler, ModuleName, Ext, FileName)
    %
    pred module_name_to_read_file_name(Compiler::in, module_name::in,
        string::in, maybe_error(string)::out, io::di, io::uo) is det,

    % module_name_to_write_file_name(Compiler, ModuleName, Ext, FileName)
    %
    pred module_name_to_write_file_name(Compiler::in, module_name::in,
        string::in, string::out, io::di, io::uo) is det
].

:- type analysis_name == string.

:- type analysis_type
    --->    some [FuncInfo, Call, Answer]
            analysis_type(
                unit(Call),
                unit(Answer)
            ) => analysis(FuncInfo, Call, Answer).

    % An analysis is defined by a type describing call patterns and
    % a type defining answer patterns.  If the analysis needs to store
    % more information about the function being analysed (e.g. arity)
    % it should be stored as part of the type for call patterns.
    %
:- typeclass analysis(FuncInfo, Call, Answer)
    <= (call_pattern(FuncInfo, Call),
        answer_pattern(FuncInfo, Answer))
    where
[
    func analysis_name(Call::unused, Answer::unused) =
        (analysis_name::out) is det,

    % The version number should be changed when the Call or Answer
    % types are changed so that results which use the old types
    % can be discarded.
    %
    func analysis_version_number(Call::unused, Answer::unused) =
        (int::out) is det,

    func preferred_fixpoint_type(Call::unused, Answer::unused) =
        (fixpoint_type::out) is det,

    func bottom(FuncInfo::in, Call::unused) = (Answer::out) is det,
    func top(FuncInfo::in, Call::unused) = (Answer::out) is det,

    pred get_func_info(module_info::in, module_name::in, func_id::in,
        Call::unused, Answer::unused, FuncInfo::out) is det
].

:- type fixpoint_type
    --->    least_fixpoint
            % Start at `bottom'.
            % Must run to completion.

    ;       greatest_fixpoint.
            % Start at `top'.
            % Can stop at any time.

:- typeclass call_pattern(FuncInfo, Call)
    <= (partial_order(FuncInfo, Call),
        to_string(Call))
    where [].

:- typeclass answer_pattern(FuncInfo, Answer)
    <= (partial_order(FuncInfo, Answer),
        to_string(Answer))
    where [].

:- type analysis_result(Call, Answer)
    --->    analysis_result(
                ar_call     :: Call,
                ar_answer   :: Answer,
                ar_status   :: analysis_status
            ).

:- typeclass partial_order(FuncInfo, T)
    <= (T -> FuncInfo)
    where
[
    pred more_precise_than(FuncInfo::in, T::in, T::in) is semidet,
    pred equivalent(FuncInfo::in, T::in, T::in) is semidet
].

:- typeclass to_string(S) where [
    func to_string(S) = string,
    func from_string(string) = S is semidet
].

:- type no_func_info
    --->    no_func_info.

    % A call pattern that can be used by analyses that do not need
    % finer granularity.
    %
:- type any_call
    --->    any_call.

:- instance call_pattern(no_func_info, any_call).
:- instance partial_order(no_func_info, any_call).
:- instance to_string(any_call).

    % The status of a module or a specific analysis result.
    %
:- type analysis_status
    --->    invalid
    ;       suboptimal
    ;       optimal.

    % Least upper bound of two analysis_status values.
    %
:- func lub(analysis_status, analysis_status) = analysis_status.

    % This will need to encode language specific details like whether
    % it is a predicate or a function, and the arity and mode number.
:- type func_id
    --->    func_id(
                fid_pf      :: pred_or_func,
                fid_name    :: string,
                fid_arity   :: int,
                fid_mode    :: proc_id
            ).

    % Holds information used while analysing a module.
:- type analysis_info.

:- func init_analysis_info(Compiler) = analysis_info <= compiler(Compiler).

%-----------------------------------------------------------------------------%

    % Look up all results for a given function.
    %
    % N.B. Newly recorded results will NOT be found. This is intended
    % for looking up results from _other_ modules.
    %
:- pred lookup_results(analysis_info::in, module_name::in, func_id::in,
    list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Look up all results for a given function and call pattern CP such
    % that the results have call patterns CP' that are equivalent to CP
    % or less specific than CP.
    %
    % N.B. Newly recorded results will NOT be found. This is intended
    % for looking up results from _other_ modules.
    %
:- pred lookup_matching_results(analysis_info::in, module_name::in,
    func_id::in, FuncInfo::in, Call::in,
    list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Look up the best result matching a given call.
    %
    % N.B. Newly recorded results will NOT be found. This is intended
    % for looking up results from _other_ modules.
    %
    % If the returned best result has a call pattern that is different
    % from the given call pattern, then it is the analysis writer's
    % responsibility to request a more precise analysis from the called module,
    % using `record_request'.
    %
:- pred lookup_best_result(analysis_info::in, module_name::in, func_id::in,
    FuncInfo::in, Call::in, maybe(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Record an analysis result for a (usually local) function.
    %
    % XXX At the moment the result is assumed to be for a function local to
    % the currently-compiled module and things will probably break if it isn't.
    %
:- pred record_result(module_name::in, func_id::in, Call::in, Answer::in,
    analysis_status::in, analysis_info::in, analysis_info::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Record the dependency of a module on the analysis result of another
    % module.
    %
:- pred record_dependency(module_name::in, analysis_name::in, module_name::in,
    func_id::in, Call::in, analysis_info::in, analysis_info::out) is det
    <= call_pattern(FuncInfo, Call).

    % Lookup all the requests for a given (usually local) function.
    %
:- pred lookup_requests(analysis_info::in, analysis_name::in, module_name::in,
    func_id::in, list(Call)::out) is det
    <= call_pattern(FuncInfo, Call).

    % Record a request for a function in an imported module.
    %
:- pred record_request(analysis_name::in, module_name::in, func_id::in,
    Call::in, analysis_info::in, analysis_info::out) is det
    <= call_pattern(FuncInfo, Call).

%-----------------------------------------------------------------------------%

    % prepare_intermodule_analysis(ThisModuleName, ModuleNames,
    %   LocalModuleNames, !Info, !IO)
    %
    % This predicate should be called before any pass begins to use the
    % analysis framework.  It ensures that all the analysis files 
    % are loaded so that lookups can be satisfied.  ModuleNames is the set of
    % all modules that are directly or indirectly imported by the module being
    % analysed.  LocalModuleNames is the set of non-"library" modules.
    %
:- pred prepare_intermodule_analysis(module_name::in, set(module_name)::in,
    set(module_name)::in, analysis_info::in, analysis_info::out,
    io::di, io::uo) is det.

     % module_is_local(Info, ModuleName, IsLocal).
     %
     % IsLocal is `yes' if the module is not a "library" module, i.e. we are
     % able to reanalyse the module. The set of local modules is set in
     % `prepare_intermodule_analysis'.
    %
:- pred module_is_local(analysis_info::in, module_name::in, bool::out)
    is det.

    % Should be called after all analysis is completed to write the
    % requests and results for the current compilation to the
    % analysis files.
    %
:- pred write_analysis_files(Compiler::in, module_info::in, module_name::in,
    set(module_name)::in, analysis_info::in, analysis_info::out,
    io::di, io::uo) is det
    <= compiler(Compiler).

%-----------------------------------------------------------------------------%

    % read_module_overall_status(Compiler, ModuleName, MaybeModuleStatus, !IO)
    %
    % Attempt to read the overall status from a module `.analysis' file.
    %
:- pred read_module_overall_status(Compiler::in, module_name::in,
    maybe(analysis_status)::out, io::di, io::uo) is det
    <= compiler(Compiler).

:- pred enable_debug_messages(bool::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module analysis.file.

:- import_module analysis.file.
:- import_module libs.
:- import_module libs.compiler_util.

:- import_module map.
:- import_module string.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type analysis_info
    --->    some [Compiler]
            analysis_info(
                compiler :: Compiler,

                % The set of local modules, i.e. for which we can issue
                % requests.
                %
                local_module_names :: set(module_name),

                % Holds outstanding requests for more specialised variants
                % of procedures. Requests are added to this map as analyses
                % proceed and written out to disk at the end of the
                % compilation of this module.
                %
                analysis_requests :: analysis_map(analysis_request),

                % The overall status of each module.
                %
                module_statuses :: map(module_name, analysis_status),

                % The "old" map stores analysis results read in from disk.
                % New results generated while analysing the current module
                % are added to the "new" map. After all the analyses
                % the two maps are compared to see which analysis results
                % have changed. Other modules may need to be marked or
                % invalidated as a result. Then "new" results are moved
                % into the "old" map, from where they can be written to disk.
                %
                old_analysis_results :: analysis_map(some_analysis_result),
                new_analysis_results :: analysis_map(some_analysis_result),

                % The Inter-module Dependency Graph records dependencies
                % of an entire module's analysis results on another module's
                % answer patterns. e.g. assume module M1 contains function F1
                % that has an analysis result that used the answer F2:CP2->AP2
                % from module M2. If AP2 changes then all of M1 will either be
                % marked `suboptimal' or `invalid'. Finer-grained dependency
                % tracking would allow only F1 to be recompiled, instead of
                % all of M1, but we don't do that.
                %
                % IMDGs are loaded from disk into the old map. During analysis
                % any dependences of the current module on other modules
                % is added into the new map. At the end of analysis all the
                % arcs which terminate at the current module are cleared
                % from the old map and replaced by those in the new map.
                %
                % XXX: Check if we really need two maps.
                %
                old_imdg :: analysis_map(imdg_arc),
                new_imdg :: analysis_map(imdg_arc)
            )
            => compiler(Compiler).

    % An analysis result is a call pattern paired with an answer.
    % The result has a status associated with it.
    %
:- type some_analysis_result
    --->    some [FuncInfo, Call, Answer]
            some_analysis_result(
                some_ar_call    :: Call,
                some_ar_answer  :: Answer,
                some_ar_status  :: analysis_status
            )
            => analysis(FuncInfo, Call, Answer).

:- type analysis_request
    --->    some [FuncInfo, Call]
            analysis_request(
                Call
            )
            => call_pattern(FuncInfo, Call).

:- type imdg_arc
    --->    some [FuncInfo, Call]
            imdg_arc(
                Call,       % Call pattern of the analysis result
                            % being depended on.
                module_name   % The module that _depends on_ this function's
                            % result.
            )
            => call_pattern(FuncInfo, Call).

:- type analysis_map(T)         == map(module_name, module_analysis_map(T)).
:- type module_analysis_map(T)  == map(analysis_name, func_analysis_map(T)).
:- type func_analysis_map(T)    == map(func_id, list(T)).

%-----------------------------------------------------------------------------%
%
% The "any" call pattern
%

:- instance call_pattern(no_func_info, any_call) where [].
:- instance partial_order(no_func_info, any_call) where [
    ( more_precise_than(_, _, _) :-
        semidet_fail
    ),
    ( equivalent(no_func_info, _, _) :-
        semidet_succeed
    )
].
:- instance to_string(any_call) where [
    to_string(any_call) = "",
    from_string("") = any_call
].

%-----------------------------------------------------------------------------%

init_analysis_info(Compiler) =
    'new analysis_info'(Compiler, set.init, map.init, map.init, map.init,
        map.init, map.init, map.init).

%-----------------------------------------------------------------------------%

lookup_results(Info, ModuleName, FuncId, ResultList) :-
    lookup_results(Info, ModuleName, FuncId, no, ResultList).

:- pred lookup_results(analysis_info::in, module_name::in, func_id::in,
    bool::in, list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

lookup_results(Info, ModuleName, FuncId, AllowInvalidModules, ResultList) :-
    trace [io(!IO)] (
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Looking up analysis results for ", !IO),
            io.write(ModuleName, !IO),
            io.write_string(".", !IO),
            io.write(FuncId, !IO),
            io.nl(!IO)
        ), !IO)
    ),
    (
        AllowInvalidModules = no,
        Info ^ module_statuses ^ elem(ModuleName) = invalid
    ->
        ResultList = []
    ;
        lookup_results_2(Info ^ old_analysis_results, ModuleName, FuncId,
            ResultList),
        trace [io(!IO)] (
            debug_msg((pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("% Found these results: ", !IO),
                io.print(ResultList, !IO),
                io.nl(!IO)
            ), !IO)
        )
    ).

:- pred lookup_results_2(analysis_map(some_analysis_result)::in, module_name::in,
    func_id::in, list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

lookup_results_2(Map, ModuleName, FuncId, ResultList) :-
    AnalysisName = analysis_name(_ : Call, _ : Answer),
    (
        ModuleResults = Map ^ elem(ModuleName),
        Results = ModuleResults ^ elem(AnalysisName) ^ elem(FuncId)
    ->
        % XXX we might have to discard results which are
        % `invalid' or `fixpoint_invalid' if they are written at all
        ResultList = list.map(
            (func(Result) = analysis_result(Call, Answer, Status) :-
                Result = some_analysis_result(Call0, Answer0, Status),
                det_univ_to_type(univ(Call0), Call),
                det_univ_to_type(univ(Answer0), Answer)
            ), Results)
    ;
        ResultList = []
    ).

lookup_matching_results(Info, ModuleName, FuncId, FuncInfo, Call, ResultList) :-
    lookup_results(Info, ModuleName, FuncId, AllResultsList),
    ResultList = list.filter(
        (pred(Result::in) is semidet :-
            ResultCall = Result ^ ar_call,
            ( more_precise_than(FuncInfo, Call, ResultCall)
            ; equivalent(FuncInfo, Call, ResultCall)
            )
        ), AllResultsList).

lookup_best_result(Info, ModuleName, FuncId, FuncInfo, Call, MaybeBestResult) :-
    trace [io(!IO)] (
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Looking up best analysis result for ", !IO),
            io.write(ModuleName, !IO),
            io.write_string(".", !IO),
            io.write(FuncId, !IO),
            io.nl(!IO)
        ), !IO)
    ),
    lookup_matching_results(Info, ModuleName, FuncId, FuncInfo, Call,
        MatchingResults),
    (
        MatchingResults = [],
        MaybeBestResult = no
    ;
        MatchingResults = [Result | Results],
        list.foldl(more_precise_answer(FuncInfo), Results, Result, BestResult),
        MaybeBestResult = yes(BestResult)
    ).

:- pred more_precise_answer(FuncInfo::in,
    analysis_result(Call, Answer)::in, analysis_result(Call, Answer)::in,
    analysis_result(Call, Answer)::out) is det
    <= analysis(FuncInfo, Call, Answer).

more_precise_answer(FuncInfo, Result, Best0, Best) :-
    ResultAnswer = Result ^ ar_answer,
    BestAnswer0 = Best0 ^ ar_answer,
    ( more_precise_than(FuncInfo, ResultAnswer, BestAnswer0) ->
        Best = Result
    ; 
        Best = Best0
    ).

:- pred lookup_exactly_matching_result_even_from_invalid_modules(
    analysis_info::in, module_name::in, func_id::in, FuncInfo::in, Call::in,
    maybe(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

lookup_exactly_matching_result_even_from_invalid_modules(Info, ModuleName,
        FuncId, FuncInfo, Call, MaybeResult) :-
    lookup_results(Info, ModuleName, FuncId, yes, AllResultsList),
    ResultList = list.filter(
        (pred(R::in) is semidet :-
            equivalent(FuncInfo, Call, R ^ ar_call)
        ), AllResultsList),
    (
        ResultList = [],
        MaybeResult = no
    ;
        ResultList = [Result],
        MaybeResult = yes(Result)
    ;
        ResultList = [_, _ | _],
        unexpected(this_file,
            "lookup_exactly_matching_result: " ++
            "zero or one exactly matching results expected")
    ).

%-----------------------------------------------------------------------------%

record_result(ModuleName, FuncId, CallPattern, AnswerPattern, Status, !Info) :-
    Map0 = !.Info ^ new_analysis_results,
    record_result_in_analysis_map(ModuleName, FuncId,
    CallPattern, AnswerPattern, Status, Map0, Map),
    !Info ^ new_analysis_results := Map.

:- pred record_result_in_analysis_map(module_name::in, func_id::in,
    Call::in, Answer::in, analysis_status::in,
    analysis_map(some_analysis_result)::in,
    analysis_map(some_analysis_result)::out) is det
    <= analysis(FuncInfo, Call, Answer).

record_result_in_analysis_map(ModuleName, FuncId,
        CallPattern, AnswerPattern, Status, !Map) :-
    ( ModuleResults0 = map.search(!.Map, ModuleName) ->
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
    !:Map = map.set(!.Map, ModuleName,
        map.set(ModuleResults1, AnalysisName,
            map.set(AnalysisResults1, FuncId, FuncResults))),
    FuncResults = [Result | FuncResults1],
    Result = 'new some_analysis_result'(CallPattern, AnswerPattern, Status).

%-----------------------------------------------------------------------------%

lookup_requests(Info, AnalysisName, ModuleName, FuncId, CallPatterns) :-
    (
        map.search(Info ^ analysis_requests, ModuleName, ModuleRequests),
        CallPatterns0 = ModuleRequests ^ elem(AnalysisName) ^ elem(FuncId)
    ->
        CallPatterns1 = list.filter_map(
            (func(analysis_request(Call0)) = Call is semidet :-
                univ(Call) = univ(Call0)
            ), CallPatterns0),
        % Requests simply get appended to `.request' files so when we read them
        % back in there may be duplicates.
        list.sort_and_remove_dups(CallPatterns1, CallPatterns)
    ;
        CallPatterns = []
    ).

record_request(AnalysisName, ModuleName, FuncId, CallPattern, !Info) :-
    ( ModuleResults0 = map.search(!.Info ^ analysis_requests, ModuleName) ->
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
    !Info ^ analysis_requests :=
        map.set(!.Info ^ analysis_requests, ModuleName,
            map.set(ModuleResults1, AnalysisName,
                map.set(AnalysisResults1, FuncId,
                    ['new analysis_request'(CallPattern) | FuncResults1]))).

%-----------------------------------------------------------------------------%

record_dependency(CallerModuleName, AnalysisName, CalleeModuleName, FuncId, Call,
        !Info) :-
    ( CallerModuleName = CalleeModuleName ->
        % XXX this assertion breaks compiling the standard library with
        % --analyse-trail-usage at the moment
        %
        % error("record_dependency: " ++ CalleeModuleName ++ " and " ++
        %    CallerModuleName ++ " must be different")
        true
    ;
        ( Analyses0 = map.search(!.Info ^ new_imdg, CalleeModuleName) ->
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
        Dep = 'new imdg_arc'(Call, CallerModuleName),
        % XXX this should really be a set to begin with
        ( list.member(Dep, FuncArcs1) ->
            true
        ;
            !Info ^ new_imdg :=
                map.set(!.Info ^ new_imdg, CalleeModuleName,
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
    %   if Ans_new \= Ans_old:
    %       Replace the entry in the registry with P^M:DP --> Ans_new
    %       if Ans_new `more_precise_than` Ans_old
    %       Status = suboptimal
    %       else
    %       Status = invalid
    %       For each entry (Q^N:DQ --> P^M:DP) in the IMDG:
    %       % Mark Q^N:DQ --> _ (_) with Status
    %       Actually, we don't do that.  We only mark the
    %       module N's _overall_ status with the
    %       least upper bound of its old status and Status.
    %   Else (P:DP --> Ans_old) did not exist:
    %   Insert result (P:DP --> Ans_new) into the registry.
    %
    % Finally, clear out the "new" analysis results map.  When we write
    % out the analysis files we will do it from the "old" results map.
    %
:- pred update_analysis_registry(module_info::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry(ModuleInfo, !Info, !IO) :-
    debug_msg(io.write_string("% Updating analysis registry.\n"), !IO),
    NewResults = !.Info ^ new_analysis_results,
    map.foldl2(update_analysis_registry_2(ModuleInfo), NewResults, !Info, !IO),
    !Info ^ new_analysis_results := map.init.

:- pred update_analysis_registry_2(module_info::in, module_name::in,
    module_analysis_map(some_analysis_result)::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_2(ModuleInfo, ModuleName, ModuleMap, !Info, !IO) :-
    map.foldl2(update_analysis_registry_3(ModuleInfo, ModuleName), ModuleMap,
        !Info, !IO).

:- pred update_analysis_registry_3(module_info::in, module_name::in,
    analysis_name::in, func_analysis_map(some_analysis_result)::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_3(ModuleInfo, ModuleName, AnalysisName, FuncMap,
        !Info, !IO) :-
    map.foldl2(update_analysis_registry_4(ModuleInfo, ModuleName, AnalysisName),
        FuncMap, !Info, !IO).

:- pred update_analysis_registry_4(module_info::in, module_name::in,
    analysis_name::in, func_id::in, list(some_analysis_result)::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_4(ModuleInfo, ModuleName, AnalysisName, FuncId,
        NewResults, !Info, !IO) :-
    % XXX Currently we do not prevent there being more than one recorded result
    % for a given call pattern.
    list.foldl2(update_analysis_registry_5(ModuleInfo, ModuleName, AnalysisName,
        FuncId), NewResults, !Info, !IO).

:- pred update_analysis_registry_5(module_info::in, module_name::in,
    analysis_name::in, func_id::in, some_analysis_result::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_5(ModuleInfo, ModuleName, AnalysisName, FuncId,
        NewResult, !Info, !IO) :-
    NewResult = some_analysis_result(Call, NewAnswer, NewStatus),
    get_func_info(ModuleInfo, ModuleName, FuncId, Call, NewAnswer, FuncInfo),
    lookup_exactly_matching_result_even_from_invalid_modules(!.Info,
        ModuleName, FuncId, FuncInfo, Call, MaybeResult),
    (
        % There was a previous answer for this call pattern.
        %
        MaybeResult = yes(OldResult),
        OldResult = analysis_result(_OldCall, OldAnswer, OldStatus),
        ( equivalent(FuncInfo, NewAnswer, OldAnswer) ->
            debug_msg((pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("% No change in the result ", !IO),
                io.write(ModuleName, !IO),
                io.write_string(".", !IO),
                io.write(FuncId, !IO),
                io.write_string(":", !IO),
                io.write(Call, !IO),
                io.write_string(" --> ", !IO),
                io.write(NewAnswer, !IO),
                io.nl(!IO)
            ), !IO),

            ( NewStatus \= OldStatus ->
                OldMap0 = !.Info ^ old_analysis_results,
                replace_result_in_analysis_map(ModuleName, FuncId, FuncInfo,
                    Call, NewAnswer, NewStatus, OldMap0, OldMap),
                !Info ^ old_analysis_results := OldMap
            ;
                true
            )
        ;
            % Answer has changed.
            % Replace the old answer in the registry with the new answer.
            OldMap0 = !.Info ^ old_analysis_results,
            replace_result_in_analysis_map(ModuleName, FuncId, FuncInfo,
                Call, NewAnswer, NewStatus, OldMap0, OldMap),
            !Info ^ old_analysis_results := OldMap,

            % If the answer is more precise than before then dependent modules
            % should be marked suboptimal. Otherwise the answer is less precise
            % than it was before, so dependent modules should be invalidated.
            ( more_precise_than(FuncInfo, NewAnswer, OldAnswer) ->
                Status = suboptimal
            ;
                Status = invalid
            ),
            debug_msg((pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("% ", !IO),
                io.write(OldAnswer, !IO),
                io.write_string(" changed to ", !IO),
                io.write(NewAnswer, !IO),
                io.nl(!IO),
                io.write_string("Mark dependent modules as ", !IO),
                io.write(Status, !IO),
                io.nl(!IO),
                io.write_string("The modules to mark are: ", !IO),
                io.write(DepModules, !IO),
                io.nl(!IO)
            ), !IO),
            OldArcs = !.Info ^ old_imdg ^ det_elem(ModuleName),
            DepModules = imdg_dependent_modules(OldArcs, AnalysisName,
                FuncId, FuncInfo, Call),
            set.fold2(taint_module_overall_status(Status), DepModules,
                !Info, !IO)
        )
    ;
        % There was no previous answer for this call pattern.
        % Just add this result to the registry.
        MaybeResult = no,
        OldMap0 = !.Info ^ old_analysis_results,
        record_result_in_analysis_map(ModuleName, FuncId,
            Call, NewAnswer, NewStatus, OldMap0, OldMap),
        !Info ^ old_analysis_results := OldMap
    ).

    % Replace an analysis result for the given function/call pattern with a
    % new result. A previous result _must_ already exist in the map with
    % exactly the same call pattern.
    %
:- pred replace_result_in_analysis_map(module_name::in, func_id::in,
    FuncInfo::in, Call::in, Answer::in, analysis_status::in,
    analysis_map(some_analysis_result)::in,
    analysis_map(some_analysis_result)::out) is det
    <= analysis(FuncInfo, Call, Answer).

replace_result_in_analysis_map(ModuleName, FuncId, FuncInfo,
        CallPattern, AnswerPattern, Status, Map0, Map) :-
    AnalysisName = analysis_name(CallPattern, AnswerPattern),
    ModuleResults0 = map.lookup(Map0, ModuleName),
    AnalysisResults0 = map.lookup(ModuleResults0, AnalysisName),
    FuncResults0 = map.lookup(AnalysisResults0, FuncId),
    replace_result_in_list(FuncInfo, CallPattern, AnswerPattern, Status,
    FuncResults0, FuncResults),
    Map = map.det_update(Map0, ModuleName,
    map.det_update(ModuleResults0, AnalysisName,
    map.det_update(AnalysisResults0, FuncId, FuncResults))).

:- pred replace_result_in_list(FuncInfo::in, Call::in, Answer::in,
    analysis_status::in,
    list(some_analysis_result)::in, list(some_analysis_result)::out) is det
    <= analysis(FuncInfo, Call, Answer).

replace_result_in_list(FuncInfo, Call, Answer, Status, Results0, Results) :-
    (
        Results0 = [],
        unexpected(this_file,
            "replace_result_in_list: found no result to replace")
    ;
        Results0 = [H0 | T0],
        det_univ_to_type(univ(H0 ^ some_ar_call), HCall),
        ( equivalent(FuncInfo, Call, HCall) ->
            H = 'new some_analysis_result'(Call, Answer, Status),
            T = T0
        ;
            H = H0,
            replace_result_in_list(FuncInfo, Call, Answer, Status, T0, T)
        ),
        Results = [H | T]
    ).

:- func imdg_dependent_modules(module_analysis_map(imdg_arc), analysis_name,
    func_id, FuncInfo, Call) = set(module_name)
    <= call_pattern(FuncInfo, Call).

imdg_dependent_modules(ModuleMap, AnalysisName, FuncId, FuncInfo, Call) =
    (
        map.search(ModuleMap, AnalysisName, FuncAnalysisMap),
        map.search(FuncAnalysisMap, FuncId, IMDGEntries)
    ->
        set.from_list(list.filter_map(arc_module_name(FuncInfo, Call),
            IMDGEntries))
    ;
        set.init
    ).

    % XXX: compiler aborts if the modes are removed
:- func arc_module_name(FuncInfo::in, Call::in, imdg_arc::in) =
    (module_name::out) is semidet
    <= call_pattern(FuncInfo, Call).

arc_module_name(FuncInfo, CallA, imdg_arc(CallB0, ModuleName)) = ModuleName :-
    det_univ_to_type(univ(CallB0), CallB),
    equivalent(FuncInfo, CallA, CallB).

:- pred taint_module_overall_status(analysis_status::in, module_name::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

taint_module_overall_status(Status, ModuleName, !Info, !IO) :-
    (
        Status = optimal
    ;
        ( Status = suboptimal
        ; Status = invalid
        ),

        % We may not have loaded the analysis results for this module yet.
        % Even though we loaded all the analysis files of modules reachable
        % from the initial module beforehand, a _caller_ of the initial module
        % may not be part of that set.
        ensure_old_module_analysis_results_loaded(ModuleName, !Info, !IO),

        ModuleStatus0 = !.Info ^ module_statuses ^ det_elem(ModuleName),
        ModuleStatus = lub(ModuleStatus0, Status),
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.print("% Tainting the overall module status of ", !IO),
            io.print(ModuleName, !IO),
            io.print(" with ", !IO),
            io.print(ModuleStatus, !IO),
            io.nl(!IO)
        ), !IO),
        !Info ^ module_statuses ^ elem(ModuleName) := ModuleStatus
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % For each module N imported by M:
    %   Delete all entries leading to module M from N's IMDG:
    %   For each P^M:DP in S (call patterns to analyse):
    %       add P^M:DP --> Q^N:DQ to N's IMDG
    %
:- pred update_intermodule_dependencies(module_name::in, set(module_name)::in,
    analysis_info::in, analysis_info::out) is det.

update_intermodule_dependencies(ModuleName, ImportedModules, !Info) :-
    set.fold(update_intermodule_dependencies_2(ModuleName), ImportedModules,
        !Info).

:- pred update_intermodule_dependencies_2(module_name::in, module_name::in,
    analysis_info::in, analysis_info::out) is det.

update_intermodule_dependencies_2(ModuleName, ImportedModuleName, !Info) :-
    trace [io(!IO)] (
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.print("% Clearing entries involving ", !IO),
            io.print(ModuleName, !IO),
            io.print(" from ", !IO),
            io.print(ImportedModuleName, !IO),
            io.print("'s IMDG.\n", !IO)
        ), !IO)
    ),
    IMDG0 = !.Info ^ old_imdg ^ det_elem(ImportedModuleName),
    clear_imdg_entries_pointing_at(ModuleName, IMDG0, IMDG1),

    ( NewArcs = !.Info ^ new_imdg ^ elem(ImportedModuleName) ->
        map.union(combine_func_imdg, IMDG1, NewArcs, IMDG)
    ;
        IMDG = IMDG1
    ),
    !Info ^ old_imdg ^ elem(ImportedModuleName) := IMDG,
    !Info ^ new_imdg := map.delete(!.Info ^ new_imdg, ImportedModuleName).

:- pred clear_imdg_entries_pointing_at(module_name::in,
    module_analysis_map(imdg_arc)::in,
    module_analysis_map(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at(ModuleName, Map0, Map) :-
    map.map_values(clear_imdg_entries_pointing_at_2(ModuleName), Map0, Map).

:- pred clear_imdg_entries_pointing_at_2(module_name::in, analysis_name::in,
    func_analysis_map(imdg_arc)::in,
    func_analysis_map(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at_2(ModuleName, _, FuncMap0, FuncMap) :-
    map.map_values(clear_imdg_entries_pointing_at_3(ModuleName),
        FuncMap0, FuncMap).

:- pred clear_imdg_entries_pointing_at_3(module_name::in, func_id::in,
    list(imdg_arc)::in, list(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at_3(ModuleName, _, Arcs0, Arcs) :-
    list.filter((pred(imdg_arc(_, ModId)::in) is semidet :- ModuleName \= ModId),
        Arcs0, Arcs).

:- pred combine_func_imdg(func_analysis_map(imdg_arc)::in,
    func_analysis_map(imdg_arc)::in, func_analysis_map(imdg_arc)::out) is det.

combine_func_imdg(FuncImdgA, FuncImdgB, FuncImdg) :-
    map.union(combine_imdg_lists, FuncImdgA, FuncImdgB, FuncImdg).

:- pred combine_imdg_lists(list(imdg_arc)::in, list(imdg_arc)::in,
    list(imdg_arc)::out) is det.

combine_imdg_lists(ArcsA, ArcsB, ArcsA ++ ArcsB).

%-----------------------------------------------------------------------------%

prepare_intermodule_analysis(ThisModuleName, ModuleNames, LocalModuleNames,
        !Info, !IO) :-
    set.fold2(ensure_analysis_files_loaded, ModuleNames, !Info, !IO),

    % Read in requests for the module being analysed.
    read_module_analysis_requests(!.Info, ThisModuleName, ThisModuleRequests,
        !IO),
    !Info ^ analysis_requests ^ elem(ThisModuleName) := ThisModuleRequests,

    !Info ^ local_module_names := LocalModuleNames.

:- pred ensure_analysis_files_loaded(module_name::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

ensure_analysis_files_loaded(ModuleName, !Info, !IO) :-
    ensure_old_module_analysis_results_loaded(ModuleName, !Info, !IO),
    ensure_old_imdg_loaded(ModuleName, !Info, !IO).

:- pred ensure_old_module_analysis_results_loaded(module_name::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

ensure_old_module_analysis_results_loaded(ModuleName, !Info, !IO) :-
    ( map.search(!.Info ^ old_analysis_results, ModuleName, _Results) ->
        % sanity check
        map.lookup(!.Info ^ module_statuses, ModuleName, _StatusMustExist)
    ;
        read_module_analysis_results(!.Info, ModuleName,
            ModuleStatus, ModuleResults, !IO),
        !Info ^ module_statuses ^ elem(ModuleName) := ModuleStatus,
        !Info ^ old_analysis_results ^ elem(ModuleName) := ModuleResults
    ).

:- pred ensure_old_imdg_loaded(module_name::in, analysis_info::in,
    analysis_info::out, io::di, io::uo) is det.

ensure_old_imdg_loaded(ModuleName, !Info, !IO) :-
    Map0 = !.Info ^ old_imdg,
    ( map.search(Map0, ModuleName, _) ->
        % already loaded
        true
    ;
        read_module_imdg(!.Info, ModuleName, IMDG, !IO),
        map.det_insert(Map0, ModuleName, IMDG, Map),
        !Info ^ old_imdg := Map
    ).

module_is_local(Info, ModuleName, IsLocal) :-
    ( set.contains(Info ^ local_module_names, ModuleName) ->
        IsLocal = yes
    ;
        IsLocal = no
    ).

%-----------------------------------------------------------------------------%

    % In this procedure we have just finished compiling module ModuleName
    % and will write out data currently cached in the analysis_info structure
    % out to disk.
    %
write_analysis_files(Compiler, ModuleInfo, ModuleName, ImportedModuleNames,
        !Info, !IO) :-
    % The current module was just compiled so we set its status to the
    % lub of all the new analysis results generated.
    ( NewResults = !.Info ^ new_analysis_results ^ elem(ModuleName) ->
        ModuleStatus = lub_result_statuses(NewResults)
    ; 
        ModuleStatus = optimal,
        % Force an `.analysis' file to be written out for this module,
        % even though there are no results recorded for it.
        !Info ^ new_analysis_results ^ elem(ModuleName) := map.init
    ),

    update_analysis_registry(ModuleInfo, !Info, !IO),

    !Info ^ module_statuses ^ elem(ModuleName) := ModuleStatus,

    update_intermodule_dependencies(ModuleName, ImportedModuleNames, !Info),
    ( map.is_empty(!.Info ^ new_analysis_results) ->
        true
    ;
        io.print("Warning: new_analysis_results is not empty\n",
            !IO),
        io.print(!.Info ^ new_analysis_results, !IO),
        io.nl(!IO)
    ),

    % Write the results for all the modules we know of.  For the module being
    % compiled, the analysis results may have changed. For other modules,
    % their overall statuses may have changed.
    write_local_modules(!.Info, write_module_analysis_results,
        !.Info ^ old_analysis_results, !IO),

    % Write the requests for the imported modules.
    write_local_modules(!.Info, write_module_analysis_requests,
        !.Info ^ analysis_requests, !IO),

    % Remove the requests for the current module since we (should have)
    % fulfilled them in this pass.
    empty_request_file(!.Info, ModuleName, !IO),

    % Write the intermodule dependency graphs.
    write_local_modules(!.Info, write_module_imdg, !.Info ^ old_imdg, !IO),

    % Touch a timestamp file to indicate the last time that this module was
    % analysed.
    module_name_to_write_file_name(Compiler, ModuleName, ".analysis_date",
        TimestampFileName, !IO),
    io.open_output(TimestampFileName, Result, !IO),
    (
        Result = ok(OutputStream),
        io.write_string(OutputStream, "\n", !IO),
        io.close_output(OutputStream, !IO)
    ;
        Result = error(IOError),
        unexpected(this_file,
            "write_analysis_files: " ++ io.error_message(IOError))
    ).

:- type write_module_analysis_map(T) ==
    (pred(analysis_info, module_name, module_analysis_map(T), io, io)).
:- mode write_module_analysis_map == in(pred(in, in, in, di, uo) is det).

:- pred write_local_modules(analysis_info::in,
    write_module_analysis_map(T)::write_module_analysis_map,
    analysis_map(T)::in, io::di, io::uo) is det.

write_local_modules(Info, Write, AnalysisMap, !IO) :-
    map.foldl(write_local_modules_2(Info, Write), AnalysisMap, !IO).

:- pred write_local_modules_2(analysis_info::in,
    write_module_analysis_map(T)::write_module_analysis_map,
    module_name::in, module_analysis_map(T)::in, io::di, io::uo) is det.

write_local_modules_2(Info, Write, ModuleName, ModuleResults, !IO) :-
    module_is_local(Info, ModuleName, IsLocal),
    (
        IsLocal = yes,
        Write(Info, ModuleName, ModuleResults, !IO)
    ;
        IsLocal = no,
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("% Not writing file for non-local module ", !IO),
            io.write(ModuleName, !IO),
            io.nl(!IO)
        ), !IO)
    ).

:- pred write_module_analysis_results(analysis_info::in, module_name::in,
    module_analysis_map(some_analysis_result)::in, io::di, io::uo) is det.

write_module_analysis_results(Info, ModuleName, ModuleResults, !IO) :-
    ModuleStatus = Info ^ module_statuses ^ det_elem(ModuleName),
    analysis.file.write_module_analysis_results(Info, ModuleName,
        ModuleStatus, ModuleResults, !IO).

%-----------------------------------------------------------------------------%

read_module_overall_status(Compiler, ModuleName, MaybeModuleStatus, !IO) :-
    analysis.file.read_module_overall_status(Compiler, ModuleName,
        MaybeModuleStatus, !IO).

%-----------------------------------------------------------------------------%
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

:- func lub_result_statuses(module_analysis_map(some_analysis_result))
    = analysis_status.

lub_result_statuses(ModuleMap) =
    map.foldl(lub_result_statuses_2, ModuleMap, optimal).

:- func lub_result_statuses_2(analysis_name,
    func_analysis_map(some_analysis_result), analysis_status) =
    analysis_status.

lub_result_statuses_2(_AnalysisName, FuncMap, Acc) =
    map.foldl(lub_result_statuses_3, FuncMap, Acc).

:- func lub_result_statuses_3(func_id, list(some_analysis_result),
    analysis_status) = analysis_status.

lub_result_statuses_3(_FuncId, Results, Acc) =
    list.foldl(lub_result_statuses_4, Results, Acc).

:- func lub_result_statuses_4(some_analysis_result, analysis_status)
    = analysis_status.

lub_result_statuses_4(Result, Acc) = lub(Result ^ some_ar_status, Acc).

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

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "analysis.m".

%-----------------------------------------------------------------------------%
:- end_module analysis.
%-----------------------------------------------------------------------------%
